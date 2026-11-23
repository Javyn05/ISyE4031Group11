# =============================================================================
# ISYE 4031 - Team 11 - Milestone 3
# Does a nonlinear model (GAM / random forest) beat linear regression at
# predicting O2 saturation in LATER years, and WHY / WHERE?
#
# One script, one shared evaluation setup, every model scored on the SAME rows.
#
# Run from the project folder:   Rscript m3_pipeline.R
# Needs: data/bottle.csv (required), data/cast.csv (optional, for Year/Month)
# Outputs: outputs/tables/*.csv, outputs/figures/*.png, outputs/experiment_log.md
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)   # fast CSV reading
  library(ranger)       # random forest (+ quantile regression forest)
  library(mgcv)         # GAM
  library(ggplot2)      # figures
})

# ----------------------------- SETTINGS --------------------------------------
SEED          <- 4031
TRAIN_END     <- 2008      # model fitting years: <= TRAIN_END
VALID_YEARS   <- 2009:2010 # tuning only (RF hyperparameters, broken-stick break)
TEST_START    <- 2011      # final test: 2011 onward. Never used for any choice.
QUAL_FILTER   <- TRUE      # drop values flagged 8 (suspect) or 9 (missing)
N_TREES       <- 300       # same as Milestone 2 RF
TUNE_SUBSAMPLE<- 60000     # rows used for RF tuning (speed); NULL = all
RUN_ROLLING   <- FALSE      # rolling-origin evaluation (train <= y-1, test y)
RUN_CTD_ERA   <- TRUE      # extra: train only on CTD-era data (1994+)
N_SEEDS       <- 2         # RF seed-variability check
THREADS       <- max(1, parallel::detectCores() - 1)

dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
LOG <- "outputs/experiment_log.md"
cat(sprintf("# Experiment log (auto-generated)\nRun started %s\n\n",
            format(Sys.time())), file = LOG)
logf <- function(...) {
  msg <- sprintf(...)
  cat(msg, "\n")
  cat(msg, "\n", file = LOG, append = TRUE)
}
set.seed(SEED)

# ----------------------------- PREDICTORS ------------------------------------
# Feature groups (used for the ablation experiment)
PHYS  <- c("Depthm", "T_degC", "Salnty")
NUTR  <- c("PO4uM", "SiO3uM", "NO2uM", "NO3uM")
BIO   <- c("ChlorA", "Phaeop")
PREDS <- c(PHYS, NUTR, BIO)                  # same 9 as the M2 linear model
TARGET <- "O2Sat"
# NEVER used as predictors (they are the outcome in other units, or computed
# from it): O2ml_L, Oxy_umol/Kg, R_O2, R_O2Sat. STheta and all other R_*
# "reported" columns are also excluded because they are derived quantities.

# quality-code column for each variable (CalCOFI codes: 8 = suspect, 9 = missing)
QCOL <- c(T_degC = "T_qual", Salnty = "S_qual", O2Sat = "O2Satq",
          ChlorA = "Chlqua", Phaeop = "Phaqua", PO4uM = "PO4q",
          SiO3uM = "SIO3qu", NO2uM = "NO2q", NO3uM = "NO3q")

# ----------------------------- 1. LOAD + CLEAN -------------------------------
logf("## 1. Data")
hdr  <- names(fread("data/bottle.csv", nrows = 0, encoding = "Latin-1"))
# quality column names differ slightly between CalCOFI versions -> match loosely
qmatch <- sapply(QCOL, function(q) {
  hit <- hdr[tolower(hdr) == tolower(q)]
  if (length(hit)) hit[1] else NA_character_
})
keep <- unique(c("Cst_Cnt", "Btl_Cnt", "Depth_ID", TARGET, PREDS,
                 na.omit(qmatch)))
bottle <- fread("data/bottle.csv", select = intersect(keep, hdr),
                encoding = "Latin-1", showProgress = FALSE)
steps <- data.table(step = "raw bottles", n = nrow(bottle))

# Year/Month: from cast.csv if present, else parsed from Depth_ID
# (Depth_ID looks like "19-4903CR-HY-..." -> century "19" + YY "49" = 1949)
if (file.exists("data/cast.csv")) {
  chdr <- names(fread("data/cast.csv", nrows = 0))
  cast <- fread("data/cast.csv",
                select = intersect(c("Cst_Cnt", "Year", "Month", "Lat_Dec",
                                     "Lon_Dec"), chdr), showProgress = FALSE)
  bottle <- merge(bottle, cast, by = "Cst_Cnt", all.x = TRUE)
}
if (!"Year" %in% names(bottle) || anyNA(bottle$Year)) {
  yr <- as.integer(paste0(substr(bottle$Depth_ID, 1, 2),
                          substr(bottle$Depth_ID, 4, 5)))
  if (!"Year" %in% names(bottle)) bottle[, Year := yr]
  bottle[is.na(Year), Year := yr[is.na(Year)]]
}
if (!"Month" %in% names(bottle))
  bottle[, Month := as.integer(substr(Depth_ID, 6, 7))]

# quality filter: set flagged values to NA before complete-case selection
if (QUAL_FILTER) {
  nflag <- 0L
  for (v in names(qmatch)) {
    q <- qmatch[[v]]
    if (is.na(q) || !q %in% names(bottle)) next
    bad <- which(bottle[[q]] %in% c(8, 9))
    nflag <- nflag + length(bad)
    if (length(bad)) set(bottle, bad, v, NA)
  }
  logf("- Quality filter: %d values flagged 8/9 set to missing", nflag)
}

for (v in c(TARGET, PREDS)) set(bottle, j = v, value = as.numeric(bottle[[v]]))
d <- bottle[complete.cases(bottle[, c(TARGET, PREDS, "Year"), with = FALSE])]
steps <- rbind(steps, data.table(step = "complete cases (target + 9 preds)",
                                 n = nrow(d)))
# physically impossible values (not 'outliers' -- errors)
d <- d[O2Sat >= 0 & O2Sat <= 250 & Salnty > 25 & Salnty < 40 &
       T_degC > -2 & T_degC < 35 & Depthm >= 0 &
       PO4uM >= 0 & SiO3uM >= 0 & NO2uM >= 0 & NO3uM >= 0 &
       ChlorA >= 0 & Phaeop >= 0]
steps <- rbind(steps, data.table(step = "after physical-range check",
                                 n = nrow(d)))
d[, depth_band := cut(Depthm, c(-Inf, 50, 200, 500, Inf),
                      labels = c("0-50 m", "50-200 m", "200-500 m", ">500 m"),
                      right = FALSE)]
d[, era := fifelse(Year >= 1994, "CTD era (1994+)", "bottle era (<1994)")]

d[, split := fifelse(Year <= TRAIN_END, "train",
             fifelse(Year %in% VALID_YEARS, "valid",
             fifelse(Year >= TEST_START, "test", "unused")))]
steps <- rbind(steps, d[, .(step = paste("split:", split), n = .N), by = split
                        ][, .(step, n)])
fwrite(steps, "outputs/tables/01_sample_sizes.csv")
yr_counts <- d[, .(n_bottles = .N, n_casts = uniqueN(Cst_Cnt)), by = Year
               ][order(Year)]
fwrite(yr_counts, "outputs/tables/02_counts_by_year.csv")
for (i in seq_len(nrow(steps))) logf("- %s: %s", steps$step[i],
                                     format(steps$n[i], big.mark = ","))
logf("- Year range in analysis set: %d-%d", min(d$Year), max(d$Year))

train  <- d[split == "train"]
valid  <- d[split == "valid"]
trval  <- d[split %in% c("train", "valid")]   # final fit after tuning (<= 2010)
test   <- d[split == "test"]
stopifnot(nrow(test) > 0, nrow(valid) > 0)
# Casts never straddle splits: a cast happens in one year, splits are by year.

# ----------------------------- helpers ---------------------------------------
metr <- function(y, p) c(RMSE = sqrt(mean((y - p)^2)), MAE = mean(abs(y - p)),
                         R2 = 1 - sum((y - p)^2) / sum((y - mean(y))^2))
fml  <- function(vars) as.formula(paste(TARGET, "~", paste(vars, collapse = "+")))

fit_bs <- function(dat, vars, brk) {          # linear + hinge at T = brk
  dat <- copy(dat); dat[, hT := pmax(T_degC - brk, 0)]
  m <- lm(fml(c(vars, "hT")), data = dat); attr(m, "brk") <- brk; m
}
pred_bs <- function(m, newd) {
  newd <- copy(newd); newd[, hT := pmax(T_degC - attr(m, "brk"), 0)]
  predict(m, newd)
}
fit_gam <- function(dat, vars) {
  sm <- paste0("s(", vars, ", k = 10)", collapse = " + ")
  bam(as.formula(paste(TARGET, "~", sm)), data = dat, method = "fREML",
      discrete = TRUE, nthreads = THREADS)
}
fit_rf <- function(dat, vars, mtry, mns, seed = SEED, quantreg = FALSE) {
  ranger(fml(vars), data = dat, num.trees = N_TREES,
         mtry = min(mtry, length(vars)), min.node.size = mns,
         num.threads = THREADS, seed = seed, quantreg = quantreg,
         importance = "none")
}
pred_rf <- function(m, newd) predict(m, newd, num.threads = THREADS)$predictions

# ----------------------------- 2. TUNING (validation years only) -------------
logf("\n## 2. Tuning on validation years %s (test years untouched)",
     paste(range(VALID_YEARS), collapse = "-"))
# broken stick: pick break on validation RMSE
grid_b <- seq(8, 20, by = 0.25)
val_b  <- sapply(grid_b, function(b)
  metr(valid$O2Sat, pred_bs(fit_bs(train, PREDS, b), valid))["RMSE"])
BRK <- grid_b[which.min(val_b)]
logf("- Broken-stick temperature break chosen on validation: %.2f C", BRK)

# random forest: small grid
tune_dat <- if (!is.null(TUNE_SUBSAMPLE) && nrow(train) > TUNE_SUBSAMPLE)
  train[sample(.N, TUNE_SUBSAMPLE)] else train
rf_grid <- CJ(mtry = c(2, 3, 5, 9), mns = c(5, 20))
rf_grid[, val_RMSE := mapply(function(a, b)
  metr(valid$O2Sat, pred_rf(fit_rf(tune_dat, PREDS, a, b), valid))["RMSE"],
  mtry, mns)]
fwrite(rf_grid, "outputs/tables/03_rf_tuning.csv")
MTRY <- rf_grid[which.min(val_RMSE), mtry]
MNS  <- rf_grid[which.min(val_RMSE), mns]
logf("- RF tuned on validation: mtry = %d, min.node.size = %d (M2 used 2 / 5)",
     MTRY, MNS)

# ----------------------------- 3. MAIN COMPARISON ----------------------------
logf("\n## 3. Main comparison: fit on <= %d, test on %d-%d (n = %s)",
     max(VALID_YEARS), TEST_START, max(test$Year),
     format(nrow(test), big.mark = ","))
models <- list()
models$`Mean (reference)` <- list(pred = function(nd) rep(mean(trval$O2Sat), nrow(nd)))
m_ols <- lm(fml(PREDS), data = trval)
models$`OLS (9 predictors)` <- list(pred = function(nd) predict(m_ols, nd))
m_bs  <- fit_bs(trval, PREDS, BRK)
models$`Broken stick OLS` <- list(pred = function(nd) pred_bs(m_bs, nd))
t0 <- Sys.time(); m_gam <- fit_gam(trval, PREDS)
logf("- GAM fit time: %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs")))
models$`GAM` <- list(pred = function(nd) as.numeric(predict(m_gam, nd)))
t0 <- Sys.time(); m_rf <- fit_rf(trval, PREDS, MTRY, MNS)
logf("- RF fit time: %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs")))
models$`Random forest` <- list(pred = function(nd) pred_rf(m_rf, nd))

for (nm in names(models)) set(test, j = paste0("p_", nm), value = as.numeric(models[[nm]]$pred(test)))
pcols <- grep("^p_", names(test), value = TRUE)

main <- rbindlist(lapply(names(models), function(nm) {
  tr <- metr(trval$O2Sat, models[[nm]]$pred(trval))
  te <- metr(test$O2Sat, test[[paste0("p_", nm)]])
  data.table(model = nm, train_RMSE = tr["RMSE"], test_RMSE = te["RMSE"],
             test_MAE = te["MAE"], test_R2 = te["R2"])
}))
fwrite(main, "outputs/tables/04_main_results.csv")
for (i in seq_len(nrow(main))) logf("- %-20s test RMSE %.3f | MAE %.3f | R2 %.4f | train RMSE %.3f",
  main$model[i], main$test_RMSE[i], main$test_MAE[i], main$test_R2[i],
  main$train_RMSE[i])
logf("- GAM effective df per smooth: %s",
     paste(sprintf("%s=%.1f", PREDS, summary(m_gam)$edf), collapse = ", "))

# ----------------------------- 4. WHERE does the gap come from? --------------
long <- melt(test[, c("Year", "depth_band", "O2Sat", pcols), with = FALSE],
             id.vars = c("Year", "depth_band", "O2Sat"),
             variable.name = "model", value.name = "pred")
long[, model := sub("^p_", "", model)]
by_depth <- long[, .(RMSE = sqrt(mean((O2Sat - pred)^2)),
                     MAE = mean(abs(O2Sat - pred)), n = .N),
                 by = .(depth_band, model)][order(depth_band, model)]
by_year  <- long[, .(RMSE = sqrt(mean((O2Sat - pred)^2)), n = .N),
                 by = .(Year, model)][order(Year, model)]
fwrite(by_depth, "outputs/tables/05_test_error_by_depth.csv")
fwrite(by_year,  "outputs/tables/06_test_error_by_year.csv")
logf("\n## 4. Test RMSE by depth band (OLS vs RF)")
w <- dcast(by_depth, depth_band + n ~ model, value.var = "RMSE")
for (i in seq_len(nrow(w))) logf("- %-10s n=%6d  OLS %.2f  GAM %.2f  RF %.2f",
  w$depth_band[i], w$n[i], w$`OLS (9 predictors)`[i], w$GAM[i],
  w$`Random forest`[i])

# ----------------------------- 5. ABLATION: which feature group matters? -----
logf("\n## 5. Feature-group ablation (fit <= %d, test %d+)",
     max(VALID_YEARS), TEST_START)
sets <- list(`All 9`              = PREDS,
             `Drop nutrients`     = c(PHYS, BIO),
             `Drop biology`       = c(PHYS, NUTR),
             `Drop physical`      = c(NUTR, BIO),
             `Physical only`      = PHYS,
             `Nutrients only`     = NUTR)
abl <- rbindlist(lapply(names(sets), function(s) {
  v <- sets[[s]]
  po <- predict(lm(fml(v), data = trval), test)
  pg <- as.numeric(predict(fit_gam(trval, v), test))
  pr <- pred_rf(fit_rf(trval, v, MTRY, MNS), test)
  data.table(feature_set = s, n_vars = length(v),
             OLS = metr(test$O2Sat, po)["RMSE"],
             GAM = metr(test$O2Sat, pg)["RMSE"],
             RF  = metr(test$O2Sat, pr)["RMSE"])
}))
abl[, `RF gain over OLS (%)` := round(100 * (OLS - RF) / OLS, 1)]
fwrite(abl, "outputs/tables/07_ablation.csv")
for (i in seq_len(nrow(abl))) logf("- %-15s OLS %.2f | GAM %.2f | RF %.2f | RF gain %.1f%%",
  abl$feature_set[i], abl$OLS[i], abl$GAM[i], abl$RF[i], abl$`RF gain over OLS (%)`[i])

# ----------------------------- 6. UNCERTAINTY --------------------------------
logf("\n## 6. Uncertainty")
# (a) uncertainty in the PERFORMANCE ESTIMATE: RF across seeds
seed_rmse <- sapply(seq_len(N_SEEDS), function(s)
  metr(test$O2Sat, pred_rf(fit_rf(trval, PREDS, MTRY, MNS, seed = s), test))["RMSE"])
fwrite(data.table(seed = seq_len(N_SEEDS), test_RMSE = seed_rmse),
       "outputs/tables/08_rf_seed_variation.csv")
logf("- RF test RMSE across %d seeds: mean %.3f, sd %.4f, range %.3f-%.3f",
     N_SEEDS, mean(seed_rmse), sd(seed_rmse), min(seed_rmse), max(seed_rmse))
logf("- (Year-to-year spread is in table 06 -- usually much bigger than seed spread)")

# (b) uncertainty about an INDIVIDUAL OUTCOME: 90% prediction intervals
m_qrf <- fit_rf(trval, PREDS, MTRY, MNS, quantreg = TRUE)
q <- predict(m_qrf, test, type = "quantiles", quantiles = c(0.05, 0.95),
             num.threads = THREADS)$predictions
pi_ols <- predict(m_ols, test, interval = "prediction", level = 0.90)
test[, `:=`(qrf_lo = q[, 1], qrf_hi = q[, 2],
            ols_lo = pi_ols[, "lwr"], ols_hi = pi_ols[, "upr"])]
cov_tab <- test[, .(n = .N,
  OLS_coverage = mean(O2Sat >= ols_lo & O2Sat <= ols_hi),
  OLS_width    = mean(ols_hi - ols_lo),
  QRF_coverage = mean(O2Sat >= qrf_lo & O2Sat <= qrf_hi),
  QRF_width    = mean(qrf_hi - qrf_lo)), by = depth_band][order(depth_band)]
cov_all <- test[, .(depth_band = "ALL", n = .N,
  OLS_coverage = mean(O2Sat >= ols_lo & O2Sat <= ols_hi),
  OLS_width    = mean(ols_hi - ols_lo),
  QRF_coverage = mean(O2Sat >= qrf_lo & O2Sat <= qrf_hi),
  QRF_width    = mean(qrf_hi - qrf_lo))]
cov_tab <- rbind(cov_all, cov_tab)
fwrite(cov_tab, "outputs/tables/09_interval_coverage.csv")
logf("- 90%% interval coverage on test (target 0.90):")
for (i in seq_len(nrow(cov_tab))) logf("  - %-10s OLS %.3f (width %.1f) | QRF %.3f (width %.1f)",
  cov_tab$depth_band[i], cov_tab$OLS_coverage[i], cov_tab$OLS_width[i],
  cov_tab$QRF_coverage[i], cov_tab$QRF_width[i])

# ----------------------------- 7. ROLLING ORIGIN -----------------------------
if (RUN_ROLLING) {
  logf("\n## 7. Rolling origin: for each test year y, fit on all years < y")
  yrs <- sort(unique(test$Year))
  roll <- rbindlist(lapply(yrs, function(y) {
    tr <- d[Year < y]; te <- d[Year == y]
    data.table(test_year = y, n_test = nrow(te),
      OLS = metr(te$O2Sat, predict(lm(fml(PREDS), data = tr), te))["RMSE"],
      GAM = metr(te$O2Sat, as.numeric(predict(fit_gam(tr, PREDS), te)))["RMSE"],
      RF  = metr(te$O2Sat, pred_rf(fit_rf(tr, PREDS, MTRY, MNS), te))["RMSE"])
  }))
  fwrite(roll, "outputs/tables/10_rolling_origin.csv")
  for (i in seq_len(nrow(roll))) logf("- %d (n=%5d): OLS %.2f | GAM %.2f | RF %.2f",
    roll$test_year[i], roll$n_test[i], roll$OLS[i], roll$GAM[i], roll$RF[i])
}

# ----------------------------- 8. CTD-ERA TRAINING ---------------------------
if (RUN_CTD_ERA) {
  logf("\n## 8. Does old bottle-era data help or hurt? (train 1994-%d only)",
       max(VALID_YEARS))
  ctd <- trval[Year >= 1994]
  ctd_res <- data.table(training = c("All years <= 2010", "CTD era 1994-2010"),
    n_train = c(nrow(trval), nrow(ctd)),
    OLS = c(main[model == "OLS (9 predictors)", test_RMSE],
            metr(test$O2Sat, predict(lm(fml(PREDS), data = ctd), test))["RMSE"]),
    RF  = c(main[model == "Random forest", test_RMSE],
            metr(test$O2Sat, pred_rf(fit_rf(ctd, PREDS, MTRY, MNS), test))["RMSE"]))
  fwrite(ctd_res, "outputs/tables/11_ctd_era_training.csv")
  for (i in 1:2) logf("- %-18s n=%7d  OLS %.2f | RF %.2f", ctd_res$training[i],
                      ctd_res$n_train[i], ctd_res$OLS[i], ctd_res$RF[i])
}

# ----------------------------- 9. FIGURES ------------------------------------
theme_set(theme_minimal(base_size = 12))
mod_order <- names(models)
by_depth[, model := factor(model, levels = mod_order)]
ggsave("outputs/figures/fig1_rmse_by_depth.png",
  ggplot(by_depth[model != "Mean (reference)"],
         aes(depth_band, RMSE, fill = model)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    labs(x = "Depth band", y = "Test RMSE (O2 saturation, % points)",
         fill = NULL, title = sprintf("Where each model misses (test years %d-%d)",
                                      TEST_START, max(test$Year))) +
    theme(legend.position = "bottom"), width = 8, height = 5, dpi = 200)

by_year[, model := factor(model, levels = mod_order)]
ggsave("outputs/figures/fig2_rmse_by_year.png",
  ggplot(by_year[model != "Mean (reference)"], aes(Year, RMSE, colour = model)) +
    geom_line() + geom_point() +
    labs(y = "Test RMSE (% points)", colour = NULL,
         title = "Test error by year (all fit on data <= 2010)") +
    theme(legend.position = "bottom"), width = 8, height = 5, dpi = 200)

ablm <- melt(abl[, .(feature_set, OLS, GAM, RF)], id.vars = "feature_set",
             variable.name = "model", value.name = "RMSE")
ablm[, feature_set := factor(feature_set, levels = rev(names(sets)))]
ggsave("outputs/figures/fig3_ablation.png",
  ggplot(ablm, aes(RMSE, feature_set, fill = model)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    labs(x = "Test RMSE (% points)", y = NULL, fill = NULL,
         title = "Which feature group carries the signal?") +
    theme(legend.position = "bottom"), width = 8, height = 5, dpi = 200)

# Shapes the flexible models learned: GAM smooth vs RF partial dependence
pd_vars <- c("T_degC", "NO3uM", "Depthm", "ChlorA")
ref <- trval[sample(.N, min(.N, 2000))]
shape <- rbindlist(lapply(pd_vars, function(v) {
  g <- seq(quantile(trval[[v]], .01), quantile(trval[[v]], .99), length.out = 40)
  rbindlist(lapply(g, function(x) {
    nd <- copy(ref); set(nd, j = v, value = x)
    data.table(var = v, x = x,
               GAM = mean(predict(m_gam, nd)), RF = mean(pred_rf(m_rf, nd)),
               OLS = mean(predict(m_ols, nd)))
  }))
}))
shape <- melt(shape, id.vars = c("var", "x"), variable.name = "model",
              value.name = "avg_pred")
ggsave("outputs/figures/fig4_partial_dependence.png",
  ggplot(shape, aes(x, avg_pred, colour = model)) + geom_line(linewidth = 0.9) +
    facet_wrap(~var, scales = "free_x") +
    labs(x = NULL, y = "Average predicted O2Sat (%)", colour = NULL,
         title = "Effect of one predictor, others held at observed values") +
    theme(legend.position = "bottom"), width = 9, height = 6, dpi = 200)

best <- main[model != "Mean (reference)"][which.min(test_RMSE), model]
ggsave("outputs/figures/fig5_obs_vs_pred.png",
  ggplot(test, aes(.data[[paste0("p_", best)]], O2Sat)) +
    geom_bin2d(bins = 80) + geom_abline(colour = "red") +
    scale_fill_viridis_c(trans = "log10") +
    labs(x = paste("Predicted O2Sat (%) -", best), y = "Observed O2Sat (%)",
         title = "Observed vs predicted, test years"), width = 6, height = 5.5, dpi = 200)

sink("outputs/sessionInfo.txt"); print(sessionInfo()); sink()
logf("\nDone %s. Tables in outputs/tables, figures in outputs/figures.",
     format(Sys.time()))
