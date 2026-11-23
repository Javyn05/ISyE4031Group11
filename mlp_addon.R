# =============================================================================
# ISYE 4031 - Team 11 - Milestone 3 add-on: neural network (MLP)
#
# Question: a GAM gives smooth curves without interactions; the random forest
# gives interactions with step-like shapes. An MLP is smooth AND can model
# interactions. Does it match the GAM at depth and the forest at the surface?
# That tests our "curves at depth, interactions at the surface" explanation.
#
# Same data cleaning, same 9 predictors, same year split as m3_pipeline.R.
# Settings chosen on 2009-2010 only; test years 2011-2016 used once.
#
# Run from the project folder AFTER m3_pipeline.R:   Rscript mlp_addon.R
# Needs: data/bottle.csv, data/cast.csv, outputs/tables/04 + 05 (from pipeline)
# Writes NEW files only (12_..16_ tables, fig6, mlp_log.md); never overwrites
# the pipeline's outputs.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(nnet)      # single-hidden-layer MLP (ships with R)
  library(ggplot2)
})

# ----------------------------- SETTINGS --------------------------------------
SEED           <- 4031
TRAIN_END      <- 2008
VALID_YEARS    <- 2009:2010
TEST_START     <- 2011
QUAL_FILTER    <- TRUE
SIZES          <- c(5, 10, 20)          # hidden neurons (tuning grid)
DECAYS         <- c(1e-4, 1e-3, 1e-2)   # weight decay (tuning grid)
TUNE_SUBSAMPLE <- 60000                 # same as pipeline's RF tuning; NULL = all
MAXIT_TUNE     <- 500
MAXIT_FINAL    <- 1000
N_SEEDS        <- 5
CORES          <- as.integer(Sys.getenv("MLP_CORES",
                                        max(1, parallel::detectCores() - 1)))
if (nzchar(Sys.getenv("MLP_QUICK_TEST"))) {   # tiny run to check the code works
  SIZES <- 5; DECAYS <- 1e-3; TUNE_SUBSAMPLE <- 5000
  MAXIT_TUNE <- 20; MAXIT_FINAL <- 20; N_SEEDS <- 2
}

dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
LOG <- "outputs/mlp_log.md"
cat(sprintf("# MLP add-on log (auto-generated)\nRun started %s\n\n",
            format(Sys.time())), file = LOG)
logf <- function(...) {
  msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = LOG, append = TRUE)
}
set.seed(SEED)

PHYS  <- c("Depthm", "T_degC", "Salnty")
NUTR  <- c("PO4uM", "SiO3uM", "NO2uM", "NO3uM")
BIO   <- c("ChlorA", "Phaeop")
PREDS <- c(PHYS, NUTR, BIO)
TARGET <- "O2Sat"
QCOL <- c(T_degC = "T_qual", Salnty = "S_qual", O2Sat = "O2Satq",
          ChlorA = "Chlqua", Phaeop = "Phaqua", PO4uM = "PO4q",
          SiO3uM = "SIO3qu", NO2uM = "NO2q", NO3uM = "NO3q")

# ----------------------------- 1. DATA (identical to m3_pipeline.R) ----------
hdr  <- names(fread("data/bottle.csv", nrows = 0, encoding = "Latin-1"))
qmatch <- sapply(QCOL, function(q) {
  hit <- hdr[tolower(hdr) == tolower(q)]
  if (length(hit)) hit[1] else NA_character_
})
keep <- unique(c("Cst_Cnt", "Btl_Cnt", "Depth_ID", TARGET, PREDS,
                 na.omit(qmatch)))
bottle <- fread("data/bottle.csv", select = intersect(keep, hdr),
                encoding = "Latin-1", showProgress = FALSE)
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
if (QUAL_FILTER) {
  for (v in names(qmatch)) {
    q <- qmatch[[v]]
    if (is.na(q) || !q %in% names(bottle)) next
    bad <- which(bottle[[q]] %in% c(8, 9))
    if (length(bad)) set(bottle, bad, v, NA)
  }
}
for (v in c(TARGET, PREDS)) set(bottle, j = v, value = as.numeric(bottle[[v]]))
d <- bottle[complete.cases(bottle[, c(TARGET, PREDS, "Year"), with = FALSE])]
d <- d[O2Sat >= 0 & O2Sat <= 250 & Salnty > 25 & Salnty < 40 &
       T_degC > -2 & T_degC < 35 & Depthm >= 0 &
       PO4uM >= 0 & SiO3uM >= 0 & NO2uM >= 0 & NO3uM >= 0 &
       ChlorA >= 0 & Phaeop >= 0]
d[, depth_band := cut(Depthm, c(-Inf, 50, 200, 500, Inf),
                      labels = c("0-50 m", "50-200 m", "200-500 m", ">500 m"),
                      right = FALSE)]
d[, split := fifelse(Year <= TRAIN_END, "train",
             fifelse(Year %in% VALID_YEARS, "valid",
             fifelse(Year >= TEST_START, "test", "unused")))]
train <- d[split == "train"]
valid <- d[split == "valid"]
trval <- d[split %in% c("train", "valid")]
test  <- d[split == "test"]
logf("## 1. Data\n- train %s | valid %s | test %s bottles",
     format(nrow(train), big.mark = ","), format(nrow(valid), big.mark = ","),
     format(nrow(test), big.mark = ","))

# sanity check: same rows as the pipeline?
if (file.exists("outputs/tables/01_sample_sizes.csv")) {
  ss <- fread("outputs/tables/01_sample_sizes.csv")
  n_pipe <- ss[step == "split: test", n]
  if (length(n_pipe) && n_pipe != nrow(test))
    warning(sprintf("Test set has %d rows; pipeline had %d. Check data files.",
                    nrow(test), n_pipe))
  else logf("- Test rows match m3_pipeline.R (%s)", format(n_pipe, big.mark = ","))
}

# ----------------------------- helpers ---------------------------------------
metr <- function(y, p) c(RMSE = sqrt(mean((y - p)^2)), MAE = mean(abs(y - p)),
                         R2 = 1 - sum((y - p)^2) / sum((y - mean(y))^2))

# Inputs and target are standardized with the FITTING data's mean/sd only.
fit_mlp <- function(dat, size, decay, seed, maxit) {
  X  <- as.matrix(dat[, ..PREDS])
  mu <- colMeans(X); s <- apply(X, 2, sd)
  ymu <- mean(dat[[TARGET]]); ysd <- sd(dat[[TARGET]])
  set.seed(seed)
  m <- nnet(scale(X, mu, s), (dat[[TARGET]] - ymu) / ysd,
            size = size, decay = decay, linout = TRUE, maxit = maxit,
            MaxNWts = 10000, trace = FALSE)
  list(m = m, mu = mu, s = s, ymu = ymu, ysd = ysd)
}
pred_mlp <- function(f, nd) {
  X <- scale(as.matrix(nd[, ..PREDS]), f$mu, f$s)
  as.numeric(predict(f$m, X)) * f$ysd + f$ymu
}
par_lapply <- function(X, FUN) {
  if (.Platform$OS.type == "unix" && CORES > 1)
    parallel::mclapply(X, FUN, mc.cores = CORES, mc.preschedule = FALSE)
  else lapply(X, FUN)
}

# ----------------------------- 2. TUNING (2009-2010 only) --------------------
tune_dat <- if (!is.null(TUNE_SUBSAMPLE) && nrow(train) > TUNE_SUBSAMPLE)
  train[sample(.N, TUNE_SUBSAMPLE)] else train
grid <- CJ(size = SIZES, decay = DECAYS)
logf("\n## 2. Tuning on %d-%d (%d settings, %s training rows)",
     min(VALID_YEARS), max(VALID_YEARS), nrow(grid),
     format(nrow(tune_dat), big.mark = ","))
t0 <- Sys.time()
grid[, val_RMSE := unlist(par_lapply(seq_len(nrow(grid)), function(i) {
  f <- fit_mlp(tune_dat, grid$size[i], grid$decay[i], SEED, MAXIT_TUNE)
  metr(valid$O2Sat, pred_mlp(f, valid))[["RMSE"]]
}))]
fwrite(grid, "outputs/tables/12_mlp_tuning.csv")
SIZE  <- grid[which.min(val_RMSE), size]
DECAY <- grid[which.min(val_RMSE), decay]
logf("- Chosen: %d hidden neurons, weight decay %g (valid RMSE %.3f) [%.1f min]",
     SIZE, DECAY, min(grid$val_RMSE),
     as.numeric(difftime(Sys.time(), t0, units = "mins")))

# ----------------------------- 3. FINAL FIT + TEST (5 seeds) -----------------
logf("\n## 3. Refit on <= %d with %d seeds, test on %d-%d",
     max(VALID_YEARS), N_SEEDS, TEST_START, max(test$Year))
t0 <- Sys.time()
fits <- par_lapply(seq_len(N_SEEDS), function(k)
  fit_mlp(trval, SIZE, DECAY, SEED + k, MAXIT_FINAL))
logf("- Fit time: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

P <- sapply(fits, pred_mlp, nd = test)          # test predictions, one col/seed
seed_res <- rbindlist(lapply(seq_len(N_SEEDS), function(k) {
  te <- metr(test$O2Sat, P[, k])
  tr <- metr(trval$O2Sat, pred_mlp(fits[[k]], trval))
  data.table(seed = SEED + k, train_RMSE = tr[["RMSE"]],
             test_RMSE = te[["RMSE"]], test_MAE = te[["MAE"]],
             test_R2 = te[["R2"]], hit_maxit = fits[[k]]$m$convergence == 1)
}))
fwrite(seed_res, "outputs/tables/13_mlp_seed_results.csv")
for (i in seq_len(N_SEEDS)) logf("- seed %d: test RMSE %.3f | MAE %.3f | train RMSE %.3f%s",
  seed_res$seed[i], seed_res$test_RMSE[i], seed_res$test_MAE[i],
  seed_res$train_RMSE[i], if (seed_res$hit_maxit[i]) " (hit maxit)" else "")
logf("- Mean over seeds: test RMSE %.3f (sd %.3f), MAE %.3f, R2 %.4f",
     mean(seed_res$test_RMSE), sd(seed_res$test_RMSE),
     mean(seed_res$test_MAE), mean(seed_res$test_R2))
ens <- metr(test$O2Sat, rowMeans(P))
logf("- Average of the %d networks' predictions: test RMSE %.3f", N_SEEDS, ens[["RMSE"]])

# ----------------------------- 4. BY DEPTH + COMBINED TABLES -----------------
MLP <- "MLP (neural net)"
mlp_depth <- rbindlist(lapply(seq_len(N_SEEDS), function(k)
  data.table(depth_band = test$depth_band, O2Sat = test$O2Sat, pred = P[, k], seed = k)
))[, .(RMSE = sqrt(mean((O2Sat - pred)^2)), MAE = mean(abs(O2Sat - pred)), n = .N),
   by = .(depth_band, seed)
 ][, .(model = MLP, RMSE = mean(RMSE), RMSE_sd = sd(RMSE), MAE = mean(MAE), n = n[1]),
   by = depth_band][order(depth_band)]
fwrite(mlp_depth, "outputs/tables/14_mlp_by_depth.csv")

logf("\n## 4. Test RMSE by depth band")
if (file.exists("outputs/tables/05_test_error_by_depth.csv")) {
  dep <- rbind(fread("outputs/tables/05_test_error_by_depth.csv"),
               mlp_depth[, .(depth_band, model, RMSE, MAE, n)])
  fwrite(dep, "outputs/tables/16_test_error_by_depth_with_mlp.csv")
  w <- dcast(dep, depth_band + n ~ model, value.var = "RMSE")
  for (i in seq_len(nrow(w))) logf("- %-10s OLS %.2f | GAM %.2f | RF %.2f | MLP %.2f",
    w$depth_band[i], w$`OLS (9 predictors)`[i], w$GAM[i],
    w$`Random forest`[i], w[[MLP]][i])
  dep[, depth_band := factor(depth_band, levels = c("0-50 m", "50-200 m", "200-500 m"))]
  dep[, model := factor(model, levels = c("OLS (9 predictors)", "Broken stick OLS",
                                          "GAM", "Random forest", MLP))]
  theme_set(theme_minimal(base_size = 12))
  ggsave("outputs/figures/fig6_rmse_by_depth_with_mlp.png",
    ggplot(dep[!is.na(model)], aes(depth_band, RMSE, fill = model)) +
      geom_col(position = position_dodge(0.85), width = 0.8) +
      labs(x = "Depth band", y = "Test RMSE (O2 saturation, % points)", fill = NULL,
           title = sprintf("Where each model misses, with MLP (test years %d-%d)",
                           TEST_START, max(test$Year))) +
      theme(legend.position = "bottom"), width = 9, height = 5, dpi = 200)
} else {
  logf("- outputs/tables/05 not found: run m3_pipeline.R first for the comparison")
}

if (file.exists("outputs/tables/04_main_results.csv")) {
  main <- rbind(fread("outputs/tables/04_main_results.csv"),
                data.table(model = MLP, train_RMSE = mean(seed_res$train_RMSE),
                           test_RMSE = mean(seed_res$test_RMSE),
                           test_MAE = mean(seed_res$test_MAE),
                           test_R2 = mean(seed_res$test_R2)))
  fwrite(main, "outputs/tables/15_main_results_with_mlp.csv")
}
logf("\nDone %s.", format(Sys.time()))
