# Installs any missing R packages. On ICE this runs automatically inside ice_mlp.sbatch
# (R is only available on compute nodes). Locally:
#   module load r
#   Rscript setup_r_packages.R
lib <- file.path(Sys.getenv("HOME"), "R", "library")   # same path ice_mlp.sbatch uses
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
need <- c("data.table", "ggplot2", "nnet", "ranger", "mgcv")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss, lib = lib, repos = "https://cloud.r-project.org")
cat("Installed / available:", need, "\nLibrary:", lib, "\n")
