# ISyE4031Group11

Predicting ocean oxygen saturation (O2Sat) from CalCOFI bottle data: depth,
temperature, salinity, four nutrients and two plankton pigments. Every model
is trained on 1977-2008, tuned on 2009-2010 and tested once on 2011-2016.

## Files

| File | What it does |
|---|---|
| `m3_pipeline.R` | Main Milestone 3 analysis: mean, OLS, broken-stick OLS, GAM and random forest on the same split; writes `outputs/tables/01-11` and `outputs/figures/fig1-fig5` |
| `mlp_addon.R` | Neural network (single-hidden-layer MLP, `nnet`) on the same data and split; writes `outputs/tables/12-16`, `outputs/figures/fig6_rmse_by_depth_with_mlp.png` and `outputs/mlp_log.md` |
| `ice_mlp.sbatch` | Slurm job script for running `mlp_addon.R` on PACE ICE |
| `setup_r_packages.R` | One-time R package install (run on the ICE login node) |

## Data

`data/bottle.csv` and `data/cast.csv` (CalCOFI) are not in the repo because
they are too large for GitHub. Copy them into `data/` before running.

## Running

Locally (from the repo folder):

```
Rscript m3_pipeline.R
Rscript mlp_addon.R       # run after the pipeline; it reads tables 01, 04 and 05
```

On PACE ICE:

```
git clone https://github.com/Javyn05/ISyE4031Group11.git && cd ISyE4031Group11
scp bottle.csv cast.csv YOUR_GT_USERNAME@login-ice.pace.gatech.edu:~/ISyE4031Group11/data/   # run on your laptop
# (packages install automatically inside the job; R only loads on compute nodes)
sbatch ice_mlp.sbatch
```

To check that the MLP code runs before a full job, set `MLP_QUICK_TEST=1`
(tiny network and few iterations; about a minute).
