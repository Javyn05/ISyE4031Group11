#!/bin/bash
# Train the MLP add-on on Georgia Tech PACE ICE.
# Submit from the repo folder:   sbatch ice_mlp.sbatch
# Check progress:                squeue -u $USER    and    tail -f mlp_<jobid>.out
#SBATCH -J o2_mlp
#SBATCH -N 1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=24G
#SBATCH -t 01:00:00
#SBATCH -o mlp_%j.out

cd "$SLURM_SUBMIT_DIR"
module load r            # R is only available on compute nodes on ICE
export MLP_CORES=$SLURM_CPUS_PER_TASK
export R_LIBS_USER="$HOME/R/library"

ls data/bottle.csv data/cast.csv || { echo "Copy bottle.csv and cast.csv into data/ first"; exit 1; }
Rscript setup_r_packages.R || exit 1   # installs only packages the module lacks
Rscript mlp_addon.R
