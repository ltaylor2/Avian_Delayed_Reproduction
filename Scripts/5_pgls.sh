#!/bin/bash
#SBATCH -p day
#SBATCH -c 64
#SBATCH --mem-per-cpu 3000
#SBATCH -t 1-00:00:00
#SBATCH -J aa_5
#SBATCH -o Output/Logs/sysout_aa_5.txt
#SBATCH --mail-type ALL

# Evolution of delayed reproduction in birds
# Slurm script to run PGLS models parameters using R
# OUTPUT: Writes Output/results_pgls.csv
# L.U.T. updated 2023-07-19

module load miniconda
conda activate avesalpha

echo "Starting Scripts/5_pgls.sh"

echo "Fitting PGLS models with Scripts/pgls.r"
export OMP_NUM_THREADS=1
export R_PROGRESSR_ENABLE=TRUE
Rscript --slave Scripts/pgls.r

echo "Done with Scripts/5_pgls.sh"