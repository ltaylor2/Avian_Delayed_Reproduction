#!/bin/bash
#SBATCH -p day
#SBATCH -c 64
#SBATCH --mem-per-cpu 3000
#SBATCH -t 1-00:00:00
#SBATCH -J aa_2
#SBATCH -o Output/Logs/sysout_aa_2.txt
#SBATCH --mail-type ALL

# Evolution of delayed reproduction in birds
# Slurm script to estimate OU model parameters using R
# OUTPUT: Writes Output/results_ou_models.csv
# L.U.T. updated 2023-07-19

module load miniconda
conda activate avesalpha

echo "Starting Scripts/2_ou_models.sh"

echo "Calculating continuous evolutionary models with Scripts/ou_models.r"

export OMP_NUM_THREADS=1
export R_PROGRESSR_ENABLE=TRUE
Rscript --slave Scripts/ou_models.r

echo "Done with Scripts/2_ou_models.sh"