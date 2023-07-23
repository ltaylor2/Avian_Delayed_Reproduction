#!/bin/bash
#SBATCH -p week
#SBATCH -c 64
#SBATCH --mem-per-cpu 3000
#SBATCH -t 7-00:00:00
#SBATCH -J aa_3
#SBATCH -o Output/Logs/sysout_aa_3.txt
#SBATCH --mail-type ALL

# Evolution of delayed reproduction in birds
# Slurm script to estimate discrete markov model parameters using R
# OUTPUT: Writes Output/results_corhmm.csv
# L.U.T. updated 2023-07-19

module load miniconda
conda activate avesalpha

echo "Starting Scripts/3_corhmm.sh"

echo "Testing discrete evolutionary models with Scripts/corhmm.r"

export OMP_NUM_THREADS=1
export R_PROGRESSR_ENABLE=TRUE
Rscript --slave Scripts/corhmm.r

echo "Done with Scripts/3_corhmm.sh"