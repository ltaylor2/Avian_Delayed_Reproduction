#!/bin/bash
#SBATCH -p day
#SBATCH -c 64
#SBATCH --mem-per-cpu 3000
#SBATCH -t 12:00:00
#SBATCH -J aa_4
#SBATCH -o Output/Logs/sysout_aa_4.txt
#SBATCH --mail-type ALL

# Evolution of delayed reproduction in birds
# Slurm script to run phylogenetic ANOVAs using R
# L.U.T. updated 2023-07-19

module load miniconda
conda activate avesalpha

echo "Starting Scripts/4_phyanova.sh"

echo "Calculating phyANOVA results with Scripts/phyanova.r"

export OMP_NUM_THREADS=1
export R_PROGRESSR_ENABLE=TRUE
Rscript --slave Scripts/phyanova.r

echo "Done with Scripts/4_phyanova.sh"