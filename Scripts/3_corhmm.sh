#!/bin/bash

# Evolution of delayed reproduction in birds
# Slurm script to estimate discrete markov model parameters using R
# Submits jobs via deadSimpleQueue
# OUTPUT: Writes Output/results_corhmm_*VARS.csv
# L.U.T. updated 2023-07-19

echo "Starting Scripts/3_corhmm.sh"

JOBTXT=""
for VAR in "Alpha_F" "Alpha_M" "Min_Alpha_F" "Min_Alpha_M"
do
    JOBTXT+="module load miniconda; conda activate avesalpha; "
    JOBTXT+="export OMP_NUM_THREADS=1; export R_PROGRESSR_ENABLE=TRUE; "
    JOBTXT+="Rscript --slave Scripts/corhmm.r $VAR\n"
done
echo -e $JOBTXT > JOBLIST_corhmm.txt

module load dSQ
dsq --job-file JOBLIST_corhmm.txt -c 64 --mem-per-cpu 2000 -t 2-00:00:00 --mail-type ALL --submit --status-dir Output/Logs

echo "Submitted corHMM jobs to run via dsq"
