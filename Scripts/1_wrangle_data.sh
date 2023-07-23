#!/bin/bash

# Evolution of delayed reproduction in birds
# Bash script to wrangle data using R
# L.U.T. updated 2023-07-19

mkdir Output/
mkdir Plots/

module load miniconda
conda activate avesalpha

echo "Starting Scripts/1_wrangle_data.sh"

echo "Wrangling age at first reproduction data via Scripts/wrangle_data.r"

Rscript --slave Scripts/wrangle_data.r

echo "Done with Scripts/1_wrangle_data.sh"