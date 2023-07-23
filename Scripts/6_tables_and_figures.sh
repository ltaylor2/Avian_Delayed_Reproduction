#!/bin/bash

# Evolution of delayed reproduction in birds
# Bash script to generate MS tables and figure in R
# L.U.T. updated 2023-07-23

module load miniconda
conda activate avesalpha

echo "Starting Scripts/6_tables_and_figures.sh"

echo "Generating MS tables and figures via Scripts/tables_and_figures.r"

Rscript --slave Scripts/tables_and_figures.r

echo "Done with Scripts/6_tables_and_figures.sh"