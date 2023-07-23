# SOCIAL CONTEXT AND THE EVOLUTION OF DELAYED REPRODUCTION IN BIRDS
## Data and code repository
## Liam U. Taylor and Richard O. Prum
## Last updated 2023-07-23 (preprint submission version)

### ABSTRACT
Classic life history theory makes generalized predictions about phenotypic correlations across large clades. Modern comparative tests of these correlations account for the underlying structure of phylogenetic trees. Yet neither life history theory nor phylogenetic comparative methods automatically specify how biological mechanisms generate correlations. This problem is evident in comparative analyses of birds. Birds show a strong correlation between body size and age at first reproduction, but do not actually grow larger if they delay reproduction. Instead, field studies raise the hypothesis that complex social contexts—especially cooperative breeding, coloniality, and lekking—generate diverse demands for behavioral development, which in turn result in delayed reproduction. Here, we support that hypothesis with a comparative dataset spanning 961 species in 155 avian families. Continuous (Ornstein-Uhlenbeck), discrete (hidden state Markov), and phylogenetic regression models revealed delayed reproduction in colonial birds, a weaker signal across cooperative birds, and the convergent evolution of sexual bimaturism in polygynous, lekking birds. These results show an association between social context, sex-specific developmental demands, and life history evolution in birds. We discuss how even statistically powerful phylogenetic regressions—whether focused on mass, lifespan, or our own broad social categories—can ultimately fail to model the history of life history evolution.

### About this repository
This repository contains all data and code necessary to generate manuscript results and figures. 
Scripts can be run in order: 

Data (in the Data/ directory):
1. data_raw_2023-07-23.xlsx (raw data, including full age sources and text quotations)
2. Clements_Taxonomy_v2022.csv (Avian taxonomy from [Clements 2022](https://www.birds.cornell.edu/clementschecklist/))
3. AVONET_Clements_Data.csv (AVONET data, including mass, from [Tobias et al. 2022](https://opentraits.org/datasets/avonet.html#:~:text=description%20%3A%20The%20AVONET%20database%20contains,on%20range%20size%20and%20location))
4. Taxonomy_Dictionary_Clements_To_BirdTree.csv (dictionary to map Clements v2022 names to BirdTree phylogeny tip labels)
5. BirdTree_1000_Trees.nex (a distribution of 1,000 phylogenetic from Jetz et al. 2012 [BirdTree](https://birdtree.org/))
6. alpha_distribution_subset.csv (example age at first reproduction distributions from well-studied species for Fig. S3)
7. tree_images.r (information for drawing [PhyloPic](https://www.phylopic.org/) images on the tree figures)

Analysis scripts (in the Scripts/ directory):
1. 1_wrangle_data.sh (wrangles main data file to clean version for analyses)
2. 2_ou_models.sh (continuous evolutionary models in OUwie)
3. 3_corhmm.sh (discrete evolutionary models in corHMM)
4. 4_phyanova.sh (phylogenetic ANOVAs in phytools; requires OU model results)
5. 5_pgls.sh (phylogenetic generalized least squares models in nlme)
6. 6_tables_and_figures.sh (generates MS tables and figures)

### Software requirements
Scripts are written for Unix bash (Ubuntu) and R v4.3.1. See scripts and manuscript for packages and software citations. Bash scripts were originally run with Yale HPC resources ([McClearly cluster] (https://docs.ycrc.yale.edu/clusters/mccleary/)) using [slurm](https://slurm.schedmd.com/documentation.html).

Required R packages can be installed in R with:
```R
install.packages(c("tidyverse",
                   "readxl",
                   "ape",
                   "furrr",
                   "OUwie",
                   "progressr",
                   "corHMM",
                   "phytools",
                   "nlme",
                   "broom.mixed",
                   "ggtree",
                   "patchwork"))
```                   