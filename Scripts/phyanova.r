# Evolution of delayed reproduction in birds
# R script to run phylogenetic ANOVAs
# OUTPUT: Writes Output/results_phyanova.csv
# L.U.T. updated 2023-07-19

# Logistics -------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(furrr))
suppressPackageStartupMessages(library(phytools))
suppressPackageStartupMessages(library(progressr))

handlers(global=TRUE)
handlers("progress")

# Organizing data -------------------------------------

cat("Organizing data.\n")

# Read in data and trees
data <- read_tsv("Data/data_clean.tsv", show_col_types=FALSE)
trees <- read.nexus("Data/BirdTree_1000_Trees.nex")

# Models -------------------------------------

# Custom function to run a single iteration of 
# phylogenetic ANOVAs  for one variable on one tree from the tree distribution,
paovIter <- function(var, i, d, tree) {

    # Initialize independent variable (social), mapping as a vector ordered by tree tip labels
    ind <- map_chr(tree$tip.label, ~ d[d$Label_BirdTree==., "Social_Context"][[1]])
    ind <- factor(ind, levels=c("Other", "Cooperative", "Colony", "Lek"))
    names(ind) <- tree$tip.label

    # Initialize dependent variable (age), mapping as a vector ordered by tree tip labels
    dep <- map_dbl(tree$tip.label, ~ d[d$Label_BirdTree==., var][[1]])
    names(dep) <- tree$tip.label

    # Run anova with 1,000 simulations
    # NOTE returns unadjusted P values for simplicity of interpretation
    #      (must agument with adjusted P-value threshold during interpretation)
    aov <- phylANOVA(tree=tree, x=ind, y=dep, nsim=1000, p.adj="none")

    # Construct final combined dataframe to return
    p_overall <- aov$Pf
    p_other_cooperative <- aov$Pt["Other", "Cooperative"]
    p_other_colony <- aov$Pt["Other", "Colony"]
    p_other_lek <- aov$Pt["Other", "Lek"]
    t_other_cooperative <- aov$T["Other", "Cooperative"]
    t_other_colony <- aov$T["Other", "Colony"]
    t_other_lek <- aov$T["Other", "Lek"]
    results <- tibble(Variable=var, Iteration=i, P_Overall=p_overall,
                      P_Other_Cooperative=p_other_cooperative, P_Other_Colony=p_other_colony, P_Other_Lek=p_other_lek,
                      T_Other_Cooperative=t_other_cooperative, T_Other_Colony=t_other_colony, T_Other_Lek=t_other_lek)
    return(results)   
}

# Custom function to run every iteration of
# phylogenetic ANOVAs for one variable across every tree in the tree distribution
paovVar <- function(var) {
    cat(paste("\nRunning phyANOVA", var, "~ Social_Context\n"))

    # Filter data
    d <- select(data, Label_BirdTree, Social_Context, all_of(var)) %>%
      filter(complete.cases(.))
    
    # Prune trees
    trees_pruned <- purrr::map(1:1000, ~drop.tip(trees[[.]], trees[[.]]$tip.label[!(trees[[.]]$tip.label %in% d$Label_BirdTree)]))

    # Run
    cat("Initiating multicore session.\n")
    plan("multisession")
    p <- progressor(steps=1000)
    results <- future_map_dfr(1:1000, 
                              ~{p(); paovIter(var, ., d, trees_pruned[[.]])},
                              .options = furrr_options(seed=NULL))
    plan("sequential")
    return(results)
}
  
# Run -------------------------------------

cat("Beginning runs.\n")
vars <- c("Alpha_F", "Alpha_M", 
          "Alpha_Log_F", "Alpha_Log_M",
          "Min_Alpha_F", "Min_Alpha_M",
          "Min_Alpha_Log_F", "Min_Alpha_Log_M",
          "Alpha_Diff", "Min_Alpha_Diff")
results <- map_df(vars, paovVar)

cat("Done with runs. Saving all results to Output/results_phyanova.csv\n")
write_csv(results, "Output/results_phyanova.csv")

warnings()
cat("Exiting.\n")