# Evolution of delayed reproduction in birds
# R script to run hidden-state discrete markov models
#   using the corHMM package
# OUTPUT: Writes Output/results_corhmm.csv
# L.U.T. updated 2023-07-19

# Logistics -------------------------------------

# Required packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(furrr))
suppressPackageStartupMessages(library(ape))
suppressPackageStartupMessages(library(corHMM))
suppressPackageStartupMessages(library(progressr))

# Number of random restarts per model run
N_STARTS <- 10

handlers(global=TRUE)
handlers("progress")

# Read in target variable
args <- commandArgs(trailing=TRUE)
VAR <- args[1]

# Organizing data -------------------------------------

cat("Organizing data.\n")

# Read in data and trees
data <- read_tsv("Data/data_clean.tsv", show_col_types=FALSE)
trees <- read.nexus("Data/BirdTree_1000_Trees.nex")

# Custom function to map numeric states to a character
binStates <- function(states) {    
    if (states <= 1) { return ("A1") }
    else if (states == 2) { return ("B2")}
    return("C3More")
}

# Models -------------------------------------

# Custom function to flatten corHMM transition rate solutions
# Returns as single-row dataframe with one rate per column,
# which can then be unnested separately
# to bind as columns of a larger dataframe
flattenTransitionMat <- function(model) {
    flat <- model$solution |>
         as.data.frame() %>%
         mutate(State_1=row.names(.), .before=1) |>
         pivot_longer(-State_1, names_to="State_2", values_to="Rate") |>
         unite("Transition", State_1, State_2, sep="->") |>
         pivot_wider(names_from=Transition, values_from=Rate)
    return(flat)
}

# Custom function to run a single iteration of 
# corHMM models for one variable on one tree from the tree distribution,
# including H1-Independent, H1-Dependent, H2-Independent, and H2-Dependent Models
corhmmIter <- function(var, i, d, tree) {

    # Initialize social data, mapping as a vector ordered by tree tip labels
    social <- map_chr(tree$tip.label, ~d[d$Label_BirdTree==.,"Social_Context"][[1]])
    
    # Simplify social characters into only Social and Other
    # using 2Social and 1Other labels so 1Other is always ordered before 2Social 
    # in corHMM model construction
    social <- ifelse(social%in%c("Cooperative", "Colony", "Lek"), "2Social", "1Other")

    # Initialize dependent variable, mapping as a vector ordered by tree tip labels
    # whichever age variable is passed to the function
    #   (e.g., "Alpha_F" or "Alpha_Log_M")
    dep <- map_chr(tree$tip.label, ~binStates(d[d$Label_BirdTree==., var][[1]]))

    # Combine dataframe for corHMM input
    d_sorted <- data.frame(Label=tree$tip.label, dep=dep, Social=social)
    colnames(d_sorted)[2] <- var

    # H1-Independent
    # independent transitions, one hidden state
    # Disallow dual shifts and drop non-stepwise transitions (one<->three)
    sm_1 <- getStateMat4Dat(data=d_sorted, model="ARD", dual=FALSE, indep=TRUE)$rate.mat |>
         dropStateMatPars(c(2, 5))
    model_onerate_independent <- corHMM(phy=tree, data=d_sorted, 
                                        node.states="none", nstarts=N_STARTS, n.cores=1, 
                                        rate.cat=1, rate.mat=sm_1)

    # H1-Dependent
    # age depends on social, one hidden state
    # Disallow dual shifts and drop non-stepwise transitions (one<->three)
    # Equate social shifts (social<->other) across age states
    sm_2 <- getStateMat4Dat(data=d_sorted, model="ARD", dual=FALSE, indep=FALSE)$rate.mat |>
         dropStateMatPars(c(3,6,13,16)) |>
         equateStateMatPars(list(c(3,9,14), c(1,6,12)))
    model_onerate_dependent <- corHMM(phy=tree, data=d_sorted, 
                                      node.states="none", nstarts=N_STARTS, n.cores=1, 
                                      rate.cat=1, rate.mat=sm_2)

    # H2-Independent
    # hidden state heterogenity in age
    # Set Social<->Other transitions the same ACROSS rate matrices,
    # Need to equate states 1->2/3->4/5->6 (#6 in H1, #12 in H2) and 2->1/4->3/6->5 (#5 in H1, #11 in H2)
    sm_3_A <- getStateMat4Dat(data=d_sorted, model="ARD", dual=FALSE, indep=TRUE)$rate.mat |>
        dropStateMatPars(c(2,5))
    sm_3_B <- getStateMat4Dat(data=d_sorted, model="ARD", dual=FALSE, indep=TRUE)$rate.mat |>
        dropStateMatPars(c(2,5))
    rm_3 <- getRateCatMat(2)
    mats_3 <- getFullMat(list(sm_3_A, sm_3_B), rm_3) |>
           equateStateMatPars(list(c(6,12), c(5,11)))
    model_tworate_independent <- corHMM(phy=tree, data=d_sorted, 
                                        node.states="none", nstarts=N_STARTS, n.cores=1, 
                                        rate.cat=2, rate.mat=mats_3)

    # H2-Dependent
    # hidden state and social heterogenity in age
    # Disallow dual shifts and drop non-stepise transitions (one<->three)
    # Equate social shifts (social<->other) across age states
    # Set Social<->Other transitions the same ACROSS rate matrices,
    # Need to equate state 1->2/3->4/5->6 (#9 in H1, #19 in H2) and 2->1/4->3/6->5 (#10 in H1, #20 in H2)
    sm_4_A <- getStateMat4Dat(data=d_sorted, model="ARD", dual=FALSE, indep=FALSE)$rate.mat |>
           dropStateMatPars(c(3,6,13,16)) |>
           equateStateMatPars(list(c(3,9,14), c(1,6,12)))
    sm_4_B <- getStateMat4Dat(data=d_sorted, model="ARD", dual=FALSE, indep=FALSE)$rate.mat |>
           dropStateMatPars(c(3,6,13,16)) |>
           equateStateMatPars(list(c(3,9,14), c(1,6,12)))
    rm_4 <- getRateCatMat(2)
    mats_4 <- getFullMat(list(sm_4_A, sm_4_B), rm_4) |>
           equateStateMatPars(list(c(9,19), c(10,20)))
    model_tworate_dependent <- corHMM(phy=tree, data=d_sorted, 
                                      node.states="none", nstarts=N_STARTS, n.cores=1, 
                                      rate.cat=2, rate.mat=mats_4)
    
    # Organize results, always in order: 
    # H1-Independent, H1-Dependent, H2-Independent, H2-Dependent
    models <- list(model_onerate_independent, model_onerate_dependent, 
                   model_tworate_independent, model_tworate_dependent)
    t1HiddenStates <- c(1,1,2,2)
    t2HiddenStates <- c(1,1,1,1)
    dependence <- c("Independent", "Age|Social", "Independent", "Age|Social")                

    # Construct final combined dataframe to return
    results <- tibble(Variable=var, 
                      HiddenStates_T1=t1HiddenStates,
                      HiddenStates_T2=t2HiddenStates,
                      Dependence=dependence,
                      Iteration=i,
                      AIC=map_dbl(models, ~.$AIC),
                      AICc=map_dbl(models, ~.$AICc),
                      Rate_Categories=map_dbl(models, ~.$rate.cat),
                      Parameters=map_df(models, flattenTransitionMat)) |>
            unnest(Parameters)
    return(results)
}

# Custom function to run every iteration of
# corHMM models for one variable across every tree in the tree distribution
corhmmVar <- function(var) {
    cat(paste("Running corHMM models for", var, "across all trees\n"))

    # Filter data
    d <- select(data, Label_BirdTree, Social_Context, all_of(var)) %>%
        filter(complete.cases(.))

    # Prune trees
    trees_pruned <- purrr::map(1:1000, ~drop.tip(trees[[.]], trees[[.]]$tip.label[!(trees[[.]]$tip.label %in% d$Label_BirdTree)]))

    # Run
    cat ("Initializing multicore session.\n")
    plan("multisession")
    p <- progressor(steps=1000)
    results <- future_map_dfr(1:1000, 
                              ~{p(); corhmmIter(var, ., d, trees_pruned[[.]])},
                              .options=furrr_options(seed=NULL))
    plan("sequential")
    return(results)
}

# Run -------------------------------------

cat(paste0("Beginning run with variable", VAR, "\n"))
results <- corhmmVar(VAR)

of <- paste0("Output/results_corhmm_", VAR, ".csv")
cat(paste("Done with runs. Saving all results to", of, "\n"))
write_csv(results, of)

warnings()
cat("Exiting.\n")
