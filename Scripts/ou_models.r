# Evolution of delayed reproduction in birds
# R script to run continuous OU models
#   using the OUwie package
# OUTPUT: Writes Output/results_ou_models.csv
# L.U.T. updated 2023-07-19

# Logistics -------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(furrr))
suppressPackageStartupMessages(library(ape))
suppressPackageStartupMessages(library(OUwie))
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
# OUwie models for one variable on one tree from the tree distribution,
# including BM, OU1, OU2-Cooperative, OU2-Colony, OU2-Lek, OU2-Social, and OU4-Social
ouIter <- function(var, i, d, tree) {

    # Initialize social data, mapping as a vector ordered by tree tip labels
    social <- map_chr(tree$tip.label, ~ d[d$Label_BirdTree==., "Social_Context"][[1]])
    names(social) <- tree$tip.label

    # Ancestral state estimation of the social state for internal nodes
    # on the tree, for use in assigning social regimes to internal nodes
    # in multi-regime OU models
    ase_social <- ape::ace(x=social, phy=tree, type="discrete", method="ML", model="SYM") %>%
               .$lik.anc |>
               as.data.frame() %>%
               mutate(node = rownames(.)) |>
               pivot_longer(-node, names_to="Social_Context", values_to="LK") |>
               group_by(node) |>
               slice_max(LK) |>
               mutate(node=as.numeric(node)) |>
               arrange(node) |>
               pull(Social_Context)

    # Label the internal tree nodes with the social regimes
    tree$node.label <- ase_social

    # Run BM1
    bm <- OUwie::OUwie(phy=tree, data=d, model="BM1", algorithm="three.point", quiet=TRUE)

    # Run OU1
    ou <- OUwie::OUwie(phy=tree, data=d, model="OU1", algorithm="three.point", quiet=TRUE)

    # Run OU2-Cooperative
    # simplifying social context to COOPERATIVE or OTHER in both data and ancestral states
    temp_d <- d |>
           mutate(Social_Context = ifelse(Social_Context!="Cooperative", "Other", "Cooperative")) |>
           as.data.frame()
    temp_tree <- tree
    temp_tree$node.label[temp_tree$node.label!="Cooperative"] <- "Other"
    ou_cooperative <- OUwie::OUwie(phy=temp_tree, data=temp_d, model="OUM", algorithm="three.point", quiet=TRUE)

    # Run OU2-Colony
    # simplifying social context to COLONY or OTHER in both data and ancestral states
    temp_d <- d |>
           mutate(Social_Context = ifelse(Social_Context!="Colony", "Other", "Colony")) |>
           as.data.frame()
    temp_tree <- tree
    temp_tree$node.label[temp_tree$node.label!="Colony"] <- "Other"
    ou_colony <- OUwie::OUwie(phy=temp_tree, data=temp_d, model="OUM", algorithm="three.point", quiet=TRUE)

    # Run OU2-Lek
    # simplifying social context to LEK or OTHER in both data and ancestral states
    temp_d <- d |>
           mutate(Social_Context = ifelse(Social_Context!="Lek", "Other", "Lek")) |>
           as.data.frame()
    temp_tree <- tree
    temp_tree$node.label[temp_tree$node.label!="Lek"] <- "Other"
    ou_lek <- OUwie::OUwie(phy=temp_tree, data=temp_d, model="OUM", algorithm="three.point", quiet=TRUE)

    # Run OU2-Social
    # simplifying social context to SOCIAL or OTHER in both data and ancestral states
    temp_d <- d |>
           mutate(Social_Context = ifelse(Social_Context!="Other", "Social", "Other")) |>
           as.data.frame()
    temp_tree <- tree
    temp_tree$node.label[temp_tree$node.label!="Other"] <- "Social"
    ou_socialTogether <- OUwie::OUwie(phy=temp_tree, data=temp_d, model="OUM", algorithm="three.point", quiet=TRUE)

    # Run OU4-Social
    temp_d <- d |>
           as.data.frame()
    temp_tree <- tree
    ou_socialSeparate <- OUwie::OUwie(phy=temp_tree, data=temp_d, model="OUM", algorithm="three.point", quiet=TRUE)

    # Construct final combined dataframe to return
    models <- list(bm, ou, ou_cooperative, ou_colony, ou_lek, ou_socialTogether, ou_socialSeparate)
    names(models) <- c("BM", "OU", "OUM-Cooperative", "OUM-Colony", "OUM-Lek", "OUM-Social-Together", "OUM-Social-Separate")
    results <- tibble(Variable=var,
                      Model=names(models),
                      Iteration=i,
                      AIC=map_dbl(models, ~.$AIC),
                      BIC=map_dbl(models, ~.$BIC),
                      Alpha=map_dbl(models, ~.$solution["alpha",1]),
                      Sigma_Sq=map_dbl(models, ~.$solution["sigma.sq",1]),
                      Theta_1=map_dbl(models, ~.$solution["theta",1]),
                      Theta_2=map_dbl(models, ~ifelse(length(.$solution["theta",])>1,.$solution["theta",2],NA)),
                      Theta_3=map_dbl(models, ~ifelse(length(.$solution["theta",])>2,.$solution["theta",3],NA)),
                      Theta_4=map_dbl(models, ~ifelse(length(.$solution["theta",])>3,.$solution["theta",4],NA)),
                      Theta_Names=map_chr(models, ~paste(names(.$solution["theta",]), collapse=";")))
    return(results)
}

# Custom function to run every iteration of
# continuous OU models for one variable across every tree in the tree distribution
ouVar <- function(var) {
    cat(paste("Estimating BM/OU parameters for", var, "across all trees.\n"))

    # Filter data
    d <- select(data, Label_BirdTree, Social_Context, all_of(var)) %>%
      filter(complete.cases(.)) |>
      as.data.frame()

    # Prune trees
    trees_pruned <- purrr::map(1:1000, ~drop.tip(trees[[.]], trees[[.]]$tip.label[!(trees[[.]]$tip.label %in% d$Label_BirdTree)]))

    # Run
    cat("Initiating multicore session.\n")
    plan("multisession")
    p <- progressor(steps=1000)
    results <- future_map_dfr(1:1000, 
                              ~{p(); ouIter(var, ., d, trees_pruned[[.]])},
                              .options = furrr_options(seed=NULL))
    plan("sequential")
    return(results)
}

# Run -------------------------------------
cat("Beginning run.\n")

vars <- c("Alpha_F", "Alpha_M", 
          "Alpha_Log_F", "Alpha_Log_M",
          "Min_Alpha_F", "Min_Alpha_M",
          "Min_Alpha_Log_F", "Min_Alpha_Log_M")
results <- map_df(vars, ~ouVar(.))

# Save results
cat("Done with runs. Saving results to Output/results_ou_models.csv\n")
write_csv(results, "Output/results_ou_models.csv")

warnings()
cat("Exiting.\n")