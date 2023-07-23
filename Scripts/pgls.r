# Evolution of delayed reproduction in birds
# R script to run PGLS models
#   using the nlme package
# OUTPUT: Writes Output/results_pgls.csv
# L.U.T. updated 2023-07-19

# NOTE must have completed OU model estimates
#   to obtain parameters used in Martin's correlation structure
#   (see Scripts/2_ou_models.sh)

# Logistics -------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(furrr))
suppressPackageStartupMessages(library(ape))
suppressPackageStartupMessages(library(nlme))
suppressPackageStartupMessages(library(broom.mixed))
suppressPackageStartupMessages(library(progressr))

handlers(global=TRUE)
handlers("progress")

# Organizing data -------------------------------------

cat("Organizing data.\n")

# Read in data and trees
data <- read_tsv("Data/data_clean.tsv", show_col_types=FALSE)
trees <- read.nexus("Data/BirdTree_1000_Trees.nex")

# Read in data and trees
parameters <- read_csv("Output/results_ou_models.csv", show_col_types=FALSE)

# Models -------------------------------------

# Custom function to extract the name of a correlation 
# structure used in a PGLS model fit, returning it as a string
getCorType <- function(model) {
    s <- capture.output(model$modelStruct$corStruct) |>
      str_split(" ") %>%
      .[[1]]
    return(s[grep("class",s)+1])
}

# Custom function to tidy and return key model results
# from a PGLS model fit
getModelResult <- function(model, term, type) {
    res <- tidy(model)
    if (!(term %in% res$term)) {return(NA)}
    value <- res[res$term==term,type][[1]]
    return(value)
}

# Custom function to run a single iteration of 
# PGLS models for one variable on one tree from the tree distribution,
# including Social, Mass, Social+Mass, and Social*Mass models
# for both corBrownian and corMartin's correlation structures
pglsIter <- function(var, i, d, tree) {

    # Initialize social data, mapping as a vector ordered by tree tip labels
    social <- map_chr(tree$tip.label, ~d[d$Label_BirdTree==.,"Social_Context"]) |>
        factor(levels = c("Other", "Cooperative", "Colony", "Lek"))
    names(social) <- tree$tip.label

    # Initialize dependent variable (age)
    dep <- map_dbl(tree$tip.label, ~d[d$Label_BirdTree==.,var])
    names(dep) <- tree$tip.label

    # Initialize mass data, including Log10 transform
    massLog10 <- map_dbl(tree$tip.label, ~log10(d[d$Label_BirdTree==.,"Mass"]))
    names(massLog10) <- tree$tip.label

    # corBrownian models
    pgls_social_bm <- nlme::gls(dep ~ social, 
                                correlation=corBrownian(phy=tree, form=~names(dep)))

    pgls_mass_all_bm <- nlme::gls(dep ~ massLog10, 
                                  correlation=corBrownian(phy=tree, form=~names(dep)))

    # Mass regression, cooperative taxa only
    pgls_mass_cooperative_bm <- nlme::gls(dep ~ massLog10, 
                                          correlation=corBrownian(phy=tree, form=~names(dep)),
                                          subset=social=="Cooperative")

    # Mass regression, colony taxa only
    pgls_mass_colony_bm <- nlme::gls(dep ~ massLog10, 
                                     correlation=corBrownian(phy=tree, form=~names(dep)),
                                     subset=social=="Colony")

    # Mass regression, lek taxa only
    pgls_mass_lek_bm <- nlme::gls(dep ~ massLog10, 
                                  correlation=corBrownian(phy=tree, form=~names(dep)),
                                  subset=social=="Lek")

    pgls_socialPlusMass_bm <- nlme::gls(dep ~ social + massLog10, 
                                        correlation=corBrownian(phy=tree, form=~names(dep)))

    pgls_socialTimesMass_bm <- nlme::gls(dep ~ social * massLog10, 
                                        correlation=corBrownian(phy=tree, form=~names(dep)))

    # corMartins models

    # get OU parameter estimates for the variable and tree, to initialize values
    parameterAlpha <- parameters |>
                   filter(Variable==var, Iteration==i, Model=="OU") |>
                   pull(Alpha)

    pgls_social_ou <- nlme::gls(dep ~ social, 
                                correlation=corMartins(value=parameterAlpha, phy=tree, form=~names(dep), fixed=TRUE))

    pgls_mass_all_ou <- nlme::gls(dep ~ massLog10, 
                                  correlation=corMartins(value=parameterAlpha, phy=tree, form=~names(dep), fixed=TRUE))

    # Mass regression, cooperative taxa only
    pgls_mass_cooperative_ou <- nlme::gls(dep ~ massLog10, 
                                          correlation=corMartins(value=parameterAlpha, phy=tree, form=~names(dep), fixed=TRUE),
                                          subset=social=="Cooperative")
 
    # Mass regression, colony taxa only
    pgls_mass_colony_ou <- nlme::gls(dep ~ massLog10, 
                                     correlation=corMartins(value=parameterAlpha, phy=tree, form=~names(dep), fixed=TRUE),
                                     subset=social=="Colony")

    # Mass regression, lek taxa only
    pgls_mass_lek_ou <- nlme::gls(dep ~ massLog10, 
                                  correlation=corMartins(value=parameterAlpha, phy=tree, form=~names(dep), fixed=TRUE),
                                  subset=social=="Lek")

    pgls_socialPlusMass_ou <- nlme::gls(dep ~ social + massLog10, 
                                        correlation=corMartins(value=parameterAlpha, phy=tree, form=~names(dep), fixed=TRUE))

    pgls_socialTimesMass_ou <- nlme::gls(dep ~ social * massLog10, 
                                         correlation=corMartins(value=parameterAlpha, phy=tree, form=~names(dep), fixed=TRUE))

    # Organize results, always in order:
    # BM-Social, BM-Mass, BM-Mass(Cooperative), BM-Mass(Colony), BM-Mass(Lek), BM-Social+Mass, BM-Social*Mass
    # OU-Social, OU-Mass, OU-Mass(Cooperative), OU-Mass(Colony), OU-Mass(Lek), OU-Social+Mass, OU-Social*Mass
    models <- list(pgls_social_bm, 
                   pgls_mass_all_bm, pgls_mass_cooperative_bm, pgls_mass_colony_bm, pgls_mass_lek_bm,
                   pgls_socialPlusMass_bm, pgls_socialTimesMass_bm,
                   pgls_social_ou, 
                   pgls_mass_all_ou, pgls_mass_cooperative_ou, pgls_mass_colony_ou, pgls_mass_lek_ou,
                   pgls_socialPlusMass_ou, pgls_socialTimesMass_ou)
    modelTypes <- c("Social", 
                    "Mass-All", "Mass-Cooperative", "Mass-Colony", "Mass-Lek",
                    "Social+Mass", "Social*Mass",
                    "Social", 
                    "Mass-All", "Mass-Cooperative", "Mass-Colony", "Mass-Lek",
                    "Social+Mass", "Social*Mass")     

    # Construct final combined dataframe to return
    results <- tibble(Variable=var,
                      Model=modelTypes,
                      Correlation=map_chr(models, getCorType),
                      Iteration=i,
                      AIC=map_dbl(models, ~broom.mixed::glance(.)$AIC[[1]]),
                      BIC=map_dbl(models, ~broom.mixed::glance(.)$BIC[[1]]),
                      Estimate_Intercept=map_dbl(models, ~getModelResult(.,"(Intercept)","estimate")),
                      Estimate_Cooperative=map_dbl(models, ~getModelResult(.,"socialCooperative","estimate")),
                      Estimate_Colony=map_dbl(models, ~getModelResult(.,"socialColony","estimate")),
                      Estimate_Lek=map_dbl(models, ~getModelResult(.,"socialLek","estimate")),          
                      Estimate_Mass=map_dbl(models, ~getModelResult(.,"massLog10","estimate")),          
                      Estimate_Interaction_Cooperative_Mass=map_dbl(models, ~getModelResult(.,"socialCooperative:massLog10","estimate")),          
                      Estimate_Interaction_Colony_Mass=map_dbl(models, ~getModelResult(.,"socialColony:massLog10","estimate")),          
                      Estimate_Interaction_Lek_Mass=map_dbl(models, ~getModelResult(.,"socialLek:massLog10","estimate")),          
                      P_Intercept=map_dbl(models, ~getModelResult(.,"(Intercept)","p.value")),
                      P_Cooperative=map_dbl(models, ~getModelResult(.,"socialCooperative","p.value")),
                      P_Colony=map_dbl(models, ~getModelResult(.,"socialColony","p.value")),
                      P_Lek=map_dbl(models, ~getModelResult(.,"socialLek","p.value")),          
                      P_Mass=map_dbl(models, ~getModelResult(.,"massLog10","p.value")),          
                      P_Interaction_Cooperative_Mass=map_dbl(models, ~getModelResult(.,"socialCooperative:massLog10","p.value")),          
                      P_Interaction_Colony_Mass=map_dbl(models, ~getModelResult(.,"socialColony:massLog10","p.value")),          
                      P_Interaction_Lek_Mass=map_dbl(models, ~getModelResult(.,"socialLek:massLog10","p.value")))
    return(results)
}

# Custom function to run every iteration of
# PGLS models for one variable across every tree in the tree distribution
pglsVar <- function(var) {   
    cat(paste("Fitting PGLS models for", var, "across all trees.\n"))

    # Filter data
    d <- select(data, Label_BirdTree, Social_Context, all_of(var), Mass) %>%
      filter(complete.cases(.)) |>
      as.data.frame()

    # Prune trees
    trees_pruned <- purrr::map(1:1000, ~drop.tip(trees[[.]], trees[[.]]$tip.label[!(trees[[.]]$tip.label %in% d$Label_BirdTree)]))

    # Run
    cat("Initiating multicore session.\n")
    plan("multisession")
    p <- progressor(steps=1000)
    results <- future_map_dfr(1:1000, 
                              ~{p(); pglsIter(var, ., d, trees_pruned[[.]])},
                              .options=furrr_options(seed=NULL))
    plan("sequential")
    return(results)
}

# Run -------------------------------------

cat("Beginning runs.\n")
vars <- c("Alpha_F", "Alpha_M", 
          "Alpha_Log_F", "Alpha_Log_M",
          "Min_Alpha_F", "Min_Alpha_M",
          "Min_Alpha_Log_F", "Min_Alpha_Log_M")
results <- map_df(vars, pglsVar)

cat("Done with runs. Saving all results to Output/results_pgls.csv\n")
write_csv(results, "Output/results_pgls.csv")

warnings()
cat("Exiting.\n")