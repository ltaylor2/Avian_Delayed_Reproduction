# Evolution of delayed reproduction in birds
# R script to generate MS tables and figures 
# OUTPUT: Writes Plots/treeplot_Alpha_M.png (for use in Fig. 1)
#                Plots/treeplot_Alpha_F.png (for use in Fig. S1)
#                Output/TABLE_1.tsv (includes results for Table S2)
#                Output/TABLE_S1.tsv
#                Output/TABLE_2.tsv (includes results for Table S3)
#                Output/TABLE_3.tsv (includes results for Table S4)
#                Output/TABLE_S5.tsv
#                Output/TABLE_S6.tsv
#                Output/TABLE_SIII* (files for Appendix III tables)
# L.U.T. updated 2023-07-13

# Logistics -------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ape))
suppressPackageStartupMessages(library(ggtree))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(corHMM))

theme_lt <- theme_bw() +
         theme(panel.background=element_blank(),
               panel.grid=element_blank(),
               axis.title=element_text(size=10),
               axis.text=element_text(size=8),
               legend.title=element_text(size=10),
               legend.text=element_text(size=8))

data <- read_tsv("Data/data_clean.tsv", show_col_types=FALSE)

colours_alpha <- c("1"="#ebebeb",
                   "2"="#a0a0a0",
                   "3+"="black")

colours_social <- c("Cooperative"="#5f410b",
                    "Colony"="#3b3b9afd", 
                    "Lek"="#b64747", 
                    "Other"="gray")

colours_pgls <- c("Mass-Cooperative"=colours_social[["Cooperative"]],
                  "Mass-Colony"=colours_social[["Colony"]],
                  "Mass-Lek"=colours_social[["Lek"]],
                  "Mass-All"="black")

# Tables and Figures -------------------------------------

# # FIGURES 1,S1----------------------------------- 
# # Tree examples
# # FIGURES 1,S1 ----------------------------------- 

# # Custom function to map numeric states to a character
# binStates <- function(state) {    
#     if (state <= 1) { return ("A1") }
#     else if (state == 2) { return ("B2")}
#     return("C3More")
# }
# unbinStates <- function(state) {
#     if (state == "A1") { return ("1") }
#     else if (state == "B2") { return ("2")}
#     else if (state == "C3More") { return("3+") }
#     return("ERROR")
# }
# unmapCorHMMStates <- function(state) {
#     if (state == 1 | state == 2) { return("A1") } 
#     else if (state == 3 | state == 4) { return("B2") }
#     else if (state == 5 | state == 6) { return("C3More") } 
#     return("ERROR")
# }

# plotAlphaTree <- function(var=c("Alpha_F", "Alpha_M")) {
#     # Read in example tree for plotting
#     if (var == "Alpha_F") {
#         treeFile <- "Data/randomMainTree_F.nex"
#     } else if (var == "Alpha_M") {
#         treeFile <- "Data/randomMainTree_M.nex"
#     }
#     tree <- read.nexus(treeFile)
    
#     # Prepare data for corHMM ancestral state reconstruction
#     d <- read_tsv("Data/data_clean.tsv", show_col_types=FALSE)
#     social <- map_chr(tree$tip.label, ~d[d$Label_BirdTree==.,"Social_Context"][[1]])
#     names(social) <- tree$tip.label
#     binnedSocial <- ifelse(social%in%c("Cooperative", "Colony", "Lek"), "2Social", "1Other")
#     names(binnedSocial) <- tree$tip.label
#     dep <- map_chr(tree$tip.label, ~binStates(d[d$Label_BirdTree==., var][[1]]))
#     names(dep) <- tree$tip.label
#     d_sorted <- data.frame(Label=tree$tip.label, dep=dep, Social=binnedSocial)
#     colnames(d_sorted)[2] <- var

#     # Use corHMM for ancestral state reconstruction under the "independent"
#     #   3 state age model
#     sm_1 <- getStateMat4Dat(data=d_sorted, model="ARD", dual=FALSE, indep=TRUE)$rate.mat |>
#         dropStateMatPars(c(2, 5))
#     model_onerate_independent <- corHMM(phy=tree, data=d_sorted, 
#                                         node.states="joint", nstarts=1, n.cores=1, 
#                                         rate.cat=1, rate.mat=sm_1)

#     # Extract node ancestral states from the corHMM results 
#     ases <- tibble(Node_Alpha=model_onerate_independent$states) |>
#          mutate(Node_Alpha = map_chr(Node_Alpha, unmapCorHMMStates)) |>
#          mutate(node = row_number() + length(tree$tip.label), .before=1)
    
#     gtree <- ggtree(tree, layout="fan", aes(colour=Alpha), linewidth=0.3)
#     gtree$data <- gtree$data |>
#                mutate(Tip_Social_Context=social[label]) |>
#                mutate(Tip_Alpha=dep[label]) |>
#                left_join(ases, by="node") |>
#                left_join(d, by=c("label"="Label_BirdTree")) |>
#                mutate(Alpha=ifelse(isTip, Tip_Alpha, Node_Alpha)) |>
#                mutate(Alpha = map_chr(Alpha, unbinStates))

#     # Source the phylopic image data
#     source("Data/tree_images.r")
#     orderLabels <- getOrderLabels(gtree, d, tree)

#     plot <- gtree + 
#         geom_tippoint(data=filter(gtree$data, Tip_Social_Context != "Other"), 
#                       aes(fill=Tip_Social_Context), 
#                       position=position_nudge(x=3),
#                       size=0.8, colour="transparent", shape=21) +
#         geom_cladelab(data=orderLabels,
#                       geom="phylopic",
#                       mapping=aes(node=Parent_Node, 
#                                   label=Blank, 
#                                   image=Phylo_ID),
#                       barcolour=NA,
#                       imagesize=orderLabels$Image_Size,
#                       imagecolour="black",
#                       alpha=0.3,
#                       offset=18) +
#         scale_colour_manual(values=colours_alpha) +
#         scale_fill_manual(values=colours_social) +
#         guides(colour="none", fill="none")
#     return(plot)
# }

# treePlot_F <- plotAlphaTree("Alpha_F")
# ggsave(treePlot_F, file="Plots/treeplot_Alpha_F.png", width=5, height=5)

# treePlot_M <- plotAlphaTree("Alpha_M")
# ggsave(treePlot_M, file="Plots/treeplot_Alpha_M.png", width=5, height=5)

# TABLE 1 ----------------------------------- 
# Continuous evolutionary model comparisons
# TABLE 1 ----------------------------------- 
ouModelOrder <- c("BM", "OU", 
                  "OUM-Cooperative", "OUM-Colony", "OUM-Lek",
                  "OUM-Social-Together", "OUM-Social-Separate")

TABLE_1 <- read_csv("Output/results_ou_models.csv", show_col_types=FALSE) |>
        mutate(Sex = substr(Variable, nchar(Variable), nchar(Variable))) |>
        mutate(Model = factor(Model, levels=ouModelOrder)) |>
        group_by(Variable, Iteration) |>
        mutate(Delta_BIC = BIC - min(BIC)) |>
        group_by(Variable, Model, Sex) |>
        summarize(.groups="keep",
                  Median_BIC=median(BIC),
                  Low_BIC=quantile(BIC, 0.05),
                  High_BIC=quantile(BIC, 0.95),
                  Median_Delta_BIC=median(Delta_BIC),
                  Low_Delta_BIC=quantile(Delta_BIC, 0.05),
                  High_Delta_BIC=quantile(Delta_BIC, 0.95)) |>
        mutate(Median_Delta_BIC=round(Median_Delta_BIC,1),
               Quantile_Delta_BIC = paste0("(", round(Low_Delta_BIC), ", ", round(High_Delta_BIC), ")")) |>
        select(Variable, Model, Sex, Median_Delta_BIC, Quantile_Delta_BIC) |>
        pivot_wider(id_cols=c(Variable, Model), names_from=Sex, values_from=c(Median_Delta_BIC, Quantile_Delta_BIC)) |>
        select(Variable, Model, 
               Median_Delta_BIC_F, Quantile_Delta_BIC_F,
               Median_Delta_BIC_M, Quantile_Delta_BIC_M)

write_tsv(TABLE_1, "Output/TABLE_1.tsv")

# TABLE S1 ----------------------------------- 
# Main continuous evolutionary model parameters
# TABLE S1 ----------------------------------- 
TABLE_S1 <- read_csv("Output/results_ou_models.csv", show_col_types=FALSE) |>
         filter(Variable %in% c("Alpha_Log_F", "Alpha_Log_M")) |>
         mutate(Model = factor(Model, levels=ouModelOrder)) |>
         pivot_longer(cols=Theta_1:Theta_4, names_to="Theta_Num", values_to="Theta_Value") |>
         mutate(Theta_Num = map_dbl(Theta_Num, ~as.numeric(str_split_1(., "_")[2]))) |>
         mutate(Theta_Names = ifelse(is.na(Theta_Names), "All", Theta_Names)) |>
         mutate(Theta_Name = map2_chr(Theta_Num, Theta_Names, ~ str_split_1(.y,";")[.x])) |>
         group_by(Variable, Model, Theta_Name) |>
         summarize(.groups="keep",
                   Median_Alpha=median(Alpha),
                   Low_Alpha=quantile(Alpha, 0.05, na.rm=TRUE),
                   High_Alpha=quantile(Alpha, 0.95, na.rm=TRUE),
                   Median_Sigma_Sq=median(Sigma_Sq),
                   Low_Sigma_Sq=quantile(Sigma_Sq, 0.05, na.rm=TRUE),
                   High_Sigma_Sq=quantile(Sigma_Sq, 0.95, na.rm=TRUE),                       
                   Median_Theta=median(Theta_Value),
                   Low_Theta=quantile(Theta_Value, 0.05, na.rm=TRUE),
                   High_Theta=quantile(Theta_Value, 0.95, na.rm=TRUE)) |>
         mutate(Alpha = paste0(round(Median_Alpha,2)," (",round(Low_Alpha,2),", ",round(High_Alpha,2),")"),
                Sigma_Sq = paste0(round(Median_Sigma_Sq,2)," (",round(Low_Sigma_Sq,2),", ",round(High_Sigma_Sq,2),")"),         
                Theta_Value = paste0(round(Median_Theta,2)," (",round(Low_Theta,2),", ",round(High_Theta,2),")")) |>
         pivot_wider(id_cols=c(Variable, Model, Alpha, Sigma_Sq), names_from=Theta_Name, values_from=Theta_Value) |>
         ungroup() |>
         mutate(across(Alpha:Social, ~ifelse(is.na(.), "", .))) |>
         select(Variable, Model, Alpha, Sigma_Sq, All, Other, Cooperative, Colony, Lek, Social) |>
         arrange(Variable, Model) 

write_tsv(TABLE_S1, "Output/TABLE_S1.tsv")

# TABLE 2 ----------------------------------- 
# Discrete evolutionary model comparisons
# TABLE 2 ----------------------------------- 
classifyModel <- function(hidden1, dependence) {
    if (hidden1==1 & dependence == "Independent") {
        return("H1-Independent")
    } else if (hidden1==1 & dependence == "Age|Social") {
        return("H1-Dependent")
    } else if (hidden1==2 & dependence == "Independent") {
        return("H2-Independent") 
    } else if (hidden1==2 & dependence == "Age|Social") {
        return("H2-Dependent")
    }
    return("ERROR")
}

discreteModelOrder <- c("H1-Independent", "H1-Dependent",
                        "H2-Independent", "H2-Dependent")
TABLE_2 <- read_csv("Output/results_corhmm.csv", show_col_types=FALSE) |> 
        mutate(Model = map2_chr(HiddenStates_T1, Dependence, classifyModel)) |>
        mutate(Model = factor(Model, levels=discreteModelOrder)) |>
        select(Variable, Model, Iteration, AIC, AICc) |>
        mutate(Sex = substr(Variable, nchar(Variable), nchar(Variable))) |>
        group_by(Variable, Iteration) |>
        mutate(Delta_AICc = AICc - min(AICc)) |>
        group_by(Variable, Model, Sex) |>
        summarize(.groups="keep",
                  Median_AICc=median(AICc),
                  Low_AICc=quantile(AICc, 0.05),
                  High_AICc=quantile(AICc, 0.95),
                  Median_Delta_AICc=median(Delta_AICc),
                  Low_Delta_AICc=quantile(Delta_AICc, 0.05),
                  High_Delta_AICc=quantile(Delta_AICc, 0.95)) |>
        mutate(Median_Delta_AICc=round(Median_Delta_AICc,1),
               Quantile_Delta_AICc = paste0("(", round(Low_Delta_AICc), ", ", round(High_Delta_AICc), ")")) |>
        select(Variable, Model, Sex, Median_Delta_AICc, Quantile_Delta_AICc) |>
        pivot_wider(id_cols=c(Variable, Model), names_from=Sex, values_from=c(Median_Delta_AICc, Quantile_Delta_AICc)) |>
        select(Variable, Model, 
               Median_Delta_AICc_F, Quantile_Delta_AICc_F,
               Median_Delta_AICc_M, Quantile_Delta_AICc_M)
write_tsv(TABLE_2, "Output/TABLE_2.tsv")

# TABLE 3 ----------------------------------- 
# phyANOVA results
# TABLE 3 ----------------------------------- 
TABLE_3 <- read_csv("Output/results_phyanova.csv", show_col_types=FALSE) |>
        mutate(Sex = ifelse(Variable=="Alpha_Diff", "Diff", 
                            substr(Variable, nchar(Variable), nchar(Variable)))) |>
        group_by(Variable) |>
        select(-Iteration) |>
        summarize_if(is.numeric, list(Median=median, Low=function(x){quantile(x,0.05)}, High=function(x){quantile(x,0.95)})) |>
        transmute(Variable, 
                  Quantile_P_Overall=paste0("(", round(P_Overall_Low,3),", ",round(P_Overall_High,3),")"),
                  Median_T_Cooperative=round(T_Other_Cooperative_Median,3),
                  Median_P_Cooperative=round(P_Other_Cooperative_Median,3),
                  Quantile_P_Cooperative=paste0("(",round(P_Other_Cooperative_Low,3),", ",round(P_Other_Cooperative_High,3),")"),
                  Median_T_Colony=round(T_Other_Colony_Median,3),
                  Median_P_Colony=round(P_Other_Colony_Median,3),
                  Quantile_P_Colony=paste0("(", round(P_Other_Colony_Low,3),", ",round(P_Other_Colony_High,3),")"),
                  Median_T_Lek=round(T_Other_Lek_Median,3),
                  Median_P_Lek=round(P_Other_Lek_Median,3),
                  Quantile_P_Lek=paste0("(",round(P_Other_Lek_Low,3),", ",round(P_Other_Lek_High,3),")"))
write_tsv(TABLE_3, "Output/TABLE_3.tsv")

# TABLE 4 ----------------------------------- 
# PGLS results
# TABLE 4 ----------------------------------- 
modelOrder <- c("Social", "Mass-All", "Social+Mass", "Social*Mass")

TABLE_4 <- read_csv("Output/results_pgls.csv", col_types=c("cccnnnnnnnnnnnnnnnnnnnnn")) |>
        filter(Model %in% c("Mass-All", "Social", "Social+Mass", "Social*Mass")) |>
        mutate(Model = factor(Model, modelOrder)) |>
        group_by(Variable, Correlation, Iteration) |>
        mutate(Delta_BIC = BIC - min(BIC)) |>
        group_by(Variable, Model, Correlation) |>
        summarize_if(is.numeric, list(Median=median, 
                                      Low=function(x){quantile(x,0.05,na.rm=TRUE)}, 
                                      High=function(x){quantile(x,0.95,na.rm=TRUE)})) |>
        transmute(Variable, Model, Correlation,
                  Median_Delta_BIC = round(Delta_BIC_Median,1),
                  Quantile_Delta_BIC = paste0("(", round(Delta_BIC_Low,0), ", ", round(Delta_BIC_High,0),")"),
                  Median_P_Mass = round(P_Mass_Median,3),
                  Median_P_Cooperative = round(P_Cooperative_Median,3),
                  Median_P_Colony = round(P_Colony_Median,3),
                  Median_P_Lek = round(P_Lek_Median,3),
                  Median_P_Interaction_Cooperative_Mass = round(P_Interaction_Cooperative_Mass_Median,3),
                  Median_P_Interaction_Colony_Mass = round(P_Interaction_Colony_Mass_Median,3),
                  Median_P_Interaction_Lek_Mass = round(P_Interaction_Lek_Mass_Median,3)) |>
        filter(grepl("Log", Variable) & !grepl("Min", Variable) & Correlation == "corMartins") |>
        arrange(Correlation, Variable, Model)

write_tsv(TABLE_4, "Output/TABLE_4.tsv")

formatEstimates <- function(median, low, high, sf) {
    if (is.na(median)) {
        return("")
    }
    return(paste0(round(median,sf)," (",round(low,sf),", ",round(high,sf),")"))
}

TABLE_S5 <- read_csv("Output/results_pgls.csv", col_types=c("cccnnnnnnnnnnnnnnnnnnnnn")) |>
         filter(Model %in% c("Mass-All", "Social", "Social+Mass", "Social*Mass")) |>
         mutate(Model = factor(Model, levels=modelOrder)) |>
         group_by(Variable, Correlation, Iteration) |>
         mutate(Delta_BIC = BIC - min(BIC)) |>
         group_by(Variable, Model, Correlation) |>
         summarize_if(is.numeric, list(Median=median, 
                                       Low=function(x){quantile(x,0.05,na.rm=TRUE)}, 
                                       High=function(x){quantile(x,0.95,na.rm=TRUE)})) |>
         transmute(Variable, Model, Correlation,
                   Delta_BIC = pmap_chr(list(Delta_BIC_Median, Delta_BIC_Low, Delta_BIC_High,1), formatEstimates),
                   Mass = pmap_chr(list(Estimate_Mass_Median, Estimate_Mass_Low, Estimate_Mass_High,1), formatEstimates),
                   P_Mass = pmap_chr(list(P_Mass_Median, P_Mass_Low, P_Mass_High), formatEstimates,2),
                   Cooperative = pmap_chr(list(Estimate_Cooperative_Median, Estimate_Cooperative_Low, Estimate_Cooperative_High,1), formatEstimates),
                   P_Cooperative = pmap_chr(list(P_Cooperative_Median, P_Cooperative_Low, P_Cooperative_High), formatEstimates,2),
                   Colony = pmap_chr(list(Estimate_Colony_Median, Estimate_Colony_Low, Estimate_Colony_High,1), formatEstimates),
                   P_Colony = pmap_chr(list(P_Colony_Median, P_Colony_Low, P_Colony_High), formatEstimates,2),
                   Lek = pmap_chr(list(Estimate_Lek_Median, Estimate_Lek_Low, Estimate_Lek_High,1), formatEstimates),
                   P_Lek = pmap_chr(list(P_Lek_Median, P_Lek_Low, P_Lek_High), formatEstimates,2),
                   Interaction_Cooperative_Mass = pmap_chr(list(Estimate_Interaction_Cooperative_Mass_Median, Estimate_Interaction_Cooperative_Mass_Low, Estimate_Interaction_Cooperative_Mass_High,1), formatEstimates),
                   P_Interaction_Cooperative_Mass = pmap_chr(list(P_Interaction_Cooperative_Mass_Median, P_Interaction_Cooperative_Mass_Low, P_Interaction_Cooperative_Mass_High), formatEstimates,2),
                   Interaction_Colony_Mass = pmap_chr(list(Estimate_Interaction_Colony_Mass_Median, Estimate_Interaction_Colony_Mass_Low, Estimate_Interaction_Colony_Mass_High,1), formatEstimates),
                   P_Interaction_Colony_Mass = pmap_chr(list(P_Interaction_Colony_Mass_Median, P_Interaction_Colony_Mass_Low, P_Interaction_Colony_Mass_High), formatEstimates,2),
                   Interaction_Lek_Mass = pmap_chr(list(Estimate_Interaction_Lek_Mass_Median, Estimate_Interaction_Lek_Mass_Low, Estimate_Interaction_Lek_Mass_High,1), formatEstimates),
                   P_Interaction_Lek_Mass = pmap_chr(list(P_Interaction_Lek_Mass_Median, P_Interaction_Lek_Mass_Low, P_Interaction_Lek_Mass_High), formatEstimates,2)) |>
         arrange(Correlation, Variable, Model)

write_tsv(TABLE_S5, "Output/TABLE_S5.tsv")

# # TABLE S6 ----------------------------------- 
# # Mass subset PGLS results
# # TABLE S6 ----------------------------------- 
massModelOrder <- c("Mass-All", "Mass-Cooperative", "Mass-Colony", "Mass-Lek")

TABLE_S6 <- read_csv("Output/results_pgls.csv", col_types=c("cccnnnnnnnnnnnnnnnnnnnnn")) |>
         filter(grepl("Mass-", Model)) |>
         mutate(Model = factor(Model, levels=massModelOrder)) |>
         group_by(Variable, Correlation, Iteration) |>
         group_by(Variable, Model, Correlation) |>
         summarize_if(is.numeric, list(Median=median, 
                                       Low=function(x){quantile(x,0.05,na.rm=TRUE)}, 
                                       High=function(x){quantile(x,0.95,na.rm=TRUE)})) |>
         transmute(Variable, Model, Correlation,
                   Intercept = pmap_chr(list(Estimate_Intercept_Median, Estimate_Intercept_Low, Estimate_Intercept_High,2), formatEstimates),
                   Mass = pmap_chr(list(Estimate_Mass_Median, Estimate_Mass_Low, Estimate_Mass_High,2), formatEstimates),
                   P_Mass = pmap_chr(list(P_Mass_Median, P_Mass_Low, P_Mass_High), formatEstimates,2)) |>
         arrange(Correlation, Variable, Model)

write_tsv(TABLE_S6, file="Output/TABLE_S6.tsv")

# FIGURE S2 ----------------------------------- 
# PGLS plot lekking
# FIGURE S2 ----------------------------------- 
massPGLS <- read_csv("Output/results_pgls.csv", col_types=c("cccnnnnnnnnnnnnnnnnnnnnn")) |>
         filter(Model %in% c("Mass-All", "Mass-Cooperative", "Mass-Colony", "Mass-Lek")) |>
         filter(Variable %in% c("Alpha_Log_F", "Alpha_Log_M"), Correlation=="corMartins") |>
         mutate(Sex = map_chr(Variable, ~str_split_1(., "_")[3])) |>
         select(Sex, Model, Correlation, 
                P_Mass, Estimate_Intercept, Estimate_Mass)
          
summaryMassPGLS <- massPGLS |>
                group_by(Sex, Model, Correlation) |>
                summarize_if(is.numeric, list(Median=median, 
                                              Low=function(x){quantile(x,0.05,na.rm=TRUE)}, 
                                              High=function(x){quantile(x,0.95,na.rm=TRUE)}))

pglsPoints <- data |>
           select(Taxon_Clements, Social_Context, Mass, Alpha_Log_F, Alpha_Log_M) |>
           mutate(Mass_Log_10 = log10(Mass)) |>
           pivot_longer(cols=c(Alpha_Log_F, Alpha_Log_M), 
                        names_to="Variable", values_to="Alpha_Log") |>
           mutate(Sex = map_chr(Variable, ~str_split_1(., "_")[3])) %>%
           bind_rows(mutate(., Social_Context = "All")) |>
           filter(Social_Context != "Other") |>
           mutate(Model = paste0("Mass-", Social_Context)) |>
           select(Taxon_Clements, Model, Sex, Mass_Log_10, Alpha_Log) |>
           filter(!is.na(Alpha_Log))

pglsNs <- pglsPoints |>
       group_by(Model, Sex) |>
       tally()

facetLabels <- c("F" = paste0("Female (n = ",
                              pglsNs[pglsNs$Model=="Mass-Lek" & pglsNs$Sex=="F","n"],
                              " lekking taxa)"),
                 "M" = paste0("Male (n = ",
                              pglsNs[pglsNs$Model=="Mass-Lek" & pglsNs$Sex=="M","n"],
                              ")"))

pglsPLabels <- TABLE_S5 |>
            ungroup() |>
            filter(Variable %in% c("Alpha_Log_F", "Alpha_Log_M"),
                   Model=="Mass-Lek",
                   Correlation=="corMartins") |>
            mutate(Sex = map_chr(Variable, ~str_split_1(.,"_")[3])) |>
            mutate(P_Mass_Label = str_replace(P_Mass, "\\(0,", "\\(<0.01,")) |>
            mutate(P_Mass_Label = map_chr(P_Mass_Label, ~ paste0("italic(\'P\')*\' = ",.,"\'"))) |>
            select(Sex, Model, Correlation, P_Mass_Label, P_Mass)

FIGURE_S2 <- ggplot(filter(pglsPoints, Model=="Mass-Lek")) +
         geom_abline(data=filter(massPGLS, Model=="Mass-Lek",
                                 Sex=="F"),
                     aes(intercept=Estimate_Intercept, 
                         slope=Estimate_Mass),
                     colour="lightgray") +
         geom_abline(data=filter(massPGLS, Model=="Mass-Lek",
                                 Sex=="M"),
                     aes(intercept=Estimate_Intercept, 
                         slope=Estimate_Mass),
                     colour="lightgray") +
         geom_abline(data=filter(summaryMassPGLS, Model=="Mass-Lek",
                                 Sex=="F"),
                     aes(intercept=Estimate_Intercept_Median,
                         slope=Estimate_Mass_Median,
                         linetype=P_Mass_Median<0.05),
                     colour="black") +
         geom_abline(data=filter(summaryMassPGLS, Model=="Mass-Lek",
                                 Sex=="M"),
                     aes(intercept=Estimate_Intercept_Median,
                         slope=Estimate_Mass_Median,
                         linetype=P_Mass_Median<0.05),
                         colour="black") +
         geom_point(aes(x=Mass_Log_10, y=Alpha_Log, fill=Model),
                    shape=21, colour="black", alpha=0.8) +
         geom_text(data=filter(pglsPLabels, Sex=="F"),
                   aes(label=P_Mass_Label),
                   parse=TRUE,
                   x=Inf, y=3.6,
                   size=2, hjust=1.05, vjust=1) +
         geom_text(data=filter(pglsPLabels, Sex=="M"),
                   aes(label=P_Mass_Label),
                   parse=TRUE,
                   x=Inf, y=3.6,
                   size=2, hjust=1.05, vjust=1) +
         facet_wrap(facets=~Sex,
                    labeller=as_labeller(facetLabels)) +
         scale_x_continuous(limits=c(0,5)) +
         scale_y_continuous(limits=c(0,3.6)) +
         scale_colour_manual(values=colours_pgls) +
         scale_linetype_manual(values=c("TRUE"="solid",
                                        "FALSE"="dashed")) +
         guides(linetype="none", fill="none") +
         xlab(expression("Mass (log"[10]*")")) +                 
         ylab(expression("Age at first reproduction (log"[2]*")")) +                 
         theme_lt +
         theme(strip.background=element_rect(colour="black", fill=NA),
               strip.text=element_text(size=10, hjust=0))

ggsave(FIGURE_S2, file="Plots/FIGURE_S2.png", width=5, height=2.5)    

# FIGURE S3 ----------------------------------- 
# Detailed breeding cohort data
# FIGURE S3 ----------------------------------- 

cohorts <- read_csv("Data/alpha_distribution_subset.csv", show_col_types=FALSE) |>
        pivot_longer(cols=A.1:A.8, names_to="Age", values_to="Proportion") |>
        mutate(Age = map_dbl(Age, ~as.numeric(str_replace(.,"A.","")))) |>
        mutate(Proportion = ifelse(is.na(Proportion), 0, Proportion))

cohortCentralAlpha <- cohorts |>
                   group_by(Taxon_Clements, Sex) |> 
                   mutate(Cumulative_Proportion = cumsum(Proportion)) |>
                   filter(Cumulative_Proportion > 0.5) |>
                   slice_min(order_by=Age, n=1, with_ties=FALSE) |>
                   select(I, Taxon_Clements, Common_Clements, Sex, Age, Cumulative_Proportion)

facetOrder <- cohorts |>
           arrange(I) |>
           pull(Taxon_Clements) |>
           unique()

cohorts$Taxon_Clements <- factor(cohorts$Taxon_Clements, levels=facetOrder)
cohortCentralAlpha$Taxon_Clements <- factor(cohortCentralAlpha$Taxon_Clements, levels=facetOrder)

cohortFillColors = c("F" = alpha("blue", 0.5),
                     "M" = alpha("red", 0.25),
                     "U" = alpha("black", 0.6))

cohortThresholdColors = c("F" = alpha("blue", 0.5),
                          "M" = alpha("red", 0.5),
                          "U" = alpha("black", 0.8))

FIGURE_S3 <- ggplot(cohorts) +
          geom_bar(aes(x=Age, y=Proportion,
                       fill=Sex), colour="black", 
                   stat="identity", position="dodge") +
          geom_vline(data=cohortCentralAlpha,
                     aes(xintercept=Age, colour=Sex),
                     linetype="dashed") +           
          facet_wrap(facets=vars(Taxon_Clements)) +
          scale_x_continuous(breaks=seq(1,8,by=2), labels=seq(1,8, by=2)) +
          scale_y_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.5)) +
          scale_fill_manual(values=cohortFillColors) +
          scale_colour_manual(values=cohortThresholdColors) +
          guides(fill="none", colour="none") +
          ylab("Proportion of population first breeding") +
          theme_bw() +
          theme(panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                strip.background=element_rect(fill=NA, colour="black"),
                strip.text=element_text(size=5, face="italic"),
                axis.title.x=element_text(size=8),
                axis.text.x=element_text(size=6),
                axis.title.y=element_text(size=8),
                axis.text.y=element_text(size=6))
ggsave(FIGURE_S3, file="Plots/FIGURE_S3.png", width=6, height=5)         

# APPENDIX C tables ----------------------------------- 
# Discrete evolutionary model transition rates
# APPENDIX C tables ----------------------------------- 
corHMMStatesFull <- c("1"="1 O",
                      "2"="1 S",
                      "3"="2 O",
                      "4"="2 S",
                      "5"="3+ O",
                      "6"="3+ S")

TABLES_APPENDIX_III <- read_csv("Output/results_corhmm.csv", show_col_types=FALSE) |> 
                    filter(Variable %in% c("Alpha_F", "Alpha_M")) |>
                    mutate(Model = map2_chr(HiddenStates_T1, Dependence, classifyModel)) |>
                    mutate(Model = factor(Model, levels=discreteModelOrder)) |>
                    select(Variable, Model, Iteration, contains("R1") | contains("R2")) |>
                    pivot_longer(cols=c(contains("R1"), contains("R2")), 
                                 names_to="Transition", values_to="Value") |>
                    group_by(Variable, Model, Transition) |>
                    summarize(.groups="keep",
                              Median=median(Value), 
                              Low=quantile(Value,0.05,na.rm=TRUE), 
                              High=quantile(Value,0.95,na.rm=TRUE)) |>
                    filter(!is.na(Median)) |>
                    mutate(Value = paste0(round(Median,4)," (",round(Low,4),", ",round(High,4),")"))|>
                    mutate(Transition = str_replace_all(Transition, "\\(|\\)", "")) |>
                    mutate(Transition = str_replace_all(Transition, "R", "H")) |>
                    separate(Transition, into=c("Start", "End"), sep="->") |>
                    separate(Start, into=c("State_Start", "Hidden_Start"), sep=",") |>
                    separate(End, into=c("State_End", "Hidden_End"), sep=",") |>
                    mutate(State_Start = corHMMStatesFull[State_Start],
                           State_End = corHMMStatesFull[State_End]) |>
                    mutate(Start = paste(Hidden_Start, State_Start),
                           End = paste(Hidden_End, State_End)) |>
                    mutate(Sex = map_chr(Variable, ~str_split_1(.,"_")[2])) |>
                    ungroup() |>
                    select(Sex, Model, Start, End, Value) |>
                    pivot_wider(names_from=Sex, values_from=Value)

TABLE_SIII_H1IND <- filter(TABLES_APPENDIX_III, Model == "H1-Independent")
write_tsv(TABLE_SIII_H1IND, file="Output/TABLE_SIII_H1IND.tsv")

TABLE_SIII_H1DEP <- filter(TABLES_APPENDIX_III, Model == "H1-Dependent")
write_tsv(TABLE_SIII_H1DEP, file="Output/TABLE_SIII_H1DEP.tsv")

TABLE_SIII_H2IND <- filter(TABLES_APPENDIX_III, Model == "H2-Independent")
write_tsv(TABLE_SIII_H2IND, file="Output/TABLE_SIII_H2IND.tsv")

TABLE_SIII_H2DEP <- filter(TABLES_APPENDIX_III, Model == "H2-Dependent")
write_tsv(TABLE_SIII_H2DEP, file="Output/TABLE_SIII_H2DEP.tsv")

DIFFERENCES_APPENDIX_III <- read_csv("Output/results_corhmm.csv", show_col_types=FALSE) |> 
                         filter(Variable %in% c("Alpha_F", "Alpha_M")) |>
                         mutate(Model = map2_chr(HiddenStates_T1, Dependence, classifyModel)) |>
                         filter(grepl("-Dependent", Model)) |>  
                         mutate(Model = factor(Model, levels=discreteModelOrder)) |>
                         select(Variable, Model, Iteration, contains("R1") | contains("R2")) |>
                         pivot_longer(cols=c(contains("R1"), contains("R2")), 
                                      names_to="Transition", values_to="Value") |>
                         group_by(Variable, Model, Transition) |>
                         mutate(Transition = str_replace_all(Transition, "\\(|\\)", "")) |>
                         mutate(Transition = str_replace_all(Transition, "R", "H")) |>
                         separate(Transition, into=c("Start", "End"), sep="->") |>
                         separate(Start, into=c("State_Start", "Hidden_Start"), sep=",") |>
                         separate(End, into=c("State_End", "Hidden_End"), sep=",") |>
                         mutate(State_Start = corHMMStatesFull[State_Start],
                                State_End = corHMMStatesFull[State_End]) |>
                         unite(Start, Hidden_Start, State_Start, sep=" ") |>
                         unite(End, Hidden_End, State_End, sep=" ") |>
                         unite(Transition, Start, End, sep="_") |>
                         pivot_wider(id_cols=c(Variable, Model, Iteration), 
                                     names_from=Transition, values_from=Value,
                                     names_repair=~str_replace_all(.," |\\+",".")) |>
                         mutate("Diff H1 1->2 S vs. O" = H1.1.S_H1.2.S - H1.1.O_H1.2.O,
                                "Diff H1 2->1 S vs. O" = H1.2.S_H1.1.S - H1.2.O_H1.1.O,                  
                                "Diff H2 1->2 S vs. O" = H2.1.S_H2.2.S - H2.1.O_H2.2.O,
                                "Diff H2 2->1 S vs. O" = H2.2.S_H2.1.S - H2.2.O_H2.1.O,
                                "Diff H1 2->3+ S vs. O" = H1.2.S_H1.3..S - H1.2.O_H1.3..O, 
                                "Diff H1 3+->2 S vs. O" = H1.3..S_H1.2.S - H1.3..O_H1.2.O,
                                "Diff H2 2->3+ S vs. O" = H2.2.S_H2.3..S - H2.2.O_H2.3..O, 
                                "Diff H2 3+->2 S vs. O" = H2.3..S_H2.2.S - H2.3..O_H2.2.O) |>
                         select(Variable, Model, Iteration, contains("Diff")) |>
                         pivot_longer(contains("Diff"), names_to="Comparison", values_to="Difference") |>
                         mutate(Sex = map_chr(Variable, ~str_split_1(.,"_")[2])) |>
                         group_by(Sex, Model, Comparison) |>
                         summarize(.groups="keep",
                                   Median=median(Difference), 
                                   Low=quantile(Difference,0.05,na.rm=TRUE), 
                                   High=quantile(Difference,0.95,na.rm=TRUE)) |>
                         mutate(Difference = paste0(round(Median,4)," (",round(Low,4),", ",round(High,4),")")) |>
                         ungroup() |>
                         select(Sex, Model, Comparison, Difference) |>
                         pivot_wider(names_from=Sex, values_from=Difference)

write_tsv(DIFFERENCES_APPENDIX_III, file="Output/TABLE_SIII_PARAMETERDIFFERENCES.tsv")



