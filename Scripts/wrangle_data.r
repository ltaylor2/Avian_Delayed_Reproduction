# Evolution of delayed reproduction in birds
# R script to clean and summarize dataset
# OUTPUT: Writes clean data file (Data/data_clean.tsv)
#         Query list for BirdTree taxa (Data/BirdTree_query_list.txt)
#         Summary taxon tallies (Output/taxa_summaries_report.txt)
#         Random focal trees for example plots (Data/randomMainTree_*.nex)
# L.U.T. updated 2023-07-13

# Logistics -------------------------------------

# Required packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(ape))

# Organizing data -------------------------------------

# Read raw data
raw <- read_xlsx("Data/data_raw_2023-07-23.xlsx", na="NA") |>
    mutate(Taxon_Clements=Taxon_Working)

# Read in Clements v2022 taxonomy
taxonomy_clements <- read_csv("Data/Clements_Taxonomy_v2022.csv",
                              col_types=paste(rep("c", times=12), collapse=""),
                              show_col_types=FALSE) |>
                  filter(category=="species") |>
                  select(Index_Clements="C",
                         Order_Clements="order",
                         Family_Clements="family",
                         Taxon_Clements="scientific name",
                         Common_Clements="English name") |>
                  separate(Family_Clements, into="Family_Clements", sep="\\ \\(", extra="drop") |>
                  mutate(Index_Clements = as.numeric(Index_Clements))

# Read in AVONET dataset for mass
data_avonet <- read_csv("Data/AVONET_Clements_Data.csv",
                        show_col_types=FALSE) |>
            select(Taxon_Clements="Species2",
                   Avibase_ID="Avibase.ID2",
                   Mass)

# Read in Clements-BirdTree dictionary
dictionary_clements_to_birdtree <- read_csv("Data/Taxonomy_Dictionary_Clements_To_BirdTree.csv",
                                            show_col_types=FALSE)


# Custom function to simplify social context classifications
#   across multiple columns
simplifySocialContext <- function(Colony, Lek, Parent) {
    isCooperative <- (Parent=="Cooperative" | Parent=="!Cooperative")
    isColonial <- Colony==1
    isLekking <- Lek==1
    if (isCooperative) { return("Cooperative") }
    if (isColonial) { return("Colony") }
    if (isLekking) { return("Lek") }
    return("Other")
}

# Organize the data
#  Beginning with age data, fill in F and M individual sex ages with U age if applicabble
#                           recode min and central ages of 0 as 1
#                           add log scale values
#                           add difference Male - Female age columns
#                           add simplified social context column
#  Join the Clements taxonomy
#  Join the Clements-to-BirdTree list
#  Join the AVONET data
#  Select and reorder columns for clean datasheet
#  Arrange in Clements taxonomic order
data <- raw |>
      mutate(Min_Alpha_F=map2_dbl(Min_Alpha_U, Min_Alpha_F, ~ifelse(!is.na(.x)&is.na(.y),.x,.y)),
             Min_Alpha_M=map2_dbl(Min_Alpha_U, Min_Alpha_M, ~ifelse(!is.na(.x)&is.na(.y),.x,.y)),
             Alpha_F=map2_dbl(Alpha_U, Alpha_F, ~ifelse(!is.na(.x)&is.na(.y),.x,.y)),
             Alpha_M=map2_dbl(Alpha_U, Alpha_M, ~ifelse(!is.na(.x)&is.na(.y),.x,.y))) |>
      mutate(Min_Alpha_F=ifelse(Min_Alpha_F<1, 1, Min_Alpha_F),
             Min_Alpha_M=ifelse(Min_Alpha_M<1, 1, Min_Alpha_M),
             Alpha_F=ifelse(Alpha_F<1, 1, Alpha_F),
             Alpha_M=ifelse(Alpha_M<1, 1, Alpha_M)) |>
      mutate(Alpha_Log_F=map_dbl(Alpha_F, ~ ifelse(.==0, log(1, base=2), log(., base=2))),
             Alpha_Log_M=map_dbl(Alpha_M, ~ ifelse(.==0, log(1, base=2), log(., base=2)))) |>
      mutate(Min_Alpha_Log_F=map_dbl(Min_Alpha_F, ~ ifelse(.==0, log(1, base=2), log(., base=2))),
             Min_Alpha_Log_M=map_dbl(Min_Alpha_M, ~ ifelse(.==0, log(1, base=2), log(., base=2)))) |>
      mutate(Alpha_Diff=ifelse(!is.na(Alpha_F)&!is.na(Alpha_M), Alpha_M-Alpha_F, NA)) |>
      mutate(Min_Alpha_Diff=ifelse(!is.na(Min_Alpha_F)&!is.na(Min_Alpha_M), Min_Alpha_M-Min_Alpha_F, NA)) |>
      mutate(Social_Context=pmap_chr(list(Colony, Lek, Parental_Care), simplifySocialContext)) |>
      left_join(taxonomy_clements, by="Taxon_Clements") |>
      left_join(dictionary_clements_to_birdtree, by="Taxon_Clements") |>
      mutate(Label_BirdTree = str_replace(Taxon_BirdTree, " ", "_")) |>
      left_join(data_avonet, by="Taxon_Clements") |>
      select(Index_Clements,
             Avibase_ID,
             Order_Clements,
             Family_Clements,
             Taxon_Clements,
             Taxon_BirdTree,
             Label_BirdTree,
             Common_Clements,
             Colony,
             Lek,
             Parental_Care,
             Social_Context,
             Mass,
             Alpha_F,
             Alpha_Log_F,
             Min_Alpha_F,
             Min_Alpha_Log_F,
             Alpha_M,
             Alpha_Log_M,
             Min_Alpha_M,
             Min_Alpha_Log_M,
             Alpha_Diff,
             Min_Alpha_Diff,
             Source_Alpha,
             Notes_Alpha) |>
     arrange(Index_Clements)

# Check for Clements taxonomy mapping errors
check <- filter(data, is.na(Order_Clements))
if (nrow(check) > 0) { 
    cat("\nERROR\tRaw data not mapped correctly to Clements taxonomy.\nCheck the Index_Working column for the following.\nQuitting early.\n") 
    check |>
        select(Taxon_Clements) %>%
        print(n=nrow(.)) 
    quit()
}

# Check for AVONET mapping errors
# NOTE rather than adding a whole dictionary, 
#      four taxon names have been revised in the Data/AVONET_Clements_Data.csv file:
#       Bubo shelleyi->Ketupa shellyi
#       Calyptorhynchus funereus->Zanda funereus
#       Calyptorhynchus latirostris->Zanda latirostris
#       Calyptorhynchus baudinii->Zanda baudinii
check <- filter(data, is.na(Mass))
if (nrow(check) > 0) { 
    cat("\nERROR\tRaw data not mapped correctly to AVONET database.\nMake a dictionary between AVONET scientific names and Taxon_Clements.\nQuitting early.\n") 
    check |>
        select(Avibase_ID, Taxon_Clements) %>%
        print(n=nrow(.))
    quit()
}

# Check for BirdTree mapping errors
check <- filter(data, is.na(Taxon_BirdTree))
if (nrow(check) > 0) { 
    cat("\nERROR\tRaw data not mapped correctly to BirdTree taxonomy.\nCheck the Clements_to_BirdTree dictionary.\nQuitting early.\n") 
    check |> 
        select(Avibase_ID, Taxon_Clements, Taxon_BirdTree) %>%
        print(n=nrow(.))
    quit()
}

# Check for missing age data
check <- filter(data, is.na(Alpha_F) &
                      is.na(Alpha_M) |
                      is.na(Min_Alpha_F) &
                      is.na(Min_Alpha_M))
if (nrow(check) > 0) { 
    cat("\nERROR\tRaw data missing some Alpha values.\nCheck the original datasheet or any functions used to map between Sex-Unknown/Monomorphic and Male/Female-specific values.\nQuitting early.\n") 
    check |>
        select(Avibase_ID, Taxon_Clements,
               Alpha_F, Min_Alpha_F, Alpha_M, Min_Alpha_M) %>%
        print(n=nrow(.))
    quit()
}

# Write cleaned data to file
write_tsv(data, "Data/data_clean.tsv")
cat("Data checked and wrangled. Clean datasheet in Data/data_clean.tsv\n")

# Write BirdTree Query List
birdTreeQueryList <- data |>
                  pull(Taxon_BirdTree) |>
                  paste(collapse="\n")
sink("Data/birdtree_query_list.txt")
cat(birdTreeQueryList)
sink()
cat("Query list for BirdTree taxa (one taxon per line) in Data/BirdTree_query_list.txt.\n")

# Taxon summary report -------------------------------------

sink("Output/taxa_summaries_report.txt")

pp <- function(..., sep="") { cat(paste(c(..., "\n"), collapse=sep)) }
lb <- function() { cat("\n\n/////////////////////////\n\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n\n") }

nSampleTaxaTotal <- nrow(data)
nSampleTaxaFemales <- nrow(filter(data, !is.na(Alpha_F)))
nSampleTaxaMales <- nrow(filter(data, !is.na(Alpha_M)))

lb()
pp("Total taxa count = ", nSampleTaxaTotal, " taxa, ",
   "with ", nSampleTaxaFemales, " female values and ",
   nSampleTaxaMales, " male values.")

totalOrders <- unique(taxonomy_clements$Order_Clements)
nTotalOrders <- length(totalOrders)
sampleOrders <- unique(data$Order_Clements)
nSampleOrders <- length(sampleOrders)

totalFamilies <- unique(taxonomy_clements$Family_Clements)
nTotalFamilies <- length(totalFamilies)
sampleFamiles <- unique(data$Family_Clements)
nSampleFamilies <- length(sampleFamiles)

pp("Samples from ", nSampleOrders, "/", nTotalOrders, " Orders (", nTotalOrders-nSampleOrders, " missing) and ",
   nSampleFamilies, "/", nTotalFamilies, " Families (", nTotalFamilies - nSampleFamilies, " missing) via Clements list.")
lb()

missingOrders <- totalOrders[!(totalOrders %in% sampleOrders)]
pp("Missing Orders:\n", paste0(missingOrders, collapse="\n"))
lb()

mfs <- totalFamilies[!(totalFamilies %in% sampleFamiles)]
missingFamilies <- tibble(Family_Clements=totalFamilies[!(totalFamilies %in% sampleFamiles)]) |>
                left_join(taxonomy_clements, by="Family_Clements") |>
                select(Order_Clements, Family_Clements) |>
                group_by(Order_Clements, Family_Clements) |>
                slice_head(n=1)

pp("Missing Families:\n", paste0(unite(missingFamilies, s, Order_Clements, Family_Clements, sep=": ")$s, collapse="\n"))

pp("\nSummary of missing families:")
missingFamilies |>
    group_by(Order_Clements) |>
    tally() |>
    print()
lb()

pp("Social context tallies:")
data |>
    group_by(Social_Context) |>
    tally() |>
    print()
lb()

pp("Female social context tallies:")
data |>
    filter(!is.na(Alpha_F)) |>
    group_by(Social_Context) |>
    tally() |>
    print()
lb()

pp("Male social context tallies:")
data |>
    filter(!is.na(Alpha_M)) |>
    group_by(Social_Context) |>
    tally() |>
    print()
lb()

pp("The 30 highest age at first reproduction values:")
data |>
    mutate(Temp_Max_Alpha=map2_dbl(Alpha_M, Alpha_F, ~ return(max(.x, .y)))) |>
    arrange(-Temp_Max_Alpha) |>
    select(Family=Family_Clements, Taxon=Taxon_Clements, Alpha_F, Alpha_M, Mass) |>
    slice_head(n=30) %>%
    print(n=nrow(.))
lb()

# Get a list of species with age code 0 in raw data 
# (for analyses, these are coded as age 1, see Appendix I)
age0Breeders <- raw |>
             filter(Min_Alpha_U == 0 | Min_Alpha_F == 0 | Min_Alpha_M == 0 |
                    Alpha_U == 0 | Alpha_F == 0 | Alpha_M == 0) |>
             left_join(taxonomy_clements, by=c("Taxon_Working"="Taxon_Clements")) |>
             select(Index_Clements, Taxon_Clements) |>
             arrange(Index_Clements) |>
             pull(Taxon_Clements)

cat("Age 0 Breeders from raw data (coded as 1 in analysis, see Appendix I):\n")
walk(age0Breeders, ~cat(paste0(.,"\n")))
sink()

cat("Taxa sample values summarized in Output/taxa_summaries_report.txt\n")

# Prepare focal tree -------------------------------------

cat("Reading BirdTree treeset and storing random focal tree.\n")

# Read in full tree distribution
trees <- read.nexus("Data/BirdTree_1000_Trees.nex")

# Choose a random focal tree
set.seed(1973)
randomMainTree <- sample(trees, size=1)[[1]]

# Prune trees to data
randomMainTree <- drop.tip(randomMainTree, 
                           randomMainTree$tip.label[!(randomMainTree$tip.label%in%data$Label_BirdTree)])
randomMainTree_F <- drop.tip(randomMainTree, data[is.na(data$Alpha_F),]$Label_BirdTree)
randomMainTree_M <- drop.tip(randomMainTree, data[is.na(data$Alpha_M),]$Label_BirdTree)

write.nexus(randomMainTree_F, file="Data/randomMainTree_F.nex", translate=FALSE)
write.nexus(randomMainTree_M, file="Data/randomMainTree_M.nex", translate=FALSE)

cat("Focal trees stored in Data/randomMainTree*.nex.\n")