#-------------------------------------------------------------------------------
# Script to retrieve the full citation of the data sets contributed to ShareTrait
#-------------------------------------------------------------------------------
# Cleaning working space
rm(list=ls())

# set the working directory to the folder containing this script:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# check directory
getwd()

#Libraries
library(dplyr)
library(stringr)
library(RefManageR)

#load data
df <- read.csv("../3_release/2_2_ShareTrait_DataBase_v1.0.0.csv",sep = ",",header = TRUE)
length(unique(df$doi_publication))
# 37 publications

length(unique(df$doi_dataset))
# 37 datasets

# Extract the DOI text after "https://doi.org/"
doi <- ifelse(!is.na(df$doi_publication), str_extract(df$doi_publication, "(?<=https://doi.org/).*"), NA)
length(unique(doi))
doi <- unique(doi)
 
# Retrieve the citation based on the DOIs

refs <- GetBibEntryWithDOI(doi)
WriteBib(refs,"../3_release/ShareTrait_DataBase_refs_v1.0.0.bib")

# saving session information with all packages versions for reproducibility purposes
sink("../outputs/2_4_citations_R_session.txt")
sessionInfo()
sink()
# END OF SCRIPT