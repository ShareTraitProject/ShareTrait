# Cleaning working space
rm(list = ls())

# Check directory
getwd()

# Load required libraries
library(data.table)  # Required for fread() function
library(dplyr)       # Required for bind_rows() function

# Set the working directory to the "cleaned_data/development" folder
setwd("../cleaned_data/development")

# Read and combine CSV files using fread()
dev_files <- list.files(pattern = "*.csv") %>%
  purrr::map_df(~ fread(.))

# Set the working directory to the "fecundity" folder
setwd("../fecundity")

fec_files <- list.files(pattern = "*.csv") %>%
  purrr::map_df(~ fread(.))

# Set the working directory to the "metabolic_rate" folder
setwd("../metabolic_rate")

met_files <- list.files(pattern = "*.csv") %>%
  purrr::map_df(~ fread(.))

# Set the working directory to the "Outputs" folder
setwd("../../Outputs")

# Export the individual data frames to CSV files
write.csv(dev_files, "ShareTrait_Development.csv", row.names = FALSE)
write.csv(fec_files, "ShareTrait_Fecundity.csv", row.names = FALSE)
write.csv(met_files, "ShareTrait_Metabolic_Rates.csv", row.names = FALSE)

# Saving session information with all package versions for reproducibility purposes
sink("./1_1_compilation_R_session.txt")
sessionInfo()
sink()