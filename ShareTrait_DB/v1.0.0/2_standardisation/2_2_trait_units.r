# The following code has been adapted from MetaR (https://github.com/felixpleiva/MetaR)
# by Felix P Leiva

# Cleaning working space
rm(list=ls())

# check directory
getwd()

#Libraries
library(dplyr)
library(tidyverse)
library(stringr)

#load data
df <- read.csv("../outputs/2_1_ShareTrait_database.csv",sep = ",",header = TRUE)

# check trait units
unique(df$trait_unit)

# Standardize units of fecundity
df$trait_unit[df$trait_unit ==  "Egg load at adult emergence" ]         <- "offspring number"
df$trait_unit[df$trait_unit ==  "Number of eggs" ]                      <- "offspring number"
df$trait_unit[df$trait_unit ==  "Total number of offspring" ]           <- "offspring number"
df$trait_unit[df$trait_unit ==  "Number of eggs per female" ]           <- "offspring number"
df$trait_unit[df$trait_unit ==  "number of mature eggs"]                <- "offspring number"
df$trait_unit[df$trait_unit ==  "number of hatched eggs"]               <- "offspring number"
df$trait_unit[df$trait_unit ==  "Number of eggs laid throughout life"]  <- "offspring number"
df$trait_unit[df$trait_unit ==  "number of eggs per female"]            <- "offspring number"
df$trait_unit[df$trait_unit ==  "number of eggs" ]                      <- "offspring number"
df$trait_unit[df$trait_unit ==  "cumulative number of eggs per female" ]<- "offspring number"
df$trait_unit[df$trait_unit ==  "number of eggs by female" ]            <- "offspring number"

# check trait units
unique(df$trait_unit)

# Standardize units of development
df$trait_unit[df$trait_unit ==  "days"]  <- "Days"

# check trait units
unique(df$trait_unit)

# The aim of the following lines is to convert all units of metabolic rates to
# mgO2/h/ind. For simplicity, I will use a conversion factor of "1" for
# developmental time and fecundity

# ------------------------------------------------------------------------------
# Conversion factors for oxygen

# 1 L of oxygen                              = 20.083 kiloJoule of oxygen  = 0.0446 mol oxygen - Schmidt-Nielsen 1997, page 583
# 1 mmHg of oxygen                           = 1 Torr = 0.133322 kPa
# 1 mol of oxygen                            = 31.9988 grams of oxygen
# 1 mol of a gas at STP occupies volume      = 22.414 L

# ------------------------------------------------------------------------------
# STEP 1: Convert all units of oxygen to milligram of oxygen (mgO2)

# make a new column and extract the unit of oxygen before the "/"

df$extracted_unit <- str_extract(df$trait_unit, "[^/]+")
sort(unique(df$extracted_unit))

# Define lockup table for conversion factors. In the case of the metabolic rate, 
# I will convert all units to **mg O2**

conversion_table_unit <- data.frame(
  extracted_unit = c("days",
                      "Joule", 
                      "mgO2",
                      "mlCO2",
                      "mLO2",
                      "nmolO2",
                      "offspring number",
                      "uLCO2",
                      "umolO2"),

  conversion_factor = c(1,
                        0.046 / 20.083 * 1000 * 31.9988, # I am not sure about this one!!
                        1,
                        1 / 22.414 * 31.9988 / 0.85,
                        1 / 22.414 * 31.9988,
                        31.9988 / 1000000,
                        1,
                        1 / 22.414 * 31.9988 / 0.85 / 1000,
                        31.9988 /1000)
)


# Merge lockup table with original data
df_converted_step_1 <- left_join(df, conversion_table_unit, by = "extracted_unit")

# Calculate trait value rates for all rows and create a new column
df_converted_step_1 <- df_converted_step_1 %>% 
  mutate(trait_converted = trait_value * conversion_factor)

# STEP 2: Convert all units of time (for metabolic rates) to hours. So we will
# keep the units for the other traits

# select the units of time
time_unit <- "(day|month|min|second|30min|20min|h|Days|offspring number)"

# Extract the text and create a new column
df_converted_step_1$extracted_time_unit <- str_extract(df_converted_step_1$trait_unit, time_unit)
sort(unique(df_converted_step_1$extracted_time_unit))

# Define lockup table for conversion factors hours
conversion_table_time <- data.frame(
  extracted_time_unit = c("day",
                          "Days",
                          "h",
                          "min",
                          "offspring number"),
  
  conversion_factor_to_hours = c(1 / 24,
                                 1,
                                 1,
                                 60,
                                 1)
)

# Merge lookup table with original data
df_converted_step_2 <- left_join(df_converted_step_1, conversion_table_time, by = "extracted_time_unit")

# Calculate respiration rate per hour for all rows
df_converted_step_2 <- df_converted_step_2 %>% 
  mutate(trait_converted = trait_converted * conversion_factor_to_hours)

# STEP 3: Express all units of oxygen consumption per individual

# select, extract and make a new column the unit of body mass in "trait_unit"

df_converted_step_2$extracted_size_unit <- str_extract(df_converted_step_2$trait_unit, "(Days|offspring number|ugN2|ugDM|ugCarbon|mgDM|mgFM|mgAFDM|gDM|gFM|100gFM|gAFDM|100gram|kgFM|ind)")
sort(unique(df_converted_step_2$extracted_size_unit))

# check the type of sizes used to express body size and check their units
sort(unique(df_converted_step_2$size_type))

sort(unique(df_converted_step_2$size_units))

# convert the units per individual

df_converted_step_3 <- df_converted_step_2 %>%
  mutate(trait_converted = case_when(
    extracted_size_unit == "ind" ~ trait_converted,
    extracted_size_unit == "Days" ~ trait_converted,
    extracted_size_unit == "offspring number" ~ trait_converted,
    extracted_size_unit == "gDM" & size_type == "dry body mass" & size_units == "gram" ~ trait_converted * size_value,
    extracted_size_unit == "kgFM" & size_type == "fresh body mass" & size_units == "kilogram" ~ trait_converted * size_value,
    TRUE ~ NA_real_ # The TRUE ~ NA_real_ statement handles the cases where none of the conditions match, assigning NA to trait_converted
  ))

# STEP 4: Convert all units of body size to gram of fresh mass for the metabolic rate

# calculate body size of fresh mass in grams

df_converted_step_4 <- df_converted_step_3 %>%
  mutate(fresh_mass_gram = case_when(
    size_type == "fresh body mass" & size_units == "kilogram" ~ size_value * 1000,
    size_type == "fresh body mass" & size_units == "gram" ~ size_value,
    size_type == "fresh body mass" & size_units == "microgram" ~ size_value / 1000000,
    size_type == "fresh body mass" & size_units == "milligram" ~ size_value / 1000,
    size_type == "fresh body mass" & size_units == "nanogram" ~ size_value / 1000000000,
    size_type == "dry body mass" & size_units == "gram" ~ size_value / 0.25,
    size_type == "dry body mass" & size_units == "kilogram" ~ size_value * 1000 / 0.25,
    size_type == "dry body mass" & size_units == "microgram" ~ size_value / 1000000 / 0.25,
    size_type == "dry body mass" & size_units == "milligram" ~ size_value / 1000 / 0.25,
    size_type == "dry body mass" & size_units == "nanogram" ~ size_value / 1000000000/ 0.25,
    size_type == "total length" & size_units == "millimeter" & dataset_id == "Hermaniuk_et_al_2021" ~ (size_value ^ 3) / 1000 ,
    TRUE ~ NA_real_ # The TRUE ~ NA_real_ statement handles the cases where none of the conditions match, assigning NAs.
  ))

# STEP 5: Convert all units of body size to gram of fresh mass for the metabolic
# rate and save the cleaned dataframe to a new file

# delete authors information and non relevant columns
df_converted_step_4 <- df_converted_step_4 %>%
  select(-name_contact) %>%
  select(-email_contact) %>%
  select(-extracted_unit) %>%
  select(-conversion_factor) %>%
  select(-conversion_factor_to_hours) %>%
  select(-extracted_size_unit) %>%
  select(-extracted_time_unit)

  
names(df_converted_step_4)

# Save the cleaned dataframe to a new file
write.csv(df_converted_step_4, "../outputs/2_2_ShareTrait_DataBase.csv", row.names = FALSE)

# saving session information with all packages versions for reproducibility purposes

sink("../outputs/2_2_trait_units_R_session.txt")
sessionInfo()
sink()
# END OF SCRIPT