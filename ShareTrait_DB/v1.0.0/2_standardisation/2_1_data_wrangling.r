# Cleaning working space
rm(list=ls())

# check directory
getwd()

#Libraries
library(dplyr)
library(stringr)

#load data
dev_files <- read.csv("../outputs/ShareTrait_Development.csv",sep = ",",header = TRUE)
fec_files <- read.csv("../outputs/ShareTrait_Fecundity.csv",sep = ",",header = TRUE)
met_files <- read.csv("../outputs/ShareTrait_Metabolic_Rates.csv",sep = ",",header = TRUE)

# Add a new column to identify the trait
dev_files$trait_name <- as.factor("development")
fec_files$trait_name <- as.factor("fecundity")
met_files$trait_name <- as.factor("metabolic_rate")

# change the names of some columns representing the same information
colnames(dev_files)[colnames(dev_files) == "comments_development"]  <- "comments_trait"
colnames(fec_files)[colnames(fec_files) == "comments_fecundity"]    <- "comments_trait"
colnames(met_files)[colnames(met_files) == "comments_respiration"]  <- "comments_trait"


colnames(dev_files)[colnames(dev_files) == "development_time"]      <- "trait_value"
colnames(fec_files)[colnames(fec_files) == "fecundity"]             <- "trait_value"
colnames(met_files)[colnames(met_files) == "resp_value"]            <- "trait_value"


colnames(dev_files)[colnames(dev_files) == "development_time_unit"] <- "trait_unit"
colnames(fec_files)[colnames(fec_files) == "fecundity_unit"]        <- "trait_unit"
colnames(met_files)[colnames(met_files) == "resp_unit"]             <- "trait_unit"

colnames(met_files)[colnames(met_files) == "resp_error_estimate"]   <- "trait_error_estimate"
colnames(met_files)[colnames(met_files) == "resp_error_type"]       <- "trait_error_type"

# Combine the data frames
df <- dplyr::bind_rows(dev_files, fec_files, met_files)

# Removing leading/trailing white spaces
df$comments_reference               <- trimws(df$comments_reference)
df$species_reported                 <- trimws(df$species_reported)
df$comments_taxonomy                <- trimws(df$comments_taxonomy)
df$comments_location                <- trimws(df$comments_location)
df$comments_timing                  <- trimws(df$comments_timing)
df$comments_experimental_conditions <- trimws(df$comments_experimental_conditions)
df$comments_trait                   <- trimws(df$comments_trait)

# Reformatting strings. Convert the string column to lowercase
df$realm_general                    <- tolower(df$realm_general)
df$realm_specific                   <- tolower(df$realm_specific)
df$origin                           <- tolower(df$origin)
df$experiment_location              <- tolower(df$experiment_location)
df$maintained                       <- tolower(df$maintained)
df$acclimated                       <- tolower(df$acclimated)
df$test_food_type                   <- tolower(df$test_food_type)
df$mating                           <- tolower(df$mating)

# Reformatting dates. Convert date column to the desired format
df$date_of_contribution         <- as.Date(df$date_of_contribution, format = "%d-%m-%Y")  
df$date_of_collection_initial   <- as.Date(df$date_of_collection_initial, format = "%d-%m-%Y")
df$date_of_collection_final     <- as.Date(df$date_of_collection_final, format = "%d-%m-%Y")

# extract initial and final year
df$year_of_collection_initial   <- as.factor(substring(df$date_of_collection_initial,1,4))
df$year_of_collection_final     <- as.factor(substring(df$date_of_collection_final,1,4))

#to extract and create a new column "month"
df$month_of_collection_initial  <- as.factor(substring(df$date_of_collection_initial,6,7))
df$month_of_collection_final    <- as.factor(substring(df$date_of_collection_final,6,7))

#to extract and create a new column "day"
df$day_of_collection_initial    <- as.factor(substring(df$date_of_collection_initial,9,10))
df$day_of_collection_final      <- as.factor(substring(df$date_of_collection_final,9,10))

# create two new columns with initial and final dates using the format "YYYYMMDD"
df$yyyymmdd_of_collection_initial  <- as.factor(paste(df$year_of_collection_initial,df$month_of_collection_initial,df$day_of_collection_initial,sep = ""))
df$yyyymmdd_of_collection_final    <- as.factor(paste(df$year_of_collection_final,df$month_of_collection_final,df$day_of_collection_final,sep = ""))

# Replacing specific names
sort(unique(df$name_contact))

df$name_contact <- gsub("K\xe9vin_Tougeron", "Kevin_Tougeron", df$name_contact)
df$name_contact <- gsub("Luis_Casta\xf1eda", "Luis_Castaneda", df$name_contact)
df$name_contact <- gsub("Adam _Hermaniuk", "Adam_Hermaniuk", df$name_contact)
df$name_contact <- gsub("Shameer_Kodambiyakamenna", "K_S_Shameer", df$name_contact) # personal request of data contributor
df$name_contact <- gsub("Iris_vandePol", "Iris_van_de_Pol", df$name_contact)
df$name_contact <- gsub("Wilco_Verberk", "Wilco_CEP_Verberk", df$name_contact)

sort(unique(df$name_contact))

# Replacing specific e-mails
sort(unique(df$email_contact))

df$email_contact <- gsub("K\xe9vin.tougeron@umons.ac.be", "kevin.tougeron@umons.ac.be", df$email_contact)

sort(unique(df$email_contact))

# Replacing specific types of reference
sort(unique(df$type_of_reference))
df$type_of_reference[is.na(df$type_of_reference)] <- as.factor("primary")
df$type_of_reference[df$type_of_reference ==  "1"] <- "primary"
df$type_of_reference[df$type_of_reference ==  "primary_unpublished"] <- "primary"
sort(unique(df$type_of_reference))

# Replacing specific doi of publications
sort(unique(df$doi_publication))

df$doi_publication[df$doi_publication == "10.1242/jeb.133785"]                                              <- "https://doi.org/10.1242/jeb.133785"
df$doi_publication[df$doi_publication == "https://doi:10.3389/fevo.2021.659363"]                            <- "https://doi.org/10.3389/fevo.2021.659363"
df$doi_publication[df$doi_publication == "https://royalsocietypublishing.org/doi/10.1098/rspb.2021.2077"]   <- "https://doi.org/10.1098/rspb.2021.2077"
df$doi_publication[df$doi_publication == "https://doi.org//10.1242/jeb.133785"]                             <- "https://doi.org/10.1242/jeb.133785"
df$doi_publication[is.na(df$doi_publication)]                                                               <- "not published yet"

sort(unique(df$doi_publication))

# Replacing specific doi of datasets
sort(unique(df$doi_dataset))

df$doi_dataset[is.na(df$doi_dataset)]                                                                       <- NA
df$doi_dataset[df$doi_dataset == "http://doi.org/10.5281/zenodo.7273648"]                                   <- "https://doi.org/10.5281/zenodo.7273648"

sort(unique(df$doi_dataset))

# Replacing specific comment reference

df$comments_reference[is.na(df$comments_reference)]                                                         <- NA
df$comments_reference[df$comments_reference == "primary"]                                                   <- NA
df$comments_reference[df$comments_reference == "We added the string_https://doi.org/_to the provided DOIs"] <- NA
df$comments_reference[df$comments_reference == "Unpublished dataset"]                                       <- "dataset not published"

# Replacing specific origin and experiment location
sort(unique(df$origin))
df$origin[df$origin ==  "mass rearing company"] <- "laboratory"
df$origin[df$origin ==  "lab"] <- "laboratory"
df$experiment_location[df$experiment_location ==  "lab"] <- "laboratory"
sort(unique(df$origin))
sort(unique(df$experiment_location))

# Fixing specific levels
unique(df$maintenance_photoperiod)
df$maintenance_photoperiod[df$maintenance_photoperiod ==  "16l_8D"] <- "16L_8D"
df$maintenance_photoperiod[df$maintenance_photoperiod ==  "16L_08D"] <- "16L_8D"
df$maintenance_photoperiod[df$maintenance_photoperiod ==  "12D_12L"] <- "12L_12D"
unique(df$maintenance_photoperiod)

df$acclimated[df$acclimated ==  "not"] <- "no"
df$strategy_of_protection[df$strategy_of_protection ==  "protected (carried by the female"] <- "protected (carried by the female, or attached to a substrate or floating in clumped masses)"

df$sex[df$sex ==  "or attached to a substrate or floating in clumped masses)"] <- NA
df$sex[df$sex ==  ""] <- NA
df$sex[df$sex ==  "indeterminate"] <- NA

# renaming specific life stages

df$life_stage_general_final[df$life_stage_general_final ==  "matamorphosis"] <- "metamorphosis"
df$life_stage_specific_initial[df$life_stage_specific_initial ==  ""] <- NA
df$life_stage_specific_initial[df$life_stage_specific_initial ==  "Gosner developmental stage 25 (about 3 days after hatching)"] <- "Gosner stage 25"
df$life_stage_specific_initial[df$life_stage_specific_initial ==  "Gosner developmental stage 25 (about 2 days after hatching)"] <- "Gosner stage 25"
df$life_stage_specific_final[df$life_stage_specific_final ==  "Gosner developmental stage 42"] <- "Gosner stage 42"
df$life_stage_specific_final[df$life_stage_specific_final ==  ""] <- NA
df$life_stage_specific_final[df$life_stage_specific_final ==  "emergence of the adult from the host pupae"] <- "adult hatching from the host pupae"

# renaming specific body size types and its units
df$size_type[df$size_type ==  "fresh mass"] <- "fresh body mass"
df$size_type[df$size_type ==  "dry mass"] <- "dry body mass"
df$size_type[df$size_type ==  ""] <- NA
df$parental_size_type[df$parental_size_type ==  "fresh mass"] <- "fresh body mass"
df$parental_size_type[df$parental_size_type ==  "FM"] <- "fresh body mass"
df$parental_size_type[df$parental_size_type ==  "dry mass"] <- "dry body mass"
df$parental_size_type[df$parental_size_type ==  "Tibia length"] <- "tibia length"
df$parental_size_type[df$parental_size_type ==  "Left hind leg tibia length"] <- "tibia length"
df$offspring_size_type[df$offspring_size_type ==  "dry mass"] <- "dry body mass"

df$size_units[df$size_units ==  ""] <- NA
df$size_units[df$size_units ==  "grams"] <- "gram"

# renaming specific comments
df$comments_trait[df$comments_trait ==  "Species is reproductively active during 1 year"] <- "Species is reproductively active for one year"
df$comments_trait[df$comments_trait ==  "Species is reproductivery active for 1 year"]    <- "Species is reproductively active for one year"
df$comments_trait[df$comments_trait ==  "Species is reproductivery active for a year"]    <- "Species is reproductively active for one year"
df$comments_trait[df$comments_trait ==  "Species is reproductively active for 1 year"]    <- "Species is reproductively active for one year"
df$comments_trait[df$comments_trait ==  ""] <- NA


df$comments_trait[df$comments_trait ==  "triploid froglet"]    <- "triploid individual"
df$comments_trait[df$comments_trait ==  "Triploid_individual"] <- "triploid individual"
df$comments_trait[df$comments_trait ==  "triploid tadpole"]    <- "triploid individual"
df$comments_trait[df$comments_trait ==  "diploid froglet"]     <- "diploid individual"
df$comments_trait[df$comments_trait ==  "diploid tadpole"]     <- "diploid individual"
df$comments_trait[df$comments_trait ==  "Diploid_individual"]  <- "diploid individual"


df$acclimation_food_type[df$acclimation_food_type ==  ""]  <- NA
df$acclimation_food_type[df$acclimation_food_type ==  "pellets (0.2 mm Ridley Aqua-feeds, Melbourne, Australia) and bloodworms (Orca, Nijimi Pty Ltd, Sydney, Australia)"]  <- "Pelleted diet- Ridley Aqua-feeds and and bloodworms (Orca, Nijimi Pty Ltd, Sydney, Australia)"
df$acclimation_food_type[df$acclimation_food_type ==  "Fish flakes (Tetramin, VA, USA)_supplemented with live Artemia salina nauplii every 3\x964 days"]  <- "Fish flakes (Tetramin, VA, USA) supplemented with live Artemia salina nauplii every three to four days"

df$comments_experimental_conditions[df$comments_experimental_conditions ==  ""]  <- NA

df$reproductive_stage[df$reproductive_stage ==  "mature adults"]  <- "mature"
df$reproductive_stage[df$reproductive_stage ==  "virgin female" ]  <- "virgin"

unique(df$metabolic_rate_type)
df$metabolic_rate_type[df$metabolic_rate_type ==  "maximum" ]  <- "active"
df$metabolic_rate_type[df$metabolic_rate_type ==  "basal" ]  <- "resting"
df$metabolic_rate_type[df$metabolic_rate_type ==  "burrowing" ]  <- "active"
unique(df$metabolic_rate_type)


df$sensor_type[df$sensor_type ==  "The two channel oxygen analyzer (S-3A/II N 37 M, Ametek, Pittsburgh, PA)"]  <- "zirconia-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "fibre-optic cable connected to a Fibox 3 reader (Presens) was fitted to the oxygen flow-through cell"]  <- "fiber optic-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "fiber optic oxygen sensor"]  <- "fiber optic-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "Microx TX3 fiberoptic oxygenmeter"]  <- "fiber optic-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "A fibre-optic cable connected to a Fibox 3 reader (Presens, Regensburg, Germany) was fixed to the oxygen flow-through cell"]  <- "fiber optic-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "oxygen meter Witrox 4 Loligo Systems"]  <- "fiber optic-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "Loligo Witrox"]  <- "fiber optic-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "Clark-type microelectrodes"]  <- "oxygen electrode"
df$sensor_type[df$sensor_type ==  "optical oxygen probe"]  <- "fiber optic-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "optical sensor spot"]  <- "fiber optic-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "fluorescence-based oxygen reading device (SDR SensorDish Reader; PreSens, Regensburg, Germany)"]  <- "fluorescence-based oxygen analyzer"
df$sensor_type[df$sensor_type ==  "oxygen electrode (Yellow Spring Instruments)"]  <- "oxygen electrode"
df$sensor_type[df$sensor_type ==  "Clark-type electrode"]  <- "oxygen electrode"
df$sensor_type[df$sensor_type ==  "Carbon dioxide analyzer sable system"]  <- "carbon dioxide analyzer"
df$sensor_type[df$sensor_type ==  "infrared CO2 analyzer Li-6251, LI-COR"]  <- "infrared-based carbon dioxide analyzer"
df$sensor_type[df$sensor_type ==  "infrared CO2 analyzer Li-6251, LI-COR "]  <- "infrared-based carbon dioxide analyzer"
df$sensor_type[df$sensor_type ==  "infrared CO2 gas analyser (LI-820, LI-COR Biosciences Inc)"]  <- "infrared-based carbon dioxide analyzer"

# Convert latitude to decimal degrees
df$lat_dec <- df$lat_gg + df$lat_mm/60 + df$lat_ss/3600
df$lat_dec <- ifelse(df$lat_gg < 0, -df$lat_dec, df$lat_dec)

# Combine values from lat_dec and lat_gg_mm_ss into a single column
df$lat_decimal <- coalesce(df$lat_dec, df$lat_gg_mm_ss)

# Convert longitude to decimal degrees
df$long_dec <- df$long_gg + df$long_mm/60 + df$long_ss/3600
df$long_dec <- ifelse(df$long_gg < 0, -df$long_dec, df$long_dec)

# Combine values from long_dec and long_gg_mm_ss into a single column
df$long_decimal <- coalesce(df$long_dec, df$long_gg_mm_ss)

names(df)
# Define the desired column order
df_cleaned <- df[, c(
  "dataset_id", "date_of_contribution", "name_contact", "email_contact", "type_of_reference","doi_dataset", "doi_publication",
  "comments_reference", "species_reported", "comments_taxonomy", "realm_general", "realm_specific",
  "elevation_of_collection", "depth_of_collection", "origin", "location_description", "comments_location", 
  "lat_decimal","long_decimal",
  "year_of_collection_initial", "year_of_collection_final",
  "yyyymmdd_of_collection_initial", "yyyymmdd_of_collection_final", "comments_timing",
  "experiment_location","maintained", "maintenance_duration_days", "maintenance_duration_generations",
  "maintenance_temperature", "maintenance_photoperiod", "maintenance_humidity", "maintenance_oxygen",
  "maintenance_carbon_dioxide", "trait_name", 
  "maintenance_salinity", "maintenance_ph","maintenance_oxygen_units", "maintenance_carbon_dioxide_units", "maintenance_food_type",
  "acclimated","acclimation_duration", "acclimation_temperature",
  "acclimation_salinity", "acclimation_ph", "acclimation_oxygen", "acclimation_carbon_dioxide",
  "acclimation_photoperiod", "acclimation_humidity", "acclimation_oxygen_units",
  "acclimation_carbon_dioxide_units", "acclimation_food_type",
  "test_temperature", "test_oxygen", "test_carbon_dioxide",
  "test_oxygen_units", "test_carbon_dioxide_units", "test_photoperiod", "test_humidity",
  "comments_experimental_conditions",  "test_food_type",
  "test_salinity", "test_ph",
  "strategy_of_protection", "sex", "life_stage_general_initial", "life_stage_general_final",
  "life_stage_specific_initial", "life_stage_specific_final",
  "life_stage_general", "life_stage_specific", 
  "size_type", "size_units", "size_value_initial", "size_value_final","size_value",
  "parental_size_type", "parental_size_units", "parental_size_value",
  "parental_age", "parental_age_units", 
  "mating", "method_type", "fecundity_temporal_unit","reproductive_stage", 
  "offspring_developmental_stage", "offspring_size_type", "offspring_size_units", "offspring_size_value",
  "metabolic_rate_type","acclimation_chamber", "fasting_time", "sensor_type", "respiration_volume", "delay_time",
  "respiratory_chamber_material", "incubation_time", "respirometry_type", "breathing_mode",
  "trait_value", "trait_unit", "comments_trait","trait_error_estimate", "trait_error_type", "sample_size"
)]

# Save the cleaned data frame to a new file
write.csv(df_cleaned, "../outputs/2_1_ShareTrait_Database.csv", row.names = FALSE)

# saving session information with all packages versions for reproducibility purposes
sink("../outputs/2_1_standarisation_R_session.txt")
sessionInfo()
sink()
# END OF SCRIPT