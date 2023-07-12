# Code adapted by FP Leiva from: 

# Lenoir, J., Bertrand, R., Comte, L. et al. Species better track climate
# warming in the oceans than on land. Nat Ecol Evol 4, 1044–1059 (2020).
# https://doi.org/10.1038/s41559-020-1198-2

rm(list=ls()) #clear your work environment

# Libraries
library(dplyr)
library(Hmisc)
library(tidyverse)
library(stringr)
library(rgbif)
library(taxize)
library(plyr)

# Functions
source("../outputs/Retrieve_taxonomy.R")

# load data
dat<-read.csv("../outputs/2_2_ShareTrait_database.csv",sep = ",",header = TRUE)

species = as.character(dat[,"species_reported"])

# create vector name
mynames <-na.omit(unique(ifelse(is.na(species),as.character(dat$genus),paste(dat$genus,species))))      

# check number of names
length(mynames)

# STEP 1: retrieve names using NCBI

gbiftestout_df <- llply(mynames, NCBI_classif,db='ncbi',.progress = "text")
dfnames.ncbi  = sapply(gbiftestout_df,as.data.frame)
dfnames.ncbi  = sapply(dfnames.ncbi,unlist)
dfnames.ncbi = t(dfnames.ncbi)
dfnames.ncbi  = data.frame(dfnames.ncbi)
colnames(dfnames.ncbi) <- c("phylum", "class", "order", "family","genus","species","species_reported")
dfnames.ncbi  = apply(dfnames.ncbi,2,unlist)
dfnames.ncbi  = apply(dfnames.ncbi,2,as.character)
dfnames.ncbi  = data.frame(dfnames.ncbi)

## 1 -Check for changes in species names
na.omit(cbind(as.character(dfnames.ncbi$species_reported[which(!dfnames.ncbi$species_reported %in% dfnames.ncbi$species)]),
              as.character(dfnames.ncbi$species[which(!dfnames.ncbi$species_reported %in% dfnames.ncbi$species)])))

## 2 -Select unidentified species & remove from the table
unid.ncbi = dfnames.ncbi$species_reported[which(sapply(strsplit(as.character(dfnames.ncbi$species_reported)," "),length)== 2 & is.na(dfnames.ncbi$species))]

## 3 - save file
if (length(unid.ncbi)>0){
  dfnames.ncbi = dfnames.ncbi[-which(sapply(strsplit(as.character(dfnames.ncbi$species_reported)," "),length)== 2 & is.na(dfnames.ncbi$species)),] 
}

dfnames.ncbi = apply(dfnames.ncbi,2,as.character)
write.table(dfnames.ncbi,"../outputs/dfnames.ncbi.txt")

#4 - compile
dfnames.ncbi = read.table("../outputs/dfnames.ncbi.txt",h=T)
source.ncbi = rep('ncbi',nrow(dfnames.ncbi))
taxo_level = ifelse(sapply(strsplit(as.character(dfnames.ncbi$species_reported)," "),length) == 1,"Genus","Species")
finaltab = data.frame(dfnames.ncbi,source.ncbi,taxo_level)
colnames(finaltab)=c("phylum","class","order","family","genus","species","species_reported","source","taxo_level")


#/!\ STOP HERE IF length(unid.ncbi)=0
#/!\ all species have been retrieved no need to go further
length(unid.ncbi)

# STEP 2: names using ITIS

gbiftestout_df.itis <- llply(unid.ncbi, NCBI_classif,db='itis',.progress = "text")

dfnames.itis  = sapply(gbiftestout_df.itis,as.data.frame)
dfnames.itis  = sapply(dfnames.itis,unlist)
dfnames.itis = t(dfnames.itis)
dfnames.itis  = data.frame(dfnames.itis)
colnames(dfnames.itis) <- c("phylum", "class", "order", "family","genus","species","species_reported")
dfnames.itis  = apply(dfnames.itis,2,unlist)
dfnames.itis  = data.frame(dfnames.itis)

## 1 -Check for changes in species names
na.omit(cbind(as.character(dfnames.itis$species_reported[which(!dfnames.itis$species_reported %in% dfnames.itis$species)]),
              as.character(dfnames.itis$species[which(!dfnames.itis$species_reported %in% dfnames.itis$species)])))

## 2 - Select unidentified species
unid.itis = dfnames.itis$species_reported[which(sapply(strsplit(as.character(dfnames.itis$species_reported)," "),length)== 2 & is.na(dfnames.itis$species))]

## 3 - save file
if (length(unid.itis)>0){
  dfnames.itis = dfnames.itis[-which(sapply(strsplit(as.character(dfnames.itis$species_reported)," "),length)== 2 & is.na(dfnames.itis$species)),] 
}

write.table(dfnames.itis,"../outputs/dfnames.itis.txt")

#4 - compile
dfnames.itis = read.table("../outputs/dfnames.itis.txt",h=T)
source.itis = rep('itis',nrow(dfnames.itis))
taxo_level = ifelse(sapply(strsplit(as.character(dfnames.itis$species_reported)," "),length) == 1,"Genus","Species")
dfnames.itis=data.frame(dfnames.itis,source.itis,taxo_level)
colnames(dfnames.itis)=c("phylum","class","order","family","genus","species","species_reported","source","taxo_level")
finaltab = rbind(finaltab,dfnames.itis)

#/!\ STOP HERE IF length(unid.ncbi)= 0
#/!\ all species have been retrieved no need to go further

length(unid.itis)

# STEP 3:  retrieve names using GBIF
#but not the classif (names are easier to retrieve using GBIF but the clasif has many problems)

gbiftestout_df.gbif <- llply(as.character(unid.itis), GB_classif,.progress = "text")

dfnames.gbif  = sapply(gbiftestout_df.gbif,as.data.frame)
dfnames.gbif  = sapply(dfnames.gbif,unlist)
dfnames.gbif = t(dfnames.gbif)
dfnames.gbif  = data.frame(dfnames.gbif)
colnames(dfnames.gbif) <- c("phylum", "class", "order", "family","genus","species","species_reported")
dfnames.gbif  = apply(dfnames.gbif,2,unlist)
dfnames.gbif  = apply(dfnames.gbif,2,as.character)
dfnames.gbif  = data.frame(dfnames.gbif)


## 1 - Check for changes in species names
na.omit(cbind(as.character(dfnames.gbif$species_reported[which(!dfnames.gbif$species_reported %in% dfnames.gbif$species)]),
              as.character(dfnames.gbif$species[which(!dfnames.gbif$species_reported %in% dfnames.gbif$species)])))

## 2 - Save file
write.table(dfnames.gbif,"../outputs/dfnames.gbif.txt")

dfnames.gbif = read.table("../outputs/dfnames.gbif.txt",h=T)
source.gbif = rep('gbif',nrow(dfnames.gbif))

## 3 -check the info against ncbi (with new names from GBIF)
check.ncbi <- llply(dfnames.gbif$species, NCBI_classif,db='ncbi',.progress = "text")
dfnames.check.ncbi  = sapply(check.ncbi,as.data.frame)
dfnames.check.ncbi  = sapply(dfnames.check.ncbi,unlist)
dfnames.check.ncbi = t(dfnames.check.ncbi)
dfnames.check.ncbi  = data.frame(dfnames.check.ncbi)
colnames(dfnames.check.ncbi) <- c("phylum", "class", "order", "family","genus","species","species_reported")
dfnames.check.ncbi  = apply(dfnames.check.ncbi,2,unlist)
dfnames.check.ncbi  = apply(dfnames.check.ncbi,2,as.character)
dfnames.check.ncbi  = data.frame(dfnames.check.ncbi)

## 4 - Check for changes in species names
na.omit(cbind(as.character(dfnames.check.ncbi$species_reported[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species)]),as.character(dfnames.check.ncbi$species[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species)])))

##5 - change the names in DB
dfnames.gbif = apply(dfnames.gbif,2,as.character)
dfnames.gbif[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),1] = as.character(dfnames.check.ncbi[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),1])
dfnames.gbif[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),2] =  as.character(dfnames.check.ncbi[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),2])
dfnames.gbif[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),3] =  as.character(dfnames.check.ncbi[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),3])
dfnames.gbif[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),4] =  as.character(dfnames.check.ncbi[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),4])
dfnames.gbif[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),5] =  as.character(dfnames.check.ncbi[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),5])
dfnames.gbif[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),6] =  as.character(dfnames.check.ncbi[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F),5])

source.gbif[which(!dfnames.check.ncbi$species_reported %in% dfnames.check.ncbi$species & is.na(dfnames.check.ncbi$species) == F)]  = 'ncbi'

##6 -  check unidentified species  : mainly mistakes in names
dfnames.gbif=as.data.frame(dfnames.gbif)
dfnames.gbif$species_reported[which(is.na(dfnames.gbif$species) & is.na(dfnames.gbif$genus))]

#7 - compile
taxo_level = ifelse(sapply(strsplit(as.character(dfnames.gbif$species_reported)," "),length) == 1,"Genus","Species")
dfnames.gbif=data.frame(dfnames.gbif,source.gbif,taxo_level)
colnames(dfnames.gbif)=c("phylum","class","order","family","genus","species","species_reported","source","taxo_level")
finaltab = rbind(finaltab,dfnames.gbif)

#finaltab gives for each name entered in dat (finaltab$species_reported) the full classification from phylum to species
#species=NA have not been recognized as valid species and need to be search elsewhere or removed
#classification sometimes has to be completed

write.csv(finaltab,"../outputs/2_3_harmonized_taxonomy_ShareTrait_DataBase.csv",row.names = F)

# saving session information with all packages versions for reproducibility purposes
sink("../outputs/2_3_harmonize_taxonomy_R_session.txt")
sessionInfo()
sink()
# END OF SCRIPT