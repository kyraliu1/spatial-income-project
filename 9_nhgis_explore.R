# i used this mostly to see how the data is formatted, and take a very quick look


# setup
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
  wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
  wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}
setwd(wd)
library(ipumsr)
library(dplyr)
library(terra)
library(geodata)
library(tidycensus)

# nhgis 1: B08: persons by detailed race 1970 - 2010
# nhgis 2: B79: median household income in previous year 1980-2021
# nhgis 3: AB2: median family income in previous year 1980-2021
# nhgis 4: BD5: per capita income in previous year 1980-2021

# nhgis 6: AV0: total population
nhdirs <- list.dirs(path = "./ipums",recursive = F)

race_ctysub = read.csv(list.files(nhdirs[2],pattern = 'sub.csv',full.names = T))
race_county = read.csv(list.files(nhdirs[2],pattern = 'county.csv',full.names = T))
race_tract = read.csv(list.files(nhdirs[2],pattern = 'tract.csv',full.names = T))

# AA: Persons: White (single race) 
# AB: Persons: Black or African American (single race)
# AC: Persons: American Indian and Alaska Native tribes excluding Eskimo and Aleut
# AD: Persons: Asian--Japanese (single race)
# AE: Persons: Asian--Chinese, including Taiwanese (single race)
# AF: Persons: Asian--Filipino (single race)
# AG: Persons: Asian--Korean (single race)
# AH: Persons: Native Hawaiian and Other Pacific Islander--Polynesian--Hawaiian (single race)
# AI: Persons: Some Other Race (single race) (includes Other Asian and Pacific Islander, Eskimo, Aleut)
# AJ: Persons: Two or More Races -- only 2000 & 2010


# tracts <- readRDS("./censusdata/acs5_2018_tract")
# tracts <- tracts[,1:2]
# 
# tracts$NAME2010 <- sapply(strsplit(tracts$NAME, ','), function(x) x[1])
# 
# race_tract$GEOID <- substr(race_tract$NHGISCODE,2,14)

#rt <- merge(tracts,race_tract,'NAME2010')

sfi <- list.files('./ipums',pattern = 'shape',full.names = T)
sfi <- unzip(sfi,exdir = './ipums/bounds')

unz <- sapply(sfi,  FUN = 'unzip',exdir = './ipums/bounds')

sfi <- list.files('./ipums/bounds',pattern = '.shp$',full.names = T)

tract1970 = vect(sfi[7])
plot(tract1970)
