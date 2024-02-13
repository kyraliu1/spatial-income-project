
# county level race and ethnicity

wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
setwd(wd)
library(ipumsr)
library(dplyr)
library(terra)
library(geodata)


# Persons by Single Race/Ethnicity [5]
# Selected year(s): 1980, 1990, 2000, 2010, 2020
# Code:CY6


ethnicity_race = function(level,directory = './ipums',save_to = T){

nhdirs <- list.dirs(path = "./ipums",recursive = F,full.names = T)
da <- list()
p = 0
cind= grep('23',nhdirs)
cind2 = grep('0002',nhdirs)

  da[[1]] = read.csv(list.files(nhdirs[cind],pattern = paste0(level,'.csv'),full.names = T))

# AA: Persons: Not Hispanic or Latino ~ White (single race reported or, since 2000, race combinations likely to report this single race)
# AB: Persons: Not Hispanic or Latino ~ Black or African American (single race reported or, since 2000, race combinations likely to report this single race) 
# AC: Persons: Not Hispanic or Latino ~ American Indian, Alaska Native, Asian, and Pacific Islander (single race reported or, since 2000, race combinations likely to report one of these single races)
# AD: Persons: Not Hispanic or Latino ~ Some Other Race (single race reported or, since 2000, race combinations likely to report this single race)
# AE: Persons: Hispanic or Latino
drace = da[[1]] #CY6


#dd <- merge(drace,tpop,by = intersect(names(drace),names(tpop)))
dd <- drace
#add total level pop for each year
dd$pop80 <- dd$CY6AA1980 + dd$CY6AB1980 + dd$CY6AC1980 + dd$CY6AD1980+ dd$CY6AE2000

dd$pop90 <- dd$CY6AA1990 + dd$CY6AB1990 + dd$CY6AC1990 + dd$CY6AD1990 + dd$CY6AE2000

dd$pop00 <- dd$CY6AA2000 + dd$CY6AB2000 + dd$CY6AC2000 + dd$CY6AD2000 + dd$CY6AE2000
dd$pop10 <- dd$CY6AA2010 + dd$CY6AB2010 + dd$CY6AC2010 + dd$CY6AD2010 + dd$CY6AE2010
dd$pop20 <- dd$CY6AA2020 + dd$CY6AB2020 + dd$CY6AC2020 + dd$CY6AD2020 + dd$CY6AE2020
# pwhite

dd$pwhite80 <- dd$CY6AA1980/dd$pop80
dd$pwhite90 <- dd$CY6AA1990/dd$pop90
dd$pwhite00 <- dd$CY6AA2000/dd$pop00
dd$pwhite10 <- dd$CY6AA2010/dd$pop10
dd$pwhite20 <- dd$CY6AA2020/dd$pop20

# pblack
dd$pblack80 <- dd$CY6AB1980/dd$pop80
dd$pblack90 <- dd$CY6AB1990/dd$pop90
dd$pblack00 <- dd$CY6AB2000/dd$pop00
dd$pblack10 <- dd$CY6AB2010/dd$pop10
dd$pblack20 <- dd$CY6AB2020/dd$pop20

# pa_nat
dd$pa_nat80 <- dd$CY6AC1980/dd$pop80
dd$pa_nat90 <- (dd$CY6AC1990)/dd$pop90
dd$pa_nat00 <- (dd$CY6AC2000)/dd$pop00
dd$pa_nat10 <- (dd$CY6AC2010)/dd$pop10
dd$pa_nat20 <- (dd$CY6AC2020)/dd$pop20

# other
dd$pother80 <- dd$CY6AD1980/dd$pop80
dd$pother90 <- dd$CY6AD1990/dd$pop90
dd$pother00 <- dd$CY6AD2000/dd$pop00
dd$pother10 <- dd$CY6AD2010/dd$pop10
dd$pother20 <- dd$CY6AD2020/dd$pop20

# hispanic/ latinx
dd$phisp80 <- dd$CY6AE1980/dd$pop80
dd$phisp90 <- dd$CY6AE1990/dd$pop90
dd$phisp00 <- dd$CY6AE2000/dd$pop00
dd$phisp10 <- dd$CY6AE2010/dd$pop10
dd$phisp20 <- dd$CY6AE2020/dd$pop20

if (save_to==T){
  finame = paste0(directory,'/',level,'_level_ethnicity')
saveRDS(dd, file = finame)
}
return(dd)
}

# for each level ----------------------------------------------------------


county = ethnicity_race('county')

state = ethnicity_race('state')
nation = ethnicity_race('nation')
tract = ethnicity_race('tract')


