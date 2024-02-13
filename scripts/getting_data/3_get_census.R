# getting data from census api, uses tidy census and census api package
# we ended up getting these again, all from ipums because they have 
# more time available.

# setup -------------------------------------------------------------------


this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
  wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
  wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}
setwd(wd)
library(terra)
library(tidycensus)
library(censusapi)

#census_api_key("cd0b04aed8602a07aaac364068f6791863fe2bac", install=TRUE)
#census_api_key("your key",install = TRUE)
ckey <- Sys.getenv("CENSUS_API_KEY")
census_api_key(ckey)
options(tigris_use_cache = TRUE)

# defining desired variables ----------------------------------------------


# Retrieve income-related variables for multiple years at the census tract level
racevar <- c("B02001_001","B02001_002", "B02001_003", "B02001_004",
             "B02001_005","B02001_006","B02001_007", "B02001_008")
# B02001_001: Total population
# B02001_002: Population of one race: White alone
# B02001_003: Population of one race: Black or African American alone
# B02001_004: Population of one race: American Indian and Alaska Native alone
# B02001_005: Population of one race: Asian alone
# B02001_006: Population of one race: Native Hawaiian and Other Pacific Islander alone
# B02001_007: Population of one race: Some Other Race alone
# B02001_008: Population of two or more races
incvar <- c("B19001_001", "B19001_002", "B19001_003", "B19001_004",
            "B19001_005", "B19001_006", "B19001_007", "B19001_008",
            "B19001_009", "B19001_010", "B19001_011", "B19001_012",
            "B19001_013", "B19001_014", "B19001_015","B19001_016",
            "B19001_017","B06011_001")
# B19001_001: Total number of households with income
# B19001_002: Number of households with income less than $10,000
# B19001_003: Number of households with income between $10,000 and $14,999
# B19001_004: Number of households with income between $15,000 and $19,999
# B19001_005: Number of households with income between $20,000 and $24,999
# B19001_006: Number of households with income between $25,000 and $29,999
# B19001_007: Number of households with income between $30,000 and $34,999
# B19001_008: Number of households with income between $35,000 and $39,999
# B19001_009: Number of households with income between $40,000 and $44,999
# B19001_010: Number of households with income between $45,000 and $49,999
# B19001_011: Number of households with income between $50,000 and $59,999
# B19001_012: Number of households with income between $60,000 and $74,999
# B19001_013: Number of households with income between $75,000 and $99,999
# B19001_014: Number of households with income between $100,000 and $124,999
# B19001_015: Number of households with income between $125,000 and 124,999
# B19001_016: Number of households with income between $150,000 and 199,999
# B19001_017: Number of households with income of $200,000 or more


# B06011_001: Median income in past 12 months
# acs2018 <- get_acs(geography = "county",
#                     variables = c(incvar, racevar), geometry = TRUE,
#                     year =2018, output="wide")
# v <- vect(acs2018)
e <- ext(-125, -66, 24, 50)
# v <- crop(v, e)
# 
# v$pblack = v$B02001_003E / v$B02001_001E
# plot(v, "pblack", border=NA, breaks=c(0,0.025,0.05,.1,.2,.5,1))
# 
# 
# popblk <- acs2018[acs2018$variable=="B02001_003",]
# v <- vect(popblk)
# v$loge <- log(v$estimate + 1)
# #plot(v, "loge", border=NA)
# 
# 
# 
#  #plot(acs2018$geometry[acs2018$variable=="B19001_002"])

states <- c(state.abb, "DC")
tract2018=list()
if(!file.exists("./censusdata/acs5_2018_tract")){
  dir.create("./censusdata",FALSE,FALSE)
for(i in 1:length(states)){
tract2018[[i]] <- get_acs(geography = "tract",
                    variables = c(incvar, racevar), geometry = TRUE,
                    year =2018, output="wide", state=states[i])
}
tract2018 <- do.call(rbind,tract2018)
tract2018 <- vect(tract2018)
  saveRDS(tract2018,"./censusdata/acs5_2018_tract")
}

v <- readRDS("./censusdata/acs5_2018_tract")
# remove empty geometries that caused trouble
v <- na.omit(v, geom=TRUE)
v <- crop(v,e)
v$pblack = v$B02001_003E / v$B02001_001E
v$pwhite = v$B02001_002E/v$B02001_001E
plot(v, "pblack", border=NA, breaks=c(0,0.025,0.05,.1,.2,.5,1))


v$lowest <- v$B19001_002E/v$B19001_001E
plot(v, "lowest", border = NA)
v$highest = v$B19001_015E/v$B02001_001E
plot(v, "highest",border = NA, breaks=c(0,0.025,0.05,.1,.2,.3))

plot(v,"B06011_001E",border = NA,breaks = c(0,10000,seq(15000,150000,10000)),
     main = "median income")

### 2010
tract2010 <- list()
if(!file.exists("./censusdata/acs5_2010_tract")){
  dir.create("./censusdata",FALSE,FALSE)
  for(i in 1:length(states)){
    tract2010[[i]] <- get_acs(geography = "tract",
                              variables = c(incvar, racevar), geometry = TRUE,
                              year =2010, output="wide", state=states[i])
  }
  tract2010 <- do.call(rbind,tract2010)
  tract2010 <- vect(tract2010)
  saveRDS(tract2010,"./censusdata/acs5_2010_tract")
}

d10 <- readRDS("./censusdata/acs5_2010_tract")
#d10 <- crop(d10,e)
d10$pblack = d10$B02001_003E / d10$B02001_001E
d10$pwhite = d10$B02001_002E/d10$B02001_001E
# par(mfrow = c(2,1))
# plot(v,"B06011_001E",border = NA,breaks = c(0,10000,seq(15000,150000,10000)),
#      main = "2018")
# plot(d10,"B06011_001E",border = NA,breaks = c(0,10000,seq(15000,150000,10000)),
#      main = "2010")
# title("median income")

par(mfrow = c(2,1))
plot(v,"B06011_001E",border = NA,type = "continuous",
     main = "2018")
plot(d10,"B06011_001E",border = NA,type = "continuous",
     main = "2010")
title("median income")


par(mfrow = c(2,1))
plot(v,"pblack",border = NA,type = "continuous",
     main = "proportion black 2018")
plot(d10,"pblack",border = NA,type = "continuous",
     main = "proportion black 2010")



par(mfrow = c(2,1))
plot(v,"pwhite",border = NA,type = "continuous",
     main = "proportion white 2018")
plot(d10,"pwhite",border = NA,type = "continuous",
     main = "proportion white 2010")
