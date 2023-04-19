this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
  wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
  wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}


library(tidycensus)
library(censusapi)
ckey <- Sys.getenv("CENSUS_API_KEY")
census_api_key(ckey)
# Retrieve income-related variables for multiple years at the census tract level
racevar <- c("B02001_001","B02001_002", "B02001_003",
             "B02001_004","B02001_005","B02001_006","B02001_007",
             "B02001_008")
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
            "B19001_013", "B19001_014", "B19001_015")
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
# B19001_015: Number of households with income $125,000 or more
acs_data <- get_acs(geography = "county",
                    variables = c(incvar, racevar),
                    year =2018)




