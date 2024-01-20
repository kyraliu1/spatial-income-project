# initial testing of ipums extract, gets some income, race, and migration data
# not in use later
# setup
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
  wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
  wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}
setwd(wd)
library(ipumsr)
#set_ipums_api_key("key", save = TRUE)
# api_key = Sys.getenv("IPUMS_API_KEY")

# defining extract
exdef <- define_extract_usa("income data extract 1", 
                            samples = c("us2021c","us2016c"),
                            variables = c("REGION", "STATEFIP","COUNTYFIP",
                                          "CITY","MET2013",
                                          "RACE","HISPAN","BPL","MIGPLAC1","MIGRATE1",
                                          "MULTYEAR","EDUC", "RACAMIND",
                                        "RACASIAN","RACBLK","RACPACIS","RACWHT",
                                        "RACOTHER","RACNUM","SEX","AGE",
                                        "INCTOT","FTOTINC","INCWAGE","CPI99",
                                        "POVERTY"))
# submitting extract
subex <- submit_extract(exdef)


# FTOTINC: total family income
# race response changed in 2000 and again in 2020
# migplace1 = state or country of resident 1 year ago
# migrate1 = whether the person had changed residence since a reference point 1 year ago
# cpi99 = cpi-u adjustment factor to 1999 dollars  


# getting status of extract
exinf <- get_extract_info("usa:1")
status <- exinf$status

readyex <- wait_for_extract(exinf)
# when extract is ready, download
dir.create("./ipums",FALSE,FALSE)
if (!file.exists("./ipums/usa_00001.xml")){
dpath <- download_extract(readyex, download_dir ="./ipums",overwrite = TRUE)
}else {dpath = "./ipums/usa_00001.xml"}
data = read_ipums_micro(dpath)

# HHWT: how many households in U.S. population represented by a given household in sample
# SEX: male = 1, female = 2
# RACE:
  # 1	White	
  # 2	Black/African American
  # 3	American Indian or Alaska Native	
  # 4	Chinese
  # 5	Japanese
  # 6	Other Asian or Pacific Islander
  # 7	Other race, nec
  # 8	Two major races	
  # 9	Three or more major races
# HISPAN:
  # 0	Not Hispanic	
  # 1	Mexican	
  # 2	Puerto Rican	
  # 3	Cuban	
  # 4	Other	
  # 9	Not Reported

