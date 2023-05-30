this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
  wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
  wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}
setwd(wd)
dl <- function(furl, fpath){
  if (!file.exists(fpath)){
    download.file(furl, fpath, mode = "wb")
    }
}
dir.create("./census",F,F)
# median household income by county: 1969, 79, 89, 99
c1 <- "https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county1.csv"
fc1 <- "./census/household_income_1969_99"
  dl(furl = c1,fpath = fc1)
hc <- read.csv(fc1)

# median family income by county: 1959, 69, 79, 89
c2 <- "https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county2.csv"
fc2 <- "./census/family_income_1959_89"
dl(c2,fc2)
fin <- read.csv(fc2,header=F)

# per capita income by county: 1959, 69, 79, 89
c3 <- "https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county3.csv"
fc3 <- "./census/percap_income_1959_89"
dl(c3,fc3)
ppi <- read.csv(fc3,header = F)

# percent change in median household income: 1969, 1979, 1989
c4 <- "https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county4.csv"
fc4 <- "./census/pctchange_median_household_1969_89"
dl(c4,fc4)
pc <- read.csv(fc4,header=F)

# income inequality measures by county: 1990, 2000
c5 <- "https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county5.xls"
fc5 <- "./census/census_income_ineq_1990_2000"
dl(c5, fc5)
#ieq <- read.csv(fc5, header = F)
