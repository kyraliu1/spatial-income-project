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
library(terra)
counties <- geodata::gadm("usa",level = 2, ".")
counties$state.name <- counties$NAME_1
dir.create("./census",F,F)
# median household income by county: 1969, 79, 89, 99
c1 <- "https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county1.csv"
fc1 <- "./census/household_income_1969_99"
  dl(furl = c1,fpath = fc1)
hc <- read.csv(fc1)

# cleaning
names(hc) <- c("county","y1999","y1989","y1979","y1969")
hc <- hc[-5:-1,-6]
hc <- hc[hc$county!="",] #removing empty

hc <- hc[grepl(",",hc$county),] # removing statewide 
aa <- hc
# numeric values for income
for (i in 2:5){
hc[,i] <- gsub(",","",hc[,i])
hc[,i] <- as.numeric(hc[,i])
}

sta <- data.frame(state.name,state.abb)
counties <- merge(counties, sta, "state.name")
counties$county <- paste0(counties$NAME_2," County, ", counties$state.abb)

hc <- merge(counties,hc, "county")
plot(hc, 17:20, border = NA, main = names(hc)[17:20])

# median family income by county: 1959, 69, 79, 89
c2 <- "https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county2.csv"
fc2 <- "./census/family_income_1959_89"
dl(c2,fc2)
fin <- read.csv(fc2,header=F)
names(fin) <- c("county","y1989","y1979","y1969","y1959")

fin <- fin[-5:-1,]
fin <- fin[fin$county!="",] #removing empty

fin <- fin[grepl(",",fin$county),] # removing statewide 

for (i in 2:5){
  fin[,i] <- gsub(",","",fin[,i])
  fin[,i] <- as.numeric(fin[,i])
}

fin <- merge(counties,fin, "county")
plot(fin, 17:20, border = NA, main = names(fin)[17:20])

# per capita income by county: 1959, 69, 79, 89
c3 <- "https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county3.csv"
fc3 <- "./census/percap_income_1959_89"
dl(c3,fc3)
ppi <- read.csv(fc3,header = F)

names(ppi) <- c("county","y1989","y1979","y1969","y1959")

ppi <- ppi[-5:-1,]
ppi <- ppi[ppi$county!="",] #removing empty

ppi <- ppi[grepl(",",ppi$county),] # removing statewide 

for (i in 2:5){
  ppi[,i] <- gsub(",","",ppi[,i])
  ppi[,i] <- as.numeric(ppi[,i])
}

ppi <- merge(counties,ppi, "county")
plot(ppi, 17:20, border = NA, main = names(fin)[17:20])
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

##### race
pc
