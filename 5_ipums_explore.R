# looking a little bit at first extract. not in use later

# setup
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
  wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
  wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}
setwd(wd)
library(ipumsr)

# laod data
dpath = "./ipums/usa_00001.xml"
d = read_ipums_micro(dpath)


# 
# datacity <- d[d$CITY != 0,]
# nocity <- d[d$CITY == 0,]
# removing empty
inc <-d[d$INCTOT!=9999999 & d$INCTOT!=0 &d$INCTOT != 1,c("STATEFIP","INCTOT","SEX","AGE","RACE")]
states <- attr(d$STATEFIP,"labels")
race <- attr(d$RACE,"labels")
#inc[which.max(inc$INCTOT),]
hist(inc$INCTOT, breaks = 100)
hist(inc$INCTOT[inc$RACE==1], breaks = 100, main = "white", 
     xlab = "income")

