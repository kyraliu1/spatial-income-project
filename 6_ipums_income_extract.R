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

# defining wanted samples and variables
samps <- c("us1950a","us1960a",
           "us1970c","us1970d", "us1980b",
           "us1990b","us2000g","us2010a")
# using 2010 acs rather than 10 pct sample bc more information  

vars <- c("YEAR", "SAMPLE","SERIAL" , "CBSERIAL" , "HHWT", 
          "CLUSTER","REGION","STATEFIP","COUNTYFIP","STRATA" ,"GQ",       
          "PERNUM","PERWT" ,"SEX", "AGE" ,"RACE" ,
          "INCTOT" , "FTOTINC","INCWAGE","POVERTY")

prev <- get_recent_extracts_info_list("usa") # all recent extracts
n <- NA
# checking if this particular extract has been submitted before
for (i in 1:length(prev)){
  if(identical(prev[[c(i,6)]], samps) & identical(prev[[c(i,7)]],
     vars)){
    n <- as.character(prev[[c(i,10)]]) # extract number
    break
  }
  
}


dir.create("./ipums",FALSE,FALSE)

# submit extract if extract has never been submitted
if (is.na(n)){
  def <- define_extract_usa("all income",samples = samps,
                            variables = c("INCTOT","FTOTINC","INCWAGE","POVERTY",
                                          "REGION","STATEFIP","COUNTYFIP", "RACE",
                                          "AGE","SEX"))
  # submitting extract
  submit <- submit_extract(def)
  
  # 1950-2020 ftotinc, inctot, incwage, poverty for decennial 1 percent 
  
  # getting status of extract

  exinf <- get_extract_info(paste0("usa:",n))
  
  status <- exinf$status
  
  readyex <- wait_for_extract(exinf)
}

# download data if it doesn't exist
if (!file.exists(paste0("./ipums/usa_0000",n,".xml"))){
  dpath <- download_extract(readyex, download_dir ="./ipums",overwrite = TRUE)
  
}else {dpath = paste0("./ipums/usa_0000",n,".xml")}

# read data
d = read_ipums_micro(dpath)

d$statename = as.character((as_factor(d$STATEFIP)))
d$cofip <- as.character((as_factor(d$COUNTYFIP)))
d$fip <- paste0(d$STATEFIP,d$cofip)

# no income data
nodata = d[,c("INCTOT","INCWAGE","FTOTINC")] == 	9999999 
d[,c("INCTOT","INCWAGE","FTOTINC")][nodata]=-999999

d <- d[!nodata[,1],]

rm(nodata) # no memory
yr <- table(d$YEAR) # data per year where "total personal income" is available


# where are counties available

dc <- d[d$COUNTYFIP!=0,]
table(d$YEAR)

# # this is being mean to me
# if (!file.exists("./ipums/countyfips.xlsx")){
#   download.file("https://usa.ipums.org/usa/resources/volii/ipums_usa_identified_counties.xlsx",
#                 "./ipums/countyfips.xlsx" )
# }
# doing it manually for now
co <- read.csv("./ipums/countyfips.csv")

# seems like there are 589 counties available
co$fip <- paste0(co$STATEFIP,co$COUNTYFIP)
co <- co[,c(2,6)]
names(co)[1] <- "countyname"
dc <- merge(d, co, "fip")

rm(co) # no memory haha
dc$fullcounty <- paste0(d$countyname,", ",d$statename)


# available counties
# not sure why counties disappeared, maybe they are just not in the data
counties <- unique(d$fullcounty)

grouped <- dc %>% group_by(fullcounty, YEAR)
avg <- grouped %>% summarize(average = mean(INCTOT))
avg2010 <- avg[avg$YEAR==2010,]

usa <- geodata::gadm("usa",2,".")
#names(usa)
usa$fullcounty <- paste0(usa$NAME_2,", ",usa$NAME_1 )


usa<- merge(usa,avg2010,"fullcounty")

plot(usa, "average", border = NA)

# now with states

# state info is available for every row


states <- geodata::gadm("usa", level = 1, ".")

names(states)[4] <- "statename"
gs <- d %>% group_by(statename, YEAR)
stav <- gs %>% summarize(average = mean(INCTOT))
st2010 <- stav[stav$YEAR==2010,]

 st2010 <- merge(states,st2010,"statename")
st2010 <- crop(st2010,ext(usa))
plot(st2010, "average", border = NA) 
