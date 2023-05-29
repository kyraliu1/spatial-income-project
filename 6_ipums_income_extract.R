this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
  wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
  wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}
setwd(wd)
library(ipumsr)

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
d$countyname <- as.character((as_factor(d$COUNTYFIP)))

# no income data
nodata = d[,c("INCTOT","INCWAGE","FTOTINC")] == 	9999999 
d[,c("INCTOT","INCWAGE","FTOTINC")][nodata]=-999999

d <- d[!nodata[,1],]
yr <- table(d$YEAR) # data per year where "total personal income" is available

# library(geodata)
# usa <- gadm("usa", level = 1, ".")
# 
# names(usa)[4] <- "statename"
# 
# d <- merge(d,usa,"statename")
