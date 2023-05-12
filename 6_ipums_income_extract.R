this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
  wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
  wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}
setwd(wd)
library(ipumsr)

def <- define_extract_usa("all income",samples = c("us1950a","us1960a",
                                         "us1970c","us1970d", "us1980b",
                                         "us1990b","us2000g","us2010g"),
                          variables = c("INCTOT","FTOTINC","INCWAGE","POVERTY",
                                        "REGION","STATEFIP","COUNTYFIP", "RACE",
                                        "AGE","SEX"))
# submitting extract
submit <- submit_extract(def)

# 1950-2020 ftotinc, inctot, incwage, poverty for decennial 1 percent 

# getting status of extract
exinf <- get_extract_info("usa:2")

status <- exinf$status

readyex <- wait_for_extract(exinf)

# when extract is ready, download
dir.create("./ipums",FALSE,FALSE)
if (!file.exists("./ipums/usa_00002.xml")){
  dpath <- download_extract(readyex, download_dir ="./ipums",overwrite = TRUE)
}else {dpath = "./ipums/usa_00002.xml"}
d = read_ipums_micro(dpath)
