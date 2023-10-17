# extracting ipums nhgis data for income and race 1970/80 to 2010 
# defines and downloads extract to ./ipums (creates if does not already exist)
# data at state, county, city/sub, place level

# setup 
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

# desired tables & geography
tab <- c('B08','B79','AB2', 'BD5','AV0')
# B08: persons by detailed race 1970 - 2010
# B79: median household income in previous year 1980-2021
# AB2: median family income in previous year 1980-2021
# BD5: per capita income in previous year 1980-2021
# AV0: total population
ge <- c('state','county','tract','cty_sub','place')

# submit & download extracts

if (!file.exists('ipums/nhgis0001_csv.zip')){
# submitting extracts
smt <- list()
counter = 1
for (t in tab){

tst_def <- tst_spec(name = t , geog_levels = ge)

ex_def <- define_extract_nhgis(description = t,
                               time_series_tables = tst_def)

smt[[counter]] <- submit_extract(ex_def)
counter = counter+1
}

dir.create('./ipums',FALSE)

for (i in 1:length(smt)){
if (get_extract_info(smt[[i]])$status=='completed'){


  dpath <- download_extract(smt[[i]], download_dir ="./ipums",overwrite = TRUE)
}
}
}



# read data



# n <- NA
# # checking if this particular extract has been submitted before
# for (i in 1:length(prev)){
#   if(identical(prev[[c(i,6)]], samps) & identical(prev[[c(i,7)]],
#                                                   vars)){
#     n <- as.character(prev[[c(i,10)]]) # extract number
#     break
#   }
#   
# }


zips <- list.files('./ipums',pattern = '.zip$',full.names = T)
#unzip(zips,exdir = './ipums')
unz <- sapply(zips,  FUN = 'unzip',exdir = './ipums')


