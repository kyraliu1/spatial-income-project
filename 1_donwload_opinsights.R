# downloads data from opportunity insights.org; this is from the exploration phase
# script 2_initial_plots explores it a little but, but we have not looked at this 
# for a while


# setup -------------------------------------------------------------------


this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
	wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}

setwd(wd)

library(terra)
#library(tidycensus)

# downloads ---------------------------------------------------------------


# getting aggregated migration data
agmigfi <- "raw/opinsights/aggmigration"

if (!file.exists(agmigfi)){
  dir.create(dirname(agmigfi), FALSE, TRUE)
  download.file("https://opportunityinsights.org/wp-content/uploads/2022/07/od_pooled.csv",
                agmigfi)
}
aggmig <- read.csv(agmigfi)


# getting parental income data
outfi <- "raw/opinsights/parentalincome"
if (!file.exists(outfi)){  
  dir.create(dirname(outfi), FALSE, TRUE)
  download.file("https://opportunityinsights.org/wp-content/uploads/2018/10/cz_outcomes.csv",
                outfi)
}
# acs.var$concept[acs.var$name == vari[2]]
parental <- read.csv(outfi)

# getting commuter zone shapefile
comzoneshape <- "raw/opinsights/commuterzones/cz1990.shp"
if (!file.exists(comzoneshape)){
  dir.create(dirname(comzoneshape))
  url <- "https://opportunityinsights.org/wp-content/uploads/2018/07/cz1990_shapefile.zip"
  f <- file.path(dirname(comzoneshape), basename(url))
  download.file(url, f, mode="wb")
  unzip(f, exdir =dirname(comzoneshape))
}
