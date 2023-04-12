wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
setwd(wd)
library(terra)
library(tidycensus)

# getting aggregated migration data
agmigfi <- "./opinsights/aggmigration"

if (!file.exists(agmigfi)){
  dir.create("./opinsights")
  download.file("https://opportunityinsights.org/wp-content/uploads/2022/07/od_pooled.csv",
                agmigfi)
}
aggmig <- read.csv(agmigfi)

# getting parental income data
outfi <- "./opinsights/parentalincome"
if (!file.exists(outfi)){
  
  download.file("https://opportunityinsights.org/wp-content/uploads/2018/10/cz_outcomes.csv",
                outfi)
}
# acs.var$concept[acs.var$name == vari[2]]
parental <- read.csv(outfi)

# getting commuter zone shapefile
comzoneshape <- "./opinsights/commuterzones/cz1990"
if (!file.exists(comzoneshape)){
  dir.create("./opinsights/commuterzones")
  download.file(  "https://opportunityinsights.org/wp-content/uploads/2018/07/cz1990_shapefile.zip",
                  comzoneshape)
}
unzip(comzoneshape,exdir ="./opinsights/commuterzones" )
comzones <- vect("./opinsights/commuterzones/cz1990.shp")
comzones = crop(comzones, c(-135,-50,20,50))

# merging commuter zones to parental income 
czonesparental <- merge(comzones,parental,by="cz")
# plot(comzones)

# Mean percentile rank (relative to other children born in the same year) in the
# national distribution of household income (i.e. own earnings and spouse’s 
# earnings) measured as mean earnings in 2014-2015 for the baseline sample
# kfr and cz

# kfr <- czonesparental[,grepl("kfr",names(czonesparental))]
# kfr <- cbind(kfr,czonesparental['cz'])
# kfr[,1]
desired <- c("mean","kfr")
kfindex <- sapply(x = names(parental),FUN = grepl, desired)

kfr <- parental[,kfindex[,1]&kfindex[,2]]
kfr <- cbind(kfr,parental['cz'])
hist(kfr[,1])
