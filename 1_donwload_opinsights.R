
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else {
	wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}

setwd(wd)

library(terra)
#library(tidycensus)

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

comzones <- vect(comzoneshape)
comzones = crop(comzones, c(-135,-50,20,50))
comzones <- rotate(comzones, longitude=0, split=T, left=TRUE, normalize=FALSE)

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

# kfi <- intersect(grep("mean", names(parental)), grep("kfr", names(parental)))
# kfi <- c(kfi, names(parental) == "cz")
# kfr <- parental[kfi, ]

# hist(kfr[,1], 
#      main= "distribution of mean percentile rank
#      in the national distribution of household income
#      for native american females in commuter zones",
#      xlab = "percentile")
pooledmean <- c("kfr_natam_pooled_mean",
                "kfr_asian_pooled_mean",
                "kfr_black_pooled_mean",
                "kfr_white_pooled_mean",
                "kfr_hisp_pooled_mean",
                "kfr_other_pooled_mean",
                "kfr_imm_pooled_pooled_mean")
kfrpooledmean <- kfr[,pooledmean]
names(kfrpooledmean)<- c("native","asian","black","white",
                         "hispanic","other-race","immigrant")
par(mfrow = c(2,4))
for (i in 1:ncol(kfrpooledmean)){
  hist(kfrpooledmean[,i], xlim=c(0.1, .8),
       main = paste0( "mean percentile rank in the\n",
     "national distribution of household\n", 
     "income for ", names(kfrpooledmean)[i], " americans"),
     cex.main = 0.7, xlab = 'percentile')
}


k <- kfr[,c("cz", pooledmean)]
names(k)<- c("cz", "native","asian","black","white",
                         "hispanic","other-race","immigrant")
ck <- merge(comzones, k)


