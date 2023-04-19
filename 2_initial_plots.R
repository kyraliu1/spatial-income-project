this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
  wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/spatial_income" 
} else if (this == "poposaurus"){
  wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
}

setwd(wd)

library(terra)

source("./1_donwload_opinsights.R")
# comzoneshape <- "raw/opinsights/commuterzones/cz1990.shp"
# comzones <- vect(comzoneshape)
# outfi <- "raw/opinsights/parentalincome"
# parental <- read.csv(outfi)


comzones <- vect(comzoneshape)
comzones <- crop(comzones, c(-135,-50,20,50))
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
par(mfrow = c(2,4),mar = c(4,4,3,2))
for (i in 1:ncol(kfrpooledmean)){
  hist(kfrpooledmean[,i], xlim=c(0.1, .8),
       main = names(kfrpooledmean)[i],
       cex.main = 0.7, xlab = 'percentile',cex.axis = 0.7,
       cex.lab = 0.6)
  
}

title(main ="mean percentile rank in the national distribution of household income",
      outer = TRUE,line = -1)
dev.off()
k <- kfr[,c("cz", pooledmean)]
names(k)<- c("cz", "native","asian","black","white",
             "hispanic","other-race","immigrant")
ck <- merge(comzones, k)

terra::plot(ck, 2:8,breaks = seq(0.15,0.8,by = 0.1), main = names(ck)[2:8],
            border = NA, legend = FALSE)

# plot(NULL, main = "mean percentile rank in the national distribution of household income", 
#      xlab = "Value", ylab = "Density",
#      type = "n",
#      xlim = range(k[6]),ylim=c(0,12))
# 
# for(i in 2:length(k)) {
#   knum <- as.numeric(unlist((na.omit(k[i]))))
#   lines(density(knum))
# }
