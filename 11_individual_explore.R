# individual level analysis

# setup -------------------------------------------------------------------


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
library(geodata)
#dpath = paste0("./ipums/usa_0000",5,".xml")

# read data ---------------------------------------------------------------


# read data
#d = read_ipums_micro(dpath)
#saveRDS(d,"./ipums/ipums_micro")
d <- readRDS("./ipums/ipums_micro")

# cleaning ----------------------------------------------------------------


d$statename = as.character((as_factor(d$STATEFIP)))
d$cofip <- as.character((as_factor(d$COUNTYFIP)))
d$fip <- paste0(d$STATEFIP,d$cofip)

# no income data
nodata = d[,c("INCTOT","INCWAGE","FTOTINC")] == 9999999 
d[,c("INCTOT","INCWAGE","FTOTINC")][nodata]=-999999

d <- d[!nodata[,1],]

rm(nodata) # no memory
yr <- table(d$YEAR) # data per year where "total personal income" is available

# reclassifying race
d$r_race <- NA
d$r_race[d$RACE==1] <- "white"
d$r_race[d$RACE==2] <- "black"
d$r_race[d$RACE==3] <- "native"
d$r_race[d$RACE %in% c(4,5,6)] <- "aapi"
d$r_race[d$RACE %in% c(7,8,9)] <- "other"


# median income by race ---------------------------------------------------


year = c(1980,1990,2000,2010)
d$medinc <-  NA

par(mfrow = c(2,2),las=2,mar = c(3.3,3.25,3,1))
for (i in 1:4){
d[d$YEAR==year[i],]$medinc = d[d$YEAR==year[i],]$INCTOT/median( d[d$YEAR==year[i],]$INCTOT)
boxplot(medinc~r_race,data = d[d$YEAR == year[i],], names = c("white","black","native","aapi","other"),
        outline = F,ylim = c(-2,6),col=NA,ylab= NA)
 text(3,5.5,year[i])
}
title(main = 'Distribution of Median Income Ratio by Race',outer = T,line = -1.5)

dwb <- d[d$r_race %in% c("white", "black"), ]

#  median income per state and race
mis <- aggregate(INCTOT ~ statename + r_race + YEAR , data = dwb, FUN = median)

mis<- reshape(mis, idvar = c("statename","YEAR"), timevar = "r_race", direction = "wide")
names(mis)<- c('state','year','black','white')
yy <- c('80','90','00','10')
mis$median_diff <- NA

# read in aggregated state level data
sl <- readRDS('~/Documents/research/spatial-income-project/ipums/state_level_d')

sl$B79AA2010 <- sl$B79AA135
for (i in 1:4){
  my <- mis[mis$year == year[i],]
 mis$median_diff[mis$year==year[i]] <-(my$white - my$black)/(median(sl[[paste0(
   'B79AA',year[i])]],na.rm = T))
}
# compute for counties, color code for regions maybs

usa <- gadm("usa",1,'.')


names(usa)[4] <- 'state'
usa = usa[usa$state != 'Alaska']
usa = usa[usa$state != 'Hawaii'] 

usa <- merge(usa,mis,'state')
par(mfrow = c(2,2))
rus <- rast(ext(usa),ncols = 1000,nrows = 1000,nlyrs = 4)

m <- c(0, 1.1, 0, 1.1)
for (i in 1:4){
  rus[[i]]<- rasterize(usa[usa$year == year[i]],rus,field = 'median_diff',fun = 'min')
# plot(usa[usa$year == year[i]],'median_diff',type = 'continuous',
#      main = year[i],border = NA,col= colorRampPalette(c('grey','black'))(100),
#      range = c(0,0.6),mar=m,legend=F,box = T,axes =F)
}

panel(rus,col = colorRampPalette(c('white','black'))(100),axes = F,main = year,loc.main = 'topright')
title(main = 'Difference between median white and\n black personal income per state',outer = T,line = -3)

