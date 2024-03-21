# filling tracts for 1980, using counties
# shannon index, diversity vs income, script 12 does this all again and includes
# the AIC calculations, script 12 is also better organized than this one

# setup
wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
setwd(wd)


library(terra)
library(geodata)
library(hexbin)
library(dplyr)
library(viridis)
#library(ggplot2)

state <- readRDS('./ipums/state_level_d')

# H = -sum(p * ln(p))

#tract <- readRDS('./ipums/tract_by_county')
tract <- readRDS('./ipums/tract_with_div')
#tract <- tract[tract$STATE != 'Alaska',]

tract$medinc80[tract$medinc80 == 0] <- NA
tract$medinc90[tract$medinc90 == 0] <- NA
tract$medinc00[tract$medinc00 == 0] <- NA
tract$medinc10[tract$medinc10 == 0] <- NA
tract$medinc20[tract$medinc20 == 0] <- NA

county <- readRDS('./ipums/county_by_state')
county$countyfull <- paste0(county$COUNTY,', ',county$STATE )
#tract <- tract[tract$STATE != 'Alaska',]

county$medinc80[county$medinc80 == 0] <- NA
county$medinc90[county$medinc90 == 0] <- NA
county$medinc00[county$medinc00 == 0] <- NA
county$medinc10[county$medinc10 == 0] <- NA
county$medinc20[county$medinc20 == 0] <- NA



tractby <- tract %>%
  group_by(countyfull) %>%
  summarise(across(c(B18AA1980, B18AB1980, B18AC1980, B18AD1980), list(tract_total_population = sum),na.rm=T))

county= merge(county,tractby,by = 'countyfull',all=T)
county$without_AA = county$B18AA1980 - county$B18AA1980_tract_total_population
county$without_AB = county$B18AB1980 - county$B18AB1980_tract_total_population
county$without_AC = county$B18AC1980 - county$B18AC1980_tract_total_population
county$without_AD = county$B18AD1980 - county$B18AD1980_tract_total_population

county$without_total = county$without_AA +county$without_AB +county$without_AC+ county$without_AD

county$without_pwhite = county$without_AA/county$without_total
county$without_pblack = county$without_AB/county$without_total 
county$without_pnative = county$without_AC/county$without_total 
county$without_paapi = county$without_AD/county$without_total 

yr <- c('80', '90', '00' , '10','20')
year <- c(1980,1990,2000,2010,2020)


  w <- ifelse(county[['without_pwhite']] == 0, 1, county[['without_pwhite']])
  b <- ifelse(county[['without_pblack']] == 0, 1, county[['without_pblack']])
  a <- ifelse(county[['without_paapi']] == 0, 1, county[['without_paapi']])
  n <- ifelse(county[['without_pnative']] == 0, 1, county[['without_pnative']])
#  o <- ifelse(county[[paste0('pother', yr[i])]] == 0, 1, county[[paste0('pother', yr[i])]])
  
  
 
  hcounty <- -1*(w * log(w)+ b * log(b)+ a * log(a)+ n * log(n))
  county$h80_without <- exp(hcounty)
  


tract <- tract[tract$STATE != 'Alaska',]

tract <- tract[tract$STATE != 'Hawaii',]
#ts <- vect('./ipums/bounds/US_tract_2010.shp')

ts1980 <- vect('./ipums/bounds/US_tract_1980')
ts1990 <- vect('./ipums/bounds/US_tract_1990')
ts2000 <- vect('./ipums/bounds/US_tract_2000')
ts2010 <- vect('./ipums/bounds/US_tract_2010')
ts2020 <- vect('./ipums/bounds/US_tract_2020')

#names(ts)[13] <- "GJOIN2010"

names(ts1980)[names(ts1980) == "GISJOIN"] <- "GJOIN1980"
names(ts1990)[names(ts1990) == "GISJOIN"] <- "GJOIN1990"
names(ts2000)[names(ts2000) == "GISJOIN"] <- "GJOIN2000"
names(ts2010)[names(ts2010) == "GISJOIN"] <- "GJOIN2010"
names(ts2020)[names(ts2020) == "GISJOIN"] <- "GJOIN2020"

pal = viridis(5)
pal = colorRampPalette(pal)(100)
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  #dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

tract80 <- merge(ts1980,tract,by = "GJOIN1980")
tract90 <- merge(ts1990,tract,by = 'GJOIN1990')
tract00 <- merge(ts2000,tract,by = 'GJOIN2000')
tract10 <- merge(ts2010,tract,by = 'GJOIN2010')
tract20 <- merge(ts2020,tract,by = 'GJOIN2020')

#plot(tract80,'h80',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 1980'),range = c(1,4),col = pal,mar=c(0,0,0,0))
#tr <- rast(tract)
par(mfrow = c(2,3),mar = c(0,0,0,0))
#panel(tr[c('h80','h90','h00','h10','h20')],main = 'Diversity by census tract',nc = 3,nr = 2)
#stop('here')
# county plots
usa <- gadm('usa',".",level = 2)
usa$fullname[usa$NAME_1 != 'Louisiana'] <- paste0(usa$NAME_2[usa$NAME_1 != 'Louisiana'], ' County, ', usa$NAME_1[usa$NAME_1 != 'Louisiana'])
usa$fullname[usa$NAME_1 == 'Louisiana'] <- paste0(usa$NAME_2[usa$NAME_1 == 'Louisiana'], ' Parish, ', usa$NAME_1[usa$NAME_1 == 'Louisiana'])

county$fullname <- paste0(county$COUNTY,', ',county$STATE)

county <- merge(usa,county,by = 'fullname')
county <- county[county$NAME_1 != "Hawaii"]
county <- county[county$NAME_1 != "Alaska"]
county <- project(county,crs(tract80))

lmat = matrix(1:6,nrow = 2,byrow=T)
layout(lmat, widths = c(1, 1, 1), heights = c(1, 1))

#plot(tract,'h80',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 1980'),range = c(1,4),col = pal,mar=c(0,0,0,0))
plot(county,'h80_without',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 1980'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
plot(tract80,'h80',add = T,type = 'continuous',border = NA,range = c(1,4),col = pal,axes = F, box = F,legend = F)

plot(tract90,'h90',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 1990'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)

plot(tract00,'h00',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 2000'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)

plot(tract10,'h10',type = 'continuous',border = NA, main = expression( e^italic(H)* 'census tract 2010'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
plot(tract20,'h20',type = 'continuous',border = NA, main = expression( e^italic(H)* 'census tract 2020'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = T)
#color.bar(pal,min = 1,max=4)
#tract$density80 <- tract$pop80/tract$area 

