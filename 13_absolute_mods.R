
# setup -------------------------------------------------------------------
wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
setwd(wd)


library(terra)
library(geodata)
library(hexbin)
library(ggplot2)
library(gridExtra)
library(MASS)

# read data ---------------------------------------------------------------
# nhgis 1: B08: persons by detailed race 1970 - 2010
# nhgis 2: B79: median household income in previous year 1980-2021
# nhgis 3: AB2: median family income in previous year 1980-2021
# nhgis 4: BD5: per capita income in previous year 1980-2021

# nhgis 6: AV0: total population

state <- readRDS('./ipums/state_level_d')

# H = -sum(p * ln(p))

tract <- readRDS('./ipums/tract_by_county')
county <- readRDS('./ipums/county_by_state')


# clean data --------------------------------------------------------------

names(tract)[names(tract) == 'B79AA135'] <- 'B79AA2010'
tract$B79AA1980[tract$B79AA1980 == 0] <- NA
tract$B79AA1990[tract$B79AA1990 == 0] <- NA
tract$B79AA2000[tract$B79AA2000 == 0] <- NA
tract$B79AA2010[tract$B79AA2010 == 0] <- NA
county <- readRDS('./ipums/county_by_state')
#tract <- tract[tract$STATE != 'Alaska',]

county$B79AA1980[county$B79AA1980 == 0] <- NA
county$B79AA1990[county$B79AA1990 == 0] <- NA
county$B79AA2000[county$B79AA2000 == 0] <- NA
#county$B79AA2010[county$B79AA2010 == 0] <- NA



#tract <- tract[tract$STATE != 'Alaska',]

# find H and e^H tracts ----------------------------------------------------------


yr <- c('80', '90', '00' , '10')
year <- c(1980,1990,2000,2010)
htract <- list()

for (i in 1:4){
  
  w <- tract[[paste0('pwhite',yr[i])]]
  b <-tract[[paste0('pblack',yr[i])]]
  a <- tract[[paste0('paapi',yr[i])]]
  n <- tract[[paste0('pnative',yr[i])]]
  o <- tract[[paste0('pother',yr[i])]]
  
  htract[[i]] <- -1*(w * log(w)+ b * log(b)+ a * log(a)+ n * log(n)+o * log(o))
  tract[[paste0('h',yr[i])]] <- exp(htract[[i]])
  
}

# model e^H tracts---------------------------------------------------------------


modtract = list()
hextract= list()
steptract = list()

newx <- seq(1,4,length.out = 100)
nx <- data.frame(h80 = newx, h90 = newx, h00 = newx, h10 = newx)
#modeling tracts
par(mfrow = c(2,2),mar = c(4,4,3,1),las = 1)
#for(i in 1:4){

# model selection tracts ---------------------------------------------------------


modsel <- function(dlevel){
  results <- list()  
  for (i in 1:4){
    m <- paste0('B79AA',year[i])
    h <- paste0('h',yr[i])
    quadmod <- lm(paste0(m,'~',h,'+ I(',h,'^2)'),data = dlevel)
    linmod <- lm(paste0(m,'~',h),data = dlevel)
    
    
    stepped <- stepAIC(quadmod,direction = 'forward',
                       scope = list(upper = quadmod,lower = ~1))
    results[[i]]<-summary(stepped)
  }
  return(results)
  #tract[[paste0('htfit',yr[i])]] <- modtract[[i]]$fitted.values
  #hextract[[i]] <- hexbin(exp(htract[[i]]),tract[[paste0('medinc',yr[i])]])
}
tractsel <- modsel(tract)
# plotting tracts ---------------------------------------------------------

for(i in 1:4){
  
  m <- paste0('B79AA',year[i])
  h <- paste0('h',yr[i])
  modtract[[i]] <- lm(paste0(m,'~',h,'+ I(',h,'^2)'),data = tract)
  plot(exp(htract[[i]]),tract[[paste0('B79AA',year[i])]]#, main = year[i]
       ,xlab = expression(e^italic(H)),ylab = 'median income ratio',cex = 0.25,
       xlim = c(1,5),
       col = rgb(0,0,0,0.8))
  
  text(3.7,5.5,year[i],pos = 4,cex = 1.2)
  fit <- predict(modtract[[i]],nx)
  lines(nx[,i],fit,col = 'red',lwd = 2)
  
}
title(expression("Tract Median Income vs. " * e^italic(H)),outer = T,line = -1.5)
