# shannon index, diversity vs income, script 12 does this all again and includes
# the AIC calculations, script 12 is also better organized than this one

# setup
wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
setwd(wd)

library(terra)
library(geodata)
library(hexbin)
library(ggplot2)

state <- readRDS('./ipums/state_level_d')

# H = -sum(p * ln(p))

tract <- readRDS('./ipums/tract_by_county')
#tract <- tract[tract$STATE != 'Alaska',]

tract$medinc80[tract$medinc80 == 0] <- NA
tract$medinc90[tract$medinc90 == 0] <- NA
tract$medinc00[tract$medinc00 == 0] <- NA
tract$medinc10[tract$medinc10 == 0] <- NA
county <- readRDS('./ipums/county_by_state')
#tract <- tract[tract$STATE != 'Alaska',]

county$medinc80[county$medinc80 == 0] <- NA
county$medinc90[county$medinc90 == 0] <- NA
county$medinc00[county$medinc00 == 0] <- NA
county$medinc10[county$medinc10 == 0] <- NA

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
modtract = list()
hextract= list()

newx <- seq(1,4,length.out = 100)
nx <- data.frame(h80 = newx, h90 = newx, h00 = newx, h10 = newx)

par(mfrow = c(2,2),mar = c(4,4,3,1),las = 1)
for(i in 1:4){
  m <- paste0('medinc',yr[i])
  h <- paste0('h',yr[i])
  modtract[[i]] <- lm(paste0(m,'~',h),data = tract)
  #tract[[paste0('htfit',yr[i])]] <- modtract[[i]]$fitted.values
  hextract[[i]] <- hexbin(exp(htract[[i]]),tract[[paste0('medinc',yr[i])]])
  
plot(exp(htract[[i]]),tract[[paste0('medinc',yr[i])]]#, main = year[i]
     ,xlab = expression(e^italic(H)),ylab = 'median income ratio',cex = 0.25,
     ylim = c(0,6),xlim = c(1,4.5),
     col = rgb(0,0,0,0.8))

text(3.7,5.5,year[i],pos = 4,cex = 1.2)
fit <- predict(modtract[[i]],nx)
lines(nx[,i],fit,col = 'red',lwd = 2)
  
}
title(expression("Tract:County Median Income vs. " * e^italic(H)),outer = T,line = -1.5)

for (i in 1:4){
  plot(hextract[[i]] ,xlab = expression(e^italic(H)),ylab = 'median income ratio',
       main = expression("Tract to County Median Income vs. " * e^italic(H)))
}
hcounty <- list()

for (i in 1:4){
  
  w <- county[[paste0('pwhite',yr[i])]]
  b <-county[[paste0('pblack',yr[i])]]
  a <- county[[paste0('paapi',yr[i])]]
  n <- county[[paste0('pnative',yr[i])]]
  o <- county[[paste0('pother',yr[i])]]
  
  hcounty[[i]] <- -1*(w * log(w)+ b * log(b)+ a * log(a)+ n * log(n)+o * log(o))
  county[[paste0('h',yr[i])]] <- exp(hcounty[[i]])
  
}

modcounty = list()


for(i in 1:4){
  
  # variable names for year
  m <- paste0('medinc',yr[i])
  h <- paste0('h',yr[i])
  wi <- county[[paste0('pop',yr[i])]]
  
  # fit model
  modcounty[[i]] <- lm(paste0(m,'~',h,'+ I(',h,'^2)'),data = county,weight = pop10)
 #county[[paste0('hcfit',yr[i])]] <- modcounty[[i]]$fitted.values
  
  # plot
  plot(exp(hcounty[[i]]),county[[paste0('medinc',yr[i])]]#, main = year[i]
       ,xlab = expression(e^italic(H)),ylab = 'median income ratio',cex = 0.25,
       ylim = c(0.4,2.5),xlim = c(1,4),
       col = rgb(0,0,0,0.8))
  text(3,2.2,year[i],pos = 4,cex = 1.5)
  
  fit <- predict(modcounty[[i]],nx)
  lines(nx[,i],fit,col = 'red')
  
}
title(expression("County:State Median Income vs. " * e^italic(H)),outer = T,line = -1.5)



hstate <- list()

for (i in 1:4){
  
  w <- state[[paste0('pwhite',yr[i])]]
  b <-state[[paste0('pblack',yr[i])]]
  a <- state[[paste0('paapi',yr[i])]]
  n <- state[[paste0('pnative',yr[i])]]
  o <- state[[paste0('pother',yr[i])]]
  
  hstate[[i]] <- -1*(w * log(w)+ b * log(b)+ a * log(a)+ n * log(n)+o * log(o))
  state[[paste0('h',yr[i])]] <- exp(hstate[[i]])
  
}

# state 
modstate = list()
par(mfrow = c(2,2),mar = c(4,4,3,1),las = 1)


for(i in 1:4){
  
  # variable names for year
  m <- paste0('medinc',yr[i])
  h <- paste0('h',yr[i])
  wi <- state[[paste0('pop',yr[i])]]
  
  # fit model
  modstate[[i]] <- lm(paste0(m,'~',h,'+ I(',h,'^2)'),data = state,weight = pop10)
  #state[[paste0('hcfit',yr[i])]] <- modstate[[i]]$fitted.values
  
  # plot
  plot(exp(hstate[[i]]),state[[paste0('medinc',yr[i])]]#, main = year[i]
       ,xlab = expression(e^italic(H)),ylab = 'median income ratio',cex = 0.65,
       ylim = c(0.4,2.5),xlim = c(1,4),pch = 20,
       col = rgb(0,0,0,0.8))
  text(3,2.2,year[i],pos = 4,cex = 1.5)
  
  fit <- predict(modstate[[i]],nx)
  lines(nx[,i],fit,col = 'red')
  
}
title(main = expression("State:National Median Income vs. " * e^italic(H)),outer = T,line = -1.5)


tract <- tract[tract$STATE != 'Alaska',]

tract <- tract[tract$STATE != 'Hawaii',]
 ts <- vect('./ipums/bounds/US_tract_2010.shp')
 names(ts)[13] <- "GJOIN2010"


tract <- merge(ts,tract)
par(mfrow = c(1,1))

plot(tract,'h80',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 1980'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))

plot(tract,'h90',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 1990'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))

plot(tract,'h00',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 2000'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))

plot(tract,'h10',type = 'continuous',border = NA, main = expression( e^italic(H)* 'census tract 2010'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))

#tract$density80 <- tract$pop80/tract$area 
# county plots
usa <- gadm('usa',".",level = 2)
usa$fullname[usa$NAME_1 != 'Louisiana'] <- paste0(usa$NAME_2[usa$NAME_1 != 'Louisiana'], ' County, ', usa$NAME_1[usa$NAME_1 != 'Louisiana'])
usa$fullname[usa$NAME_1 == 'Louisiana'] <- paste0(usa$NAME_2[usa$NAME_1 == 'Louisiana'], ' Parish, ', usa$NAME_1[usa$NAME_1 == 'Louisiana'])

county$fullname <- paste0(county$COUNTY,', ',county$STATE)

county <- merge(usa,county,by = 'fullname')
county <- county[county$NAME_1 != "Hawaii"]
par(mfrow = c(1,1))
plot(county,'h80',type = 'continuous',border = NA, main = expression( e^italic(H) * ' county 1980'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))
# 
plot(county,'h90',type = 'continuous',border = NA, main = expression( e^italic(H) * ' county 1990'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))
# 
plot(county,'h00',type = 'continuous',border = NA, main = expression( e^italic(H) * ' county 2000'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))
# 
plot(county,'h10',type = 'continuous',border = NA, main = expression( e^italic(H)* ' county 2010'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))

states <- gadm("usa",".",level = 1)
names(states)[4] <- "STATE"
states <- merge(states,state,by = "STATE")
states <- states[states$STATE != "Alaska"]
states <- states[states$STATE != "Hawaii"]

par(mfrow = c(1,1))
plot(states,'h80',type = 'continuous',border = NA, main = expression( e^italic(H) * ' state 1980'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))
# 
plot(states,'h90',type = 'continuous',border = NA, main = expression( e^italic(H) * ' state 1990'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))
# 
plot(states,'h00',type = 'continuous',border = NA, main = expression( e^italic(H) * ' state 2000'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))
# 
plot(states,'h10',type = 'continuous',border = NA, main = expression( e^italic(H)* ' state 2010'),range = c(1,4),col = colorRampPalette(c('lightgrey','black'))(100))
