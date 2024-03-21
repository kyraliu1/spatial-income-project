# shannon index, diversity vs income, script 12 does this all again and includes
# the AIC calculations, script 12 is also better organized than this one

# setup
wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
setwd(wd)


library(terra)
library(geodata)
library(hexbin)
library(viridis)
library(RColorBrewer)

source("./scripts/color_bar_func.R")
state <- readRDS('./ipums/state_level_d')

# H = -sum(p * ln(p))

tract <- readRDS('./ipums/tract_by_county')
#tract <- tract[tract$STATE != 'Alaska',]

tract$medinc80[tract$medinc80 == 0] <- NA
tract$medinc90[tract$medinc90 == 0] <- NA
tract$medinc00[tract$medinc00 == 0] <- NA
tract$medinc10[tract$medinc10 == 0] <- NA
tract$medinc20[tract$medinc20 == 0] <- NA

county <- readRDS('./ipums/county_by_state')
#tract <- tract[tract$STATE != 'Alaska',]

county$medinc80[county$medinc80 == 0] <- NA
county$medinc90[county$medinc90 == 0] <- NA
county$medinc00[county$medinc00 == 0] <- NA
county$medinc10[county$medinc10 == 0] <- NA
county$medinc20[county$medinc20 == 0] <- NA
yr <- c('80', '90', '00' , '10','20')
year <- c(1980,1990,2000,2010,2020)
htract <- list()

for (i in 1:length(year)){
w <- ifelse(tract[[paste0('pwhite', yr[i])]] == 0, 1, tract[[paste0('pwhite', yr[i])]])
b <- ifelse(tract[[paste0('pblack', yr[i])]] == 0, 1, tract[[paste0('pblack', yr[i])]])
a <- ifelse(tract[[paste0('paapi', yr[i])]] == 0, 1, tract[[paste0('paapi', yr[i])]])
n <- ifelse(tract[[paste0('pnative', yr[i])]] == 0, 1, tract[[paste0('pnative', yr[i])]])
o <- ifelse(tract[[paste0('pother', yr[i])]] == 0, 1, tract[[paste0('pother', yr[i])]])


  if (year[i] < 2000){
    o = 1
  }
  htract[[i]] <- -1*(w * log(w)+ b * log(b)+ a * log(a)+ n * log(n)+o * log(o))
  tract[[paste0('h',yr[i])]] <- exp(htract[[i]])
  
}
modtract = list()
hextract= list()

newx <- seq(1,length(year),length.out = 100)
nx <- data.frame(h80 = newx, h90 = newx, h00 = newx, h10 = newx,h20 = newx)

par(mfrow = c(2,3),mar = c(4,4,3,1),las = 1)
for(i in 1:length(year)){
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
# 
# for (i in 1:length(year)){
#   plot(hextract[[i]] ,xlab = expression(e^italic(H)),ylab = 'median income ratio',
#        main = expression("Tract to County Median Income vs. " * e^italic(H)))
# }
hcounty <- list()

for (i in 1:length(year)){
  
  w <- ifelse(county[[paste0('pwhite', yr[i])]] == 0, 1, county[[paste0('pwhite', yr[i])]])
  b <- ifelse(county[[paste0('pblack', yr[i])]] == 0, 1, county[[paste0('pblack', yr[i])]])
  a <- ifelse(county[[paste0('paapi', yr[i])]] == 0, 1, county[[paste0('paapi', yr[i])]])
  n <- ifelse(county[[paste0('pnative', yr[i])]] == 0, 1, county[[paste0('pnative', yr[i])]])
  o <- ifelse(county[[paste0('pother', yr[i])]] == 0, 1, county[[paste0('pother', yr[i])]])
  
  if (year[i] < 2000){
    o = 1
  }
  hcounty[[i]] <- -1*(w * log(w)+ b * log(b)+ a * log(a)+ n * log(n)+o * log(o))
  county[[paste0('h',yr[i])]] <- exp(hcounty[[i]])
  
}

modcounty = list()
par(mfrow = c(2,3),mar = c(4,4,3,1),las = 1)


for(i in 1:length(year)){
  
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

for (i in 1:length(year)){
  
 w <- ifelse(state[[paste0('pwhite', yr[i])]] == 0, 1, state[[paste0('pwhite', yr[i])]])
b <- ifelse(state[[paste0('pblack', yr[i])]] == 0, 1, state[[paste0('pblack', yr[i])]])
a <- ifelse(state[[paste0('paapi', yr[i])]] == 0, 1, state[[paste0('paapi', yr[i])]])
n <- ifelse(state[[paste0('pnative', yr[i])]] == 0, 1, state[[paste0('pnative', yr[i])]])
o <- ifelse(state[[paste0('pother', yr[i])]] == 0, 1, state[[paste0('pother', yr[i])]])

  if (year[i] < 2000){
    o = 1
  }
  hstate[[i]] <- -1*(w * log(w)+ b * log(b)+ a * log(a)+ n * log(n)+o * log(o))
  state[[paste0('h',yr[i])]] <- exp(hstate[[i]])
  
}


# state 
modstate = list()
par(mfrow = c(2,3),mar = c(4,4,3,1),las = 1)


for(i in 1:length(year)){
  
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
#saveRDS(tract,'./ipums/tract_with_div')


tract <- tract[tract$STATE != 'Alaska',]

tract <- tract[tract$STATE != 'Hawaii',]
 ts <- vect('./ipums/bounds/US_tract_2010')
 
 ts1980 <- vect('./ipums/bounds/US_tract_1980')
 ts1990 <- vect('./ipums/bounds/US_tract_1990')
 ts2000 <- vect('./ipums/bounds/US_tract_2000')
 ts2010 <- vect('./ipums/bounds/US_tract_2010')
 ts2020 <- vect('./ipums/bounds/US_tract_2020')
 
 names(ts)[13] <- "GJOIN2010"

 names(ts1980)[3] <- "GJOIN1980"
 names(ts1990)[3] <- "GJOIN1990"
 names(ts2000)[names(ts2000) == "GISJOIN"] <- "GJOIN2000"
 names(ts2010)[names(ts2010) == "GISJOIN"] <- "GJOIN2010"
 names(ts2020)[names(ts2020) == "GISJOIN"] <- "GJOIN2020"
 
pal = plasma(6)
 pal = colorRampPalette(pal)(100)

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

layout(matrix(c(1:6),ncol=3, nrow=2, byrow=TRUE))

#plot(tract,'h80',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 1980'),range = c(1,4),col = pal,mar=c(0,0,0,0))
#plot(tract80,'h80',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 1980'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)

#plot(tract90,'h90',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 1990'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)

#plot(tract00,'h00',type = 'continuous',border = NA, main = expression( e^italic(H) * 'census tract 2000'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)

#plot(tract10,'h10',type = 'continuous',border = NA, main = expression( e^italic(H)* 'census tract 2010'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
#plot(tract20,'h20',type = 'continuous',border = NA, main = expression( e^italic(H)* 'census tract 2020'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = T)
#color.bar(pal,min = 1,max=4)
#tract$density80 <- tract$pop80/tract$area 
# county plots
usa <- gadm('usa',".",level = 2)
usa <- project(usa,crs(tract80))
usa$fullname[usa$NAME_1 != 'Louisiana'] <- paste0(usa$NAME_2[usa$NAME_1 != 'Louisiana'], ' County, ', usa$NAME_1[usa$NAME_1 != 'Louisiana'])
usa$fullname[usa$NAME_1 == 'Louisiana'] <- paste0(usa$NAME_2[usa$NAME_1 == 'Louisiana'], ' Parish, ', usa$NAME_1[usa$NAME_1 == 'Louisiana'])

county$fullname <- paste0(county$COUNTY,', ',county$STATE)

county <- merge(usa,county,by = 'fullname')
county <- county[county$NAME_1 != "Hawaii"]
par(mar = c(0,0,0,0))
layout(matrix(c(1:6),ncol=3, nrow=2, byrow=TRUE))

plot(county,'h80',type = 'continuous',border = NA, main = expression( e^italic(H) * ' county 1980'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
# 
plot(county,'h90',type = 'continuous',border = NA, main = expression( e^italic(H) * ' county 1990'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
# 
plot(county,'h00',type = 'continuous',border = NA, main = expression( e^italic(H) * ' county 2000'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
# 
plot(county,'h10',type = 'continuous',border = NA, main = expression( e^italic(H)* ' county 2010'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
plot(county,'h20',type = 'continuous',border = NA, main = expression( e^italic(H)* ' county 2020'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = T)

states <- gadm("usa",".",level = 1)
states <- project(states,crs(tract80))


names(states)[4] <- "STATE"
states <- merge(states,state,by = "STATE")
states <- states[states$STATE != "Alaska"]
states <- states[states$STATE != "Hawaii"]

par(mar = c(0,0,0,0))
layout(matrix(c(1:6),ncol=3, nrow=2, byrow=TRUE))
plot(states,'h80',type = 'continuous',border = NA, main = expression( e^italic(H) * ' state 1980'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
# 
plot(states,'h90',type = 'continuous',border = NA, main = expression( e^italic(H) * ' state 1990'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
# 
plot(states,'h00',type = 'continuous',border = NA, main = expression( e^italic(H) * ' state 2000'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)
# 
plot(states,'h10',type = 'continuous',border = NA, main = expression( e^italic(H)* ' state 2010'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = F)

plot(states,'h20',type = 'continuous',border = NA, main = expression( e^italic(H)* ' state 2020'),range = c(1,4),col = pal,mar=c(1,1,1,1),axes = F, box = F,legend = T)
