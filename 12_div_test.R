# shannon index, diversity vs income

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


state <- readRDS('./ipums/state_level_d')

# H = -sum(p * ln(p))

tract <- readRDS('./ipums/tract_by_county')
#tract <- tract[tract$STATE != 'Alaska',]
county <- readRDS('./ipums/county_by_state')

# clean data --------------------------------------------------------------


tract$medinc80[tract$medinc80 == 0] <- NA
tract$medinc90[tract$medinc90 == 0] <- NA
tract$medinc00[tract$medinc00 == 0] <- NA
tract$medinc10[tract$medinc10 == 0] <- NA
#tract <- tract[tract$STATE != 'Alaska',]

county$medinc80[county$medinc80 == 0] <- NA
county$medinc90[county$medinc90 == 0] <- NA
county$medinc00[county$medinc00 == 0] <- NA
county$medinc10[county$medinc10 == 0] <- NA


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
  m <- paste0('medinc',yr[i])
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

#}

# plotting tracts ---------------------------------------------------------

for(i in 1:4){
  
    m <- paste0('medinc',yr[i])
    h <- paste0('h',yr[i])
    modtract[[i]] <- lm(paste0(m,'~',h,'+ I(',h,'^2)'),data = tract)
  plot(exp(htract[[i]]),tract[[paste0('medinc',yr[i])]]#, main = year[i]
       ,xlab = expression(e^italic(H)),ylab = 'median income ratio',cex = 0.25,
       ylim = c(0,6),xlim = c(1,4.5),
       col = rgb(0,0,0,0.8))
  
  text(3.7,5.5,year[i],pos = 4,cex = 1.2)
  fit <- predict(modtract[[i]],nx)
  lines(nx[,i],fit,col = 'red',lwd = 2)
  
}
title(expression("Tract:County Median Income vs. " * e^italic(H)),outer = T,line = -1.5)

# hexbin plots ------------------------------------------------------------


# using hexbin package
# for (i in 1:4){
#   plot(hextract[[i]] ,xlab = expression(e^italic(H)),ylab = 'median income ratio',
#        main = expression("Tract to County Median Income vs. " * e^italic(H)))
# }


# ggplot hexbins
#plot_list <- list()
# 
# for (i in 1:4) {
#   plots <- ggplot(tract, aes(x = !!sym(paste0('h', yr[i])), y = !!sym(paste0('medinc', yr[i])))) +
#                    geom_hex(fill = "black") +  # Set fill color to black
#                    labs(
#                      x = expression(e^italic(H)),
#                      y = 'median income ratio',
#                      title = year[i]
#                    ) +
#                    scale_fill_manual(values = c("black", "gray")) +  # Use black and gray fill colors
#                    theme_minimal()
#                  
#                  plot_list[[i]] <- plot
# }
# 
# grid.arrange(grobs = plot_list, ncol = 2)


# calculate H counties ----------------------------------------------------


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

# model county ------------------------------------------------------------


modcounty = list()

countysel <- modsel(county)


for(i in 1:4){
  
  # variable names for year
  m <- paste0('medinc',yr[i])
  h <- paste0('h',yr[i])
  wi <- county[[paste0('pop',yr[i])]]
  
  # fit model
  modcounty[[i]] <- lm(paste0(m,'~',h,'+ I(',h,'^2)'),data = county,weight = wi)
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


# state -------------------------------------------------------------------



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
statesel <- modsel(state)
modstate = list()
par(mfrow = c(2,2),mar = c(4,4,3,1),las = 1)


for(i in 1:4){
  
  # variable names for year
  m <- paste0('medinc',yr[i])
  h <- paste0('h',yr[i])
  wi <- state[[paste0('pop',yr[i])]]
  
  # fit model
  modstate[[i]] <- lm(paste0(m,'~',h,'+ I(',h,'^2)'), data = state, weight = wi)
  #state[[paste0('hcfit',yr[i])]] <- modstate[[i]]$fitted.values
  
  # plot
  plot(exp(hstate[[i]]),state[[paste0('medinc',yr[i])]]#, main = year[i]
       ,xlab = expression(e^italic(H)),ylab = 'median income ratio',cex = 0.65,
       ylim = c(0.4,2.5),xlim = c(1,3),pch = 20,
       col = rgb(0,0,0,0.8))
  text(3,2.2,year[i],pos = 4,cex = 1.5)
  
  fit <- predict(modstate[[i]],nx)
  lines(nx[,i],fit,col = 'red')
  
}
title(main = expression("State:National Median Income vs. " * e^italic(H)),outer = T,line = -1.5)

