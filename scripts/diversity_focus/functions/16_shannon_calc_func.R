# shannon index diversity calculation function with race, ethnicity data

# setup
wd <- "~/Documents/research/spatial-income-project" # kyra pop wd
setwd(wd)


library(terra)
library(geodata)
library(hexbin)


h_calc = function(level){
  
  finames = c("./ipums/tract_level_ethnicity",  "./ipums/county_level_ethnicity", "./ipums/state_level_ethnicity", "./ipums/nation_level_ethnicity")
  fi = finames[grep(level,finames)]
  df = readRDS(fi)

yr <- c('80', '90', '00' , '10','20')
year <- c(1980,1990,2000,2010,2020)
hdf <- list()

for (i in 1:length(year)){
  
  w <- ifelse(df[[paste0('pwhite', yr[i])]] == 0, 1, df[[paste0('pwhite', yr[i])]])
  b <- ifelse(df[[paste0('pblack', yr[i])]] == 0, 1, df[[paste0('pblack', yr[i])]])
  a <- ifelse(df[[paste0('pa_nat', yr[i])]] == 0, 1, df[[paste0('pa_nat', yr[i])]])
  n <- ifelse(df[[paste0('pother', yr[i])]] == 0, 1, df[[paste0('pother', yr[i])]])
  o <- ifelse(df[[paste0('phisp', yr[i])]] == 0, 1, df[[paste0('phisp', yr[i])]])
  
  hdf[[i]] <- -1*(w * log(w)+ b * log(b)+ a * log(a)+ n * log(n)+o * log(o))
  df[[paste0('h',yr[i])]] <- exp(hdf[[i]])
  
}
}



