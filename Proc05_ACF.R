#=========================================================================================
# Proc 05. ACF
# Author: Sebastian Gutierrez
# Data: December 15 2021
# PhD student Aarhus University
#=========================================================================================
rm(list = ls())
# 1) Set working directory
setwd("~/AARHUS_PhD/DSMactivities/1_SOCseq/INPUTS/RASTER/Denmark-Landsat-Indices")

# 2) Load libraries

pckg <- c('raster',     
          'parallel',
          'RStoolbox',
          'nightmares',
          'sp',
          'doSNOW',
          'foreach',
          'rgdal',
          'readr',
          'sf'
)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

rm(pckg)
rm(usePackage)

# 3) Loading shapefiles with the time-series as an attribute
lu_ag <- read_sf("lu_ag.shp") 
lu_f_sn <- read_sf("lu_f_sn.shp")
lu_w <- read_sf("lu_w.shp")

#4) Removing out or range NDVI values

for(i in 1:(ncol(lu_ag)-1)) {
  lu_ag[[i]] <- ifelse(lu_ag[[i]]>1|lu_ag[[i]]< (-1),
                          NA,
                       lu_ag[[i]])
}

for(i in 1:(ncol(lu_f_sn)-1)) {
  lu_f_sn[[i]] <- ifelse(lu_f_sn[[i]]>1|lu_f_sn[[i]]< (-1),
                       NA,
                       lu_f_sn[[i]])
}

for(i in 1:(ncol(lu_w)-1)) {
  lu_w[[i]] <- ifelse(lu_w[[i]]>1|lu_w[[i]]< (-1),
                       NA,
                      lu_w[[i]])
}


summary(lu_ag)
summary(lu_f_sn)
summary(lu_w)

# 4) ACF
start <- Sys.time()
cl <- makeCluster(parallel::detectCores()-2) 
registerDoSNOW(cl) 
rows <- nrow(l00.20)
cols <- ncol(l00.20)

#plot(l00.20[[21]])
i=100
j=120
a <- foreach(i = rows) %:%
  foreach(j = cols) %dopar% {
    temp <- acf(ts(c(l00.20[i,j])),plot = F,lag.max=3)
    b <- temp$acf[2]###LAG 2
    a <- cbind(coordinates(l00.20),lag=b)
    a <- as.data.frame(a)
  }
a <- data.frame(a)
summary(a)
stopCluster(cl)
print(Sys.time() - start)



