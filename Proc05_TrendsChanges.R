#=========================================================================================
# Proc 05. Trend analysis and changes between 2005 and 2018
# Author: Sebastian Gutierrez
# Data: January 04 2022
# PhD student Aarhus University
#=========================================================================================
rm(list = ls())

# 1) Set working directory ------------------------------------------------


setwd("~/AARHUS_PhD/DSMactivities/1_SOCseq/INPUTS/RASTER/Denmark-Landsat-Indices")


#  2) Load libraries ------------------------------------------------------



pckg <- c('raster',     
          'parallel',
          'RStoolbox',
          'nightmares',
          'sp',
          'doSNOW',
          'foreach',
          'rgdal',
          'readr',
          'sf',
          'doParallel',
          'trend'
)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

rm(pckg)
rm(usePackage)


# 3) Loading shapefiles with the time-series as an attribute --------------
points <- readRDS("PointsNDVI_LULC.rds")
points <- as.data.frame(points)

# 4) NA counting (2005-2020)----------------------------------------------------------
points <- points[,-c(1:6,21,22)]
points <- points[,-c(1:6)]
points$values <- rowSums(!is.na(points[,1:14]))
names(points)
head(points)
points <- points[,-c(1:6)]

# 5) Mk test y Sen´s slope batch functions -------------------------------------------------

# Function to perform the Mann-Kendall Trend Test

mk.batch <- function(data,row,values) {
  ifelse(values>=3,
         c(trend::mk.test(na.omit(unlist(c(data[row,-c(15:20)])))))$p.value,
         NA)
  }

# Function to compute the Sen´s slope for linear rate of change
sens.batch <-  function(data,row,values) {
  ifelse(values>=3,
         c(trend::sens.slope(na.omit(unlist(c(data[row,-c(15:20)])))))$estimates,
         NA)
  }

# Function to compute the Sen´s slope confidence interval
sens.batch.ci.low <-  function(data,row,values) {
  ifelse(values>=3,
         c(trend::sens.slope(na.omit(unlist(c(data[row,-c(15:20)])))))$conf.int[1],
         NA)
  }

# Function to compute the Sen´s slope confidence interval
sens.batch.ci.high <-  function(data,row,values) {
  ifelse(values>=3,
         c(trend::sens.slope(na.omit(unlist(c(data[row,-c(15:20)])))))$conf.int[2],
         NA)
  }

# 6) Mk test and Sen´s slope performing ----------------------------------------------

myCluster <- makeCluster(detectCores()-1, # number of cores to use
                         type = "SOCK") # type of cluster
registerDoParallel(myCluster)

start <- Sys.time()
{
  a <- foreach(i=1:nrow(points),.combine=c,.inorder=T) %dopar% {
    sens.batch(data=points,row=i,values=points$values[i])
  }
  
  b <- foreach(i=1:nrow(points),.combine = c,.inorder=T) %dopar% {
    mk.batch(data=points,row=i,values=points$values[i])
  }
  
  c <- foreach(i=1:nrow(points),.combine = c,.inorder=T) %dopar% {
    sens.batch.ci.low(data=points,row=i,values=points$values[i])
  }
  
  d <- foreach(i=1:nrow(points),.combine = c,.inorder=T) %dopar% {
    sens.batch.ci.high(data=points,row=i,values=points$values[i])
  }
}
Sys.time()-start
stopCluster(myCluster)

points$mk_pvalue <- b
points$sens_slope <- a
points$sens_slope_l <- c
points$sens_slope_h <- d

names(points)
head(points)

saveRDS(points,"Points_NDVI_LULC_0518.rds")
