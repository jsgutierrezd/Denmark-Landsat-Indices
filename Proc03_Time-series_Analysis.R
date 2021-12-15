#=========================================================================================
# Proc 04. NDVI time-series analysis
# Author: Sebastian Gutierrez
# Data: December 13 2021
# PhD student Aarhus University
#=========================================================================================

# 1) Set working directory
setwd("~/AARHUS_PhD/DSMactivities/1_SOCseq/INPUTS/RASTER/Denmark-Landsat-Indices")

# 2) Load libraries

pckg <- c('raster',     
          'parallel',
          'RStoolbox',
          'nightmares'
)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

rm(pckg)
rm(usePackage)

# 3) Loading NDVI 2000-2020
l00.20 <- stack("NDVI2000-2020.tif")
names(l00.20) <- readRDS("NamesNDVI2000-2020.rds")

# 4) Counting missing values by pixel stack
detectCores()
beginCluster(n = detectCores()-3 )
fun1 <- function(x) { (sum(is.na(x))/21)*100}
l00.20.NAs <- clusterR(l00.20, fun=fun1)
l00.20.NAs <- mask(l00.20.NAs, limit)
l00.20.NAs <- crop(l00.20.NAs,limit)
writeRaster(l00.20.NAs,"NDVI_Landsat_NAs.tif",overwrite=T)
endCluster()


