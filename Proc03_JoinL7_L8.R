#=========================================================================================
# Proc 03. NDVI time-series joining --> Landsat 7 (2000- 2013) and Landsat 8 (2014-2020)
# Author: Sebastian Gutierrez
# Data: December 13 2021
# PhD student Aarhus University
#=========================================================================================
rm(list = ls())

# 1) Set working directory ------------------------------------------------

setwd("~/AARHUS_PhD/DSMactivities/1_SOCseq/INPUTS/RASTER/Denmark-Landsat-Indices")


# 2) Load libraries -------------------------------------------------------

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


# 3) Loading NDVI 2000-2013 -----------------------------------------------

l00.13 <- stack("NDVI2000_2013.tif")


# 4) Loading NDVI 2014-2020 -----------------------------------------------

l14.20 <- stack("NDVI2014-2020.tif")

# 5) Loading limit -----------------------------------------------
lim <- rgdal::readOGR("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/1_SOCseq/INPUTS/SHP/LIMIT/LIMIT.shp")


# 6) Cropping and masking by limit ----------------------------------------

l00.13 <- crop(l00.13,lim)
l00.13 <- mask(l00.13,lim)

l14.20 <- crop(l14.20,lim)
l14.20 <- mask(l14.20,lim)


# 7) Stacking NDVI time-series --------------------------------------------

l14.20 <- resample(l14.20,l00.13)
l00.20 <- stack(l00.13,l14.20)
names(l00.20) <- paste0("NDVI_",2000:2020)


# 8) Saving raster stack NDVI annually time-series 2000-2020 --------------

writeRaster(l00.20,"NDVI2000-2020.tif",overwrite=T)
saveRDS(names(l00.20),"NamesNDVI2000-2020.rds")
