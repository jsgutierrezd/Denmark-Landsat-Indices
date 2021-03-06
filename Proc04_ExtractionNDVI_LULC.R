#=========================================================================================
# Proc 04. Extraction NDVI time-series and LULC 2005-2018
# Author: Sebastian Gutierrez
# Data: December 13 2021
# PhD student Aarhus University
#=========================================================================================
rm(list = ls())

# 1) Set working directory ------------------------------------------------

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
          'magrittr',
          'rgdal',
          'sf',
          'terra'
)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

rm(pckg)
rm(usePackage)


# 3) Loading NDVI 2000-2020 -----------------------------------------------
NDVI05.20 <- rast("NDVImed2005_2020.tif")
names(NDVI05.20) <- readRDS("NamesNDVImed2005_2020.rds")


# 4) Loading LULC2005 and LULC2020 ----------------------------------------
lu2005 <- rast("lu_2005.tif")
lu2020 <- rast("lu_2020.tif")
lu05.20 <- c(lu2005,lu2020)
#lu05.20 <- terra::project(lu05.20, NDVI05.20)
rm(lu2005)
rm(lu2020)

# 5) Reference raster of wetland areas ------------------------------------
#Raster to points of a wetland areas raster layer to make an extraction by points from NDVI2000-2020 time-series.
ref <- raster("O:/Tech_AGRO/Jord/ambe/covariates_wetlands/wetland_only.tif")
ref <- rasterToPoints(ref) %>% data.frame
coordinates(ref) <- ~x+y
ref@proj4string <- CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
writeOGR(ref,".", "PointsDef", driver="ESRI Shapefile",overwrite=TRUE)

# 6) Extracting values from NDVI time-series by points for wetland areas --------
start <- Sys.time()
points <- vect("PointsDef.shp")
g <- geom(points)
points$x <- g[,3]
points$y <- g[,4]
points <- terra::project(points, NDVI05.20)
points <- cbind(points,terra::extract(NDVI05.20, points))


# 7) Extracting values for LULC2005 and LULC2020 --------------------------
points <- cbind(points,terra::extract(lu05.20, points))
names(points)

Sys.time()-start

# 8) Removing out of range NDVI values ------------------------------------
class(points)
points <- as.data.frame(points)

for(i in 5:20) {
  points[[i]] <- ifelse(points[[i]]>1|points[[i]]<= 0,
                       NA,
                       points[[i]])
}

# 9) Saving final table ---------------------------------------------------
saveRDS(points,"PointsNDVI_LULC.rds")
