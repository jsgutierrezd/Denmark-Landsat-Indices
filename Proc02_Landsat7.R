
#=========================================================================================
# Proc 02. Spectral indices generation for a yearly-series Landsat 7 images (2000- 2013)
# Author: Sebastian Gutierrez
# Data: December 06 2021
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



# 3) Read Landsat 8 multi-band images from 2000 to 2013 -------------------

files <- list.files("O:/Tech_AGRO/Jord/Sebastian/LANDSAT7_DK/",
                    pattern = "tiff$|tif$",
                    full.names = TRUE)

all <- lapply(files,function(x){
  stack(x)
})


# 4) Creating functions to calculate spectral indices ---------------------

# 4.1) NDVI
ndvi <- function(x){
  ((x[[4]])-(x[[3]]))/
    ((x[[4]])+(x[[3]]))
}

# # 4.2) BSI
# #BSI = (SWIR2+R)−(NIR+B)/(SWIR2+R)+(NIR+B)
# bsi <- function(x){
#   ((x[[7]])+(x[[3]]))-((x[[4]])+(x[[1]]))/
#     ((x[[7]])+(x[[3]]))+((x[[4]])+(x[[1]]))
# }

# 5) Generating spectral indices for the list of images -------------------

cores <- detectCores()
raster::beginCluster(cores)

start <- Sys.time()

# 5.1) NDVI
NDVI <- stack()
for(i in 1:length(all)){
  temp <- clusterR(all[[i]], 
                   fun=ndvi)
  NDVI <- stack(NDVI,temp)
}
NDVI
writeRaster(NDVI,"NDVI2000_2013.tif",overwrite=T)


# # 5.2) BSI
# BSI <- stack()
# for(i in 1:length(all)){
#   temp <- clusterR(all[[i]], 
#                    fun=bsi)
#   BSI <- stack(BSI,temp)
# }
# BSI


# 6) Exporting results ----------------------------------------------------

# writeRaster(NDVI,"NDVI2000_2013.tif")
# writeRaster(BSI,"BSI2000_2013.tif")
endCluster()
print(Sys.time() - start)


