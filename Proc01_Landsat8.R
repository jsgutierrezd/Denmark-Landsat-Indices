
#=========================================================================================
# Proc 01. Spectral indices generation for a yearly-series Landsat 8 images (2014- 2020)
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


# 3) Read Landsat 8 multi-band images from 2014 to 2020 -------------------

files <- list.files("O:/Tech_AGRO/Jord/Sebastian/LANDSAT8_DK/",
                             pattern = "tiff$|tif$",
                             full.names = TRUE)

all <- lapply(files,function(x){
  stack(x)
})



# 4) Creating functions to calculate spectral indices ---------------------

# 4.1) NDVI
ndvi <- function(x){
  ((x[[5]])-(x[[4]]))/
    ((x[[5]])+(x[[4]]))
}

# 4.2) BSI
#BSI = (SWIR2+R)−(NIR+B)/(SWIR2+R)+(NIR+B)
# bsi <- function(x){
#   ((x[[7]])+(x[[4]]))-((x[[5]])+(x[[2]]))/
#     ((x[[7]])+(x[[4]]))+((x[[5]])+(x[[2]]))
#   }


# 5) Generating spectral indices for the list of images -------------------

cores <- detectCores()-3
beginCluster(cores)

start <- Sys.time()
# data(lsat)
# all <- list(lsat,lsat)

# 5.1) NDVI
NDVI <- stack()
for(i in 1:length(all)){
  temp <- clusterR(all[[i]], 
                   fun=ndvi)
  NDVI <- stack(NDVI,temp)
}
NDVI

# # 5.2) BSI
# BSI <- stack()
# for(i in 1:length(all)){
#   temp <- clusterR(all[[i]], 
#                    fun=bsi)
#   BSI <- stack(BSI,temp)
# }
# BSI




# 6) Exporting results ----------------------------------------------------

writeRaster(NDVI,"NDVI2014-2020.tif",overwrite=T)
# writeRaster(BSI,"BSI2014-2020.tif")

endCluster()

print(Sys.time() - start)
