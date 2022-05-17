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
          'nightmares',
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


# 3) Load layers ----------------------------------------------------------

all <- list.files("O:/Tech_AGRO/Jord/Sebastian/NDVI1985_2021/",
                  pattern = "tiff$|tif$",
                  full.names = TRUE
)

all <- lapply(all[21:36],function(x){
  rast(x)
})




# 4) Median NDVI 2005-2020 ------------------------------------------------

# 
# Each raster file (.tif) contains five raster layers:
#   
# Annual median of NDVI
# Annual mean of NDVI
# Annual standard deviation of NDVI
# Annual 10th percentile of NDVI
# Annual 90th percentile of NDVI

# NDVI median
NDVImed <- rast()
for (i in 1:16) {
  tmp <- all[[i]][[1]]
  NDVImed <- c(NDVImed,tmp)
}

names(NDVImed) <- paste0("NDVI_",2005:2020)

# 5) Saving raster stack NDVI annually time-series 2005-2020 --------------
writeRaster(NDVImed,"NDVImed2005_2020.tif",overwrite=T)
# writeRaster(NDVImed,"NDVI2000-2020.tif",overwrite=T)
saveRDS(names(NDVImed) ,"NamesNDVImed2005_2020.rds")
