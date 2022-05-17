#=========================================================================================
# Proc 05. Slope classification
# Author: Sebastian Gutierrez
# Data: January 05 2022
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
          'trend',
          'rgdal',
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


# 3) Load database --------------------------------------------------------

points <- readRDS("Points_NDVI_LULC_0518.rds")
class(points)
names(points)


# 4) Classify trend and significance ------------------------------------------

points$slope_class <- ifelse(points$sens_slope<0&points$mk_pvalue>0.1,
                             "N1",
                             ifelse(points$sens_slope<0&points$mk_pvalue>0.05&points$mk_pvalue<=0.1,
                                    "N2",
                                    ifelse(points$sens_slope<0&points$mk_pvalue<=0.05,
                                           "N3",
                                           ifelse(points$sens_slope>0&points$mk_pvalue>0.1,
                                                  "P1",
                                                  ifelse(points$sens_slope>0&points$mk_pvalue>0.05&points$mk_pvalue<=0.1,
                                                         "P2",
                                                         ifelse(points$sens_slope>0&points$mk_pvalue<=0.05,
                                                                "P3",NA))))))


# 5) LULC changes 2005-2020 -----------------------------------------------

points$lu_2005 <- factor(round(points$lu_2005,0))
points$lu_2020 <- factor(round(points$lu_2020,0))
levels(points$lu_2005) <- c("Settlement","WetlandPermanent","Woodland","ChristmasTrees","Cropland",
                            "Grasslands","WetlandPeriodic","Unclassified","OtherLandUse","Sea")
levels(points$lu_2020) <- c("Settlement","WetlandPermanent","Woodland","ChristmasTrees","Cropland",
                            "Grasslands","WetlandPeriodic","Unclassified","OtherLandUse","Sea")
head(points)
points$lu_0520 <- paste0(points$lu_2005,"_",points$lu_2020)

write.csv(table(points$lu_0520,points$slope_class),"ContTableL7L8_20052020.csv")


as.factor(points$slope_class)

# 6) Rasterize attributes -------------------------------------------------
points_sp <- vect("PointsDef.shp")
df <- points
df <- vect(df, geom=c("x", "y"), crs=crs(points_sp))

empty_raster<-rast("demdetrend.tif")*0

# Replace Na values for zero values in the column to be rasterized
df$slope_class[is.na(df$slope_class)] <- 0


# Points to Raster slope classification
df$slope_class <- as.numeric(as.factor(df$slope_class))
summary(df$slope_class)
df_map<-rasterize(df, empty_raster ,df$slope_class, updateValue='all')
df_map
plot(df_map)
terra::writeRaster(df_map,"SensSlopeClassL7L8.tif",overwrite=TRUE)


# Points to Raster Slope value
df$sens_slope <- as.numeric(df$sens_slope)
summary(df$sens_slope)
df_map<-rasterize(df, empty_raster ,df$sens_slope, updateValue='all')
df_map
plot(df_map)
terra::writeRaster(df_map,"SensSlopeL7L8.tif",overwrite=TRUE)
