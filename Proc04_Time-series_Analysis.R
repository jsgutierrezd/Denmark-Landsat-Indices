#=========================================================================================
# Proc 04. NDVI time-series analysis
# Author: Sebastian Gutierrez
# Data: December 13 2021
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
          'readr'
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

# 4) Loading point-shapefiles by land uses (agriculture, forest and semi-natural, and wetlands)

# lu_ag <- match(lu, c(2))
# lu.f.sn <- match(lu,(3))
# lu.w <- match(lu,(4))

lu_ag <- read_csv("lu_ag.csv") %>% data.frame() %>% na.omit()
coordinates(lu_ag) <- ~x+y
lu_ag@proj4string <- CRS("+proj=utm +zone=32 +ellps=intl +units=m +no_defs")

lu_f_sn <- read_csv("lu_f_sn.csv") %>% data.frame() %>% na.omit()
coordinates(lu_f_sn) <- ~x+y
lu_f_sn@proj4string <- CRS("+proj=utm +zone=32 +ellps=intl +units=m +no_defs")

lu_w <- read_csv("lu_w.csv") %>% data.frame() %>% na.omit()
coordinates(lu_w) <- ~x+y
lu_w@proj4string <- CRS("+proj=utm +zone=32 +ellps=intl +units=m +no_defs")

# 5) Extracting values from NDVI time-series by agriculture land-use/land cover
lu_ag <- raster::extract(l00.20,lu_ag,sp=T)
lu_f_sn <- raster::extract(l00.20,lu_f_sn,sp=T)
lu_w <- raster::extract(l00.20,lu_w,sp=T)

# 6) Exporting point-shapefile with time-series as attibutes
writeOGR(lu_ag, ".", "lu_ag", driver="ESRI Shapefile",overwrite=TRUE) 
writeOGR(lu_f_sn, ".", "lu_f_sn", driver="ESRI Shapefile",overwrite=TRUE) 
writeOGR(lu_w, ".", "lu_w", driver="ESRI Shapefile",overwrite=TRUE)



# 4) Counting missing values by pixel stack
limit <- rgdal::readOGR("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/1_SOCseq/INPUTS/SHP/LIMIT/LIMIT.shp")
start <- Sys.time()
detectCores()
beginCluster(n = detectCores()-3 )
fun1 <- function(x) { (sum(is.na(x))/21)*100}
l00.20.NAs <- clusterR(l00.20, fun=fun1)
l00.20.NAs <- mask(l00.20.NAs, limit)
l00.20.NAs <- crop(l00.20.NAs,limit)
l00.20.NAs[l00.20.NAs > 0] <- NA
plot(l00.20.NAs)
writeRaster(l00.20.NAs,"NDVI_Landsat_NAs.tif",overwrite=T)

endCluster()
print(Sys.time() - start)

# 5) Computing temporal autocorrelation layers at different lags
rm(list = ls())
l00.20 <- stack("NDVI2000-2020.tif")
names(l00.20) <- readRDS("NamesNDVI2000-2020.rds")

# 5.1) Stack to data.frame
library(reshape2)
library(magrittr)
start <- Sys.time()
#table <- cbind(coordinates(l00.20),as.data.frame(l00.20))
# table <- as.data.frame(l00.20, xy = TRUE) %>%
#   melt(id.vars = c('x','y'))
write.table(table,"NDVI2000-2020.txt",row.names = F)
print(Sys.time() - start)

write.csv(table,"NDVI2000-2020.csv",row.names = F)



#l00.20mask <- mask(l00.20, l00.20.NAs)
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



r <- raster(ncols=3, nrows=3)
cbind(coordinates(s),as.data.frame(s))
values(r) <- c(1:4,NA,NA,NA,8,9)
s <- stack(r, r*2, sqrt(r))

start <- Sys.time()
a <- data.frame(0)
# i=1
# j=1
# k=1
for(i in 1:nrow(l00.20)) {
  for(j in 1:ncol(l00.20)) {
    temp <- acf(ts(c(l00.20[i,j])),plot = F,lag.max=3)
    b <- temp$acf[2]
    a <- as.data.frame(cbind(coordinates(l00.20),lag2=b))
  }
}
a
class(a)
coordinates(a) <- ~x+y
gridded(a) <- TRUE
c <- raster(a)
print(Sys.time() - start)



