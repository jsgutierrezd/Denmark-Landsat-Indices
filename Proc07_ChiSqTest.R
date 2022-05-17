#=========================================================================================
# Proc 07. Chi square test
# Author: Sebastian Gutierrez
# Data: January 12 2022
# PhD student Aarhus University
#=========================================================================================

rm(list = ls())

# 1) Set working directory ------------------------------------------------
setwd("~/AARHUS_PhD/DSMactivities/1_SOCseq/INPUTS/RASTER/Denmark-Landsat-Indices")

#  2) Load libraries ------------------------------------------------------
pckg <- c('corrplot',
          'rasterVis',      # Visualizaton methods for raster data
          'RColorBrewer'
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
data <- read.csv("ContTableL7L8_20052020.csv",sep=";",row.names = 1)
head(data)

# 4) Chi square test ------------------------------------------------------
chisq <- chisq.test(data)
chisq

# 5) Plotting residuals ---------------------------------------------------
round(chisq$residuals, 2)


# Function to normalize data
normalize <- function(x) {
  return ((x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T)))
}


res <- raster(matrix(chisq$residuals,25,6))
extent(res) <- c(1,6,1,25)
plot(res)

stres <- raster(matrix(chisq$stdres,25,6))
extent(stres) <- c(1,6,1,25)
plot(stres)

scres <- raster(matrix(scale(chisq$residuals),25,6))
extent(scres) <- c(1,6,1,25)
plot(scres)

normres <- raster(matrix(normalize(chisq$residuals),25,6))
extent(normres) <- c(1,6,1,25)
plot(normres)


x11()
#values(scres)
levelplot(scres,
          margin=FALSE,
          colorkey=list(
            space='right',                   
            labels=list(at=-4:4, font=4),
            axis.line=list(col='black')),       
          par.settings=list(
            axis.line=list(col='transparent') 
          ),
          scales=list(draw=T),            
          col.regions=colorRampPalette(brewer.pal(5, 'RdBu')),                   
          at=seq(-4,4, len=8)
          )

display.brewer.all(colorblindFriendly=TRUE)

#Contribution plot

contrib <- 100*chisq$residuals^2/chisq$statistic
contrib <- raster(matrix(contrib,25,6))
extent(contrib) <- c(1,6,1,25)
plot(contrib)

x11()
#values(scres)
levelplot(contrib,
          margin=FALSE,
          colorkey=list(
            space='right',                   
            labels=list(at=0:20, font=4),
            axis.line=list(col='black')),       
          par.settings=list(
            axis.line=list(col='transparent') 
          ),
          scales=list(draw=T),            
          col.regions=colorRampPalette(brewer.pal(10, 'YlOrRd')),                   
          at=seq(0,20, len=15)
)
