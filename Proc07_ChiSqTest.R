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
pckg <- c('corrplot')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

rm(pckg)
rm(usePackage)

# 3) Load database --------------------------------------------------------
data <- read.csv("ContTableL7L8_20052018.csv",sep=";",row.names = 1)
head(data)

# 4) Chi square test ------------------------------------------------------
chisq <- chisq.test(data)
chisq

# 5) Plotting residuals ---------------------------------------------------
round(chisq$residuals, 2)

stres <- chisq$stdres
res <- chisq$residuals
scres<- scale(chisq$residuals)

x11()
par(mfrow=c(1,3))
corrplot(res, is.cor = FALSE, method="color",
         col.lim = c(min(res), max(res)),
         col=colorRampPalette(c("red","yellow","green"))(200),
         addgrid.col = 'white',
         cl.pos = 'r',main="res")

corrplot(stres, is.cor = FALSE, method="color",
         col.lim = c(min(stres), max(stres)),
         col=colorRampPalette(c("red","yellow","green"))(200),
         addgrid.col = 'white',
         cl.pos = 'r',main="stres")

corrplot(scres, is.cor = FALSE, method="color",
         col.lim = c(min(scres), max(scres)),
         col=colorRampPalette(c("red","yellow","green"))(200),
         addgrid.col = 'white',
         cl.pos = 'r',main="res")
