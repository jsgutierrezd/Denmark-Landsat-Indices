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

#  7) ACF and PACF functions --------------------------------------------------------

acf.num <-  function(data,row,values,lag.max) {
  ifelse(values!=0,
         c(acf(ts(na.omit(unlist(c(data[row,]))[-c(17:20)])),plot = F,lag.max=lag.max))$acf[lag.max+1],
         NA)
  #return(a)
}

pacf.num <-  function(data,row,values,lag.max) {
  ifelse(values!=0,
         c(pacf(ts(na.omit(unlist(c(data[row,]))[-c(17:20)])),plot = F,lag.max=lag.max))$acf[lag.max+1],
         NA)
  #return(a)
}


# 8) ACF function for different lags points ------------------------------

{
  
  lag.max=0
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag0 <- a
  }
  
  lag.max=1
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag1 <- a
  }
  
  lag.max=2
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag2 <- a
  }
  
  lag.max=3
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag3 <- a
  }
  
  lag.max=4
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag4 <- a
  }
  
  lag.max=5
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag5 <- a
  }
  
  lag.max=6
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag6 <- a
  }
  
  lag.max=7
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag7 <- a
  }
  
  lag.max=8
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag8 <- a
  }
  
  lag.max=9
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag9 <- a
  }
  
  lag.max=10
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      acf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$acf_lag10 <- a
  }
  
}

# 9) PACF function for different lags lu_f_sn -----------------------------



{
  
  lag.max=1
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag1 <- a
  }
  
  lag.max=2
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag2 <- a
  }
  
  lag.max=3
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag3 <- a
  }
  
  lag.max=4
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag4 <- a
  }
  
  lag.max=5
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag5 <- a
  }
  
  lag.max=6
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag6 <- a
  }
  
  lag.max=7
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag7 <- a
  }
  
  lag.max=8
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag8 <- a
  }
  
  lag.max=9
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag9 <- a
  }
  
  lag.max=10
  {
    a <- foreach(i=1:nrow(points),.combine = c) %dopar% {
      pacf.num(data=points,row=i,values=points$values[i],lag.max=lag.max)
    }
    points$pacf_lag10 <- a
  }
  
}
print(Sys.time() - start)
stopCluster(myCluster)

points2 <- points
saveRDS(points,"Points_NDVI_LULC.rds")


