{
  library(raster)
  library(rgdal)
  library(rgeos)
  library(trend)
  library(readxl)
  library(hydroGOF)
  library(Fgmutils)
  library(ggpubr)
  library(tidyverse)
  library(tsibble)
  library(feasts)
  library(rapportools)
  library(dunn.test)
  library(FSA)
  library(beepr)
  library(robustbase)
  library(RColorBrewer)
  library(ape)
  library(terra)
  library(png)
  library(xlsx)
  library(easyNCDF)
  library(ncdf4)
  library(readr)
  library(plotrix)
  library(fs)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(dplyr)
  library(tidyverse)
  library(hrbrthemes)
  library(viridis)
  library(dunn.test)
  library(rcompanion)
  options(mc_doScale_quiet=TRUE)
  windowsFonts(A = windowsFont("Times New Roman"))
  p <- function(...) {
    
    return(paste0(...))
  }
  remove.na <- function(x) {
    x <- x[!is.na(x)]
    return(x)
  }
  df_to_vector <- function(x) {
    lista <- list()
    for (i in 1:ncol(x)) {
      lista[[i]] <- x[,i]
    }
    return(unlist(lista))
  }
  d.stats <- function(x) {
    lista <- list("max" = max(x,na.rm =T),"min"= min(x,na.rm = T), "mean" = mean(x,na.rm =T))
    return(lista)
  }
  suma <- function(x) {if(length(which(is.na(x)))>0) {return(NA)} else{return(sum(x))}}
  reclass <- function(x,rcl,categorize=F) {
    if (categorize == F) {
      unlist(lapply(x,FUN= function(i) {
        if (length(which(rcl==i))==0) {return(i)} 
        else {return(rcl[which(rcl==i),2])}}))
    }
    else {
      unlist(lapply(x,FUN= function(i) {
        if (length(which(rcl==i))==0) {return(NA)} 
        else {return(rcl[which(rcl==i),2])}}))
    }
  }
  which.various <- function(x,y, sort=T) {
    n <- list()
    y <- unique(y)
    
    for (i in 1:length(y)) {
      n[[i]] <- which(x==y[i])
    }
    
    if(sort==T) {return(sort(unlist(n)))} else {return(unlist(n))}
  }
  check.int <- function(x) {
    x == round(x)
  }
} # LIBRARY

files <- list.files('data/data_raw/fluorescencia/',full.names = T)




lineas <- readLines(files[1])
lineas <- lineas[6:length(lineas)]
campos <- strsplit(lineas, "\t")
datos <- list()
names <- c()

for (i in 1:length(lineas)) {
  if (i==1) {datos[[i]] <- as.Date(substr(campos[[i]][2:length(campos[[i]])],11,20),c("%d.%m.%Y"))}
  else {datos[[i]] <- as.numeric(campos[[i]][2:length(campos[[i]])])}
  names[i] <- campos[[i]][1]
}

df <- as.data.frame(setNames(datos, names))
