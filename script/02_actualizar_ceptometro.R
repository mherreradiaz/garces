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
  library(zoo)
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
} # LIBRERIA GENERAL

# ACTUALIZAR CEPTOMETRO ####

data_cep <- read.csv('data/data_processed/rds/data_ceptometro.csv')
data_cep <- data_cep[-c(1:nrow(data_cep)),]

dates_cep <- data_cep |> 
  group_by(sitio) |> 
  distinct(fecha) 

files <- dir_ls('data/data_raw/ceptometro',regexp = 'cepto_')
dates_new <- str_extract(files,'[0-9]{8}')

ind <- which(!(ymd(dates_new) %in% ymd(dates_cep$fecha)))

sit <- str_remove_all(sapply(str_split(files[ind],'/'),function(x) x[4]),'cepto_|_[0-9]{8}.xls')

for (x in 1:length(ind)) {
  
  data_new <- read.xlsx(files[ind][x], sheetIndex = 1)
  
  t_col <- rep(substr(na.omit(unique(data_new$Annotation))[1:5],1,2), 
                 times = diff(c(which(!is.na(data_new$Annotation)),nrow(data_new)+1))-1)
  
  data_new <- data_new |> 
    rename(above_par = Average.Above.PAR, 
           below_par = Average.Below.PAR,
           lai = Leaf.Area.Index..LAI.,
           latitud = Latitude,
           longitud = Longitude) |>
    select(above_par, below_par, lai, latitud, longitud) |>
    filter(above_par != 0) |>
    mutate(fecha = ymd(dates_new[ind][x]),
           sitio = sit[x],
           tratamiento = t_col,
           .before = above_par) |>
    group_by(fecha, sitio, tratamiento) |>
    summarise(across(above_par:longitud,.fns= mean,na.rm = TRUE))
    
  data_cep <- rbind(data_cep, data_new)
  
}

write_rds(data_cep,'data/data_processed/rds/data_ceptometro.rds')
