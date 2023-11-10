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
} # LIBRERIA GENERAL

# actualizar_fluorescencia ####

data_fluo <- read_rds('data/data_processed/rds/data_fluorescencia.rds')

dates_fluo <- data_fluo |> 
  group_by(sitio) |> 
  distinct(fecha) 

files <- dir_ls('data/data_raw/fluorescencia',regexp = 'fluor_')
dates_new <- str_extract(files,'[0-9]{8}')

ind <- which(!(ymd(dates_new) %in% ymd(dates_fluo$fecha)))

sit <- str_remove_all(sapply(str_split(files[ind],'/'),function(x) x[4]),'fluor_|_[0-9]{8}.txt')

names_cols <- as.data.frame(read_csv('data/data_raw/fluorescencia/ubicacion_muestreo.csv'))

for (x in 1:length(ind)) {
  
  lineas <- readLines(files[ind][x])
  lineas <- lineas[6:length(lineas)]
  campos <- strsplit(lineas, "\t")
  datos <- list()
  names <- c()
  
  for (i in 2:length(lineas)) {
    datos[[i-1]] <- as.numeric(campos[[i]][2:length(campos[[i]])])
    names[i-1] <- campos[[i]][1]
  }
  
  data_new <- as.data.frame(setNames(datos, names))
  data_new[which(data_new$Mo %in% boxplot.stats(data_new$Mo)$out),1:ncol(data_new)] <- NA
  
  data_new <- data_new |> 
    mutate(sitio = sit[x],
           fecha = ymd(dates_new[ind])[x],
           codigo = names_cols[which(names_cols$sitio == sit[x]),2], 
           .before = Bckg) |>
    group_by(sitio, fecha, codigo) |> 
    summarise(across(Bckg:`DIo.RC`,.fns= mean,na.rm = TRUE))
  
  names(data_new) <- names(data_fluo)
  data_fluo <- rbind(data_fluo, data_new)
  
}

write_rds(data_fluo,'data/data_processed/rds/data_fluorescencia.rds')

