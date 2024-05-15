
library(fs)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(dplyr)
library(xlsx)

data_cepto <- read_rds('data/processed/ceptometro.rds')

dates_cepto <- data_cepto |> 
  group_by(sitio) |> 
  distinct(fecha) 

files <- dir_ls('data/raw/ceptometro',regexp = 'cepto_')
dates_new <- str_extract(files,'[0-9]{8}')

ind <- which(!(ymd(dates_new) %in% ymd(dates_cepto$fecha)))

sit <- str_remove_all(sapply(str_split(files[ind],'/'),function(x) x[4]),'cepto_|_[0-9]{8}.xls')

for (x in 1:length(ind)) {
  
  data_new <- read.xlsx(files[ind][x], sheetIndex = 1) |> filter(!is.na(Record.Type))
  
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
    group_by(sitio, fecha, tratamiento) |>
    summarise(across(above_par:longitud, \(x) mean(x, na.rm = TRUE))) |>
    mutate(temporada = '2023-2024') |>
    select(sitio, temporada, fecha, tratamiento, everything())
    
  data_cepto <- rbind(data_cepto, data_new)
  
}

data_cepto <- data_cepto |>
  arrange(fecha, tratamiento)

write_rds(data_cepto,'data/processed/ceptometro.rds')
