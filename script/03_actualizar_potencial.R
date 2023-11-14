library(fs)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(dplyr)
library(xlsx)

data_potencial <- read_rds('data/data_processed/rds/data_potencial.rds')

dates_potencial <- data_potencial |> 
  group_by(sitio) |> 
  distinct(fecha) 

files <- dir_ls('data/data_raw/potencial',regexp = 'potencial_')
dates_new <- str_extract(files,'[0-9]{8}')

ind <- which(!(ymd(dates_new) %in% ymd(dates_potencial$fecha)))

sit <- str_remove_all(sapply(str_split(files[ind],'/'),function(x) x[4]),'potencial_|_[0-9]{8}.xlsx')

for (x in 1:length(ind)) {
  
  data_new <- read.xlsx(files[ind][x], sheetIndex = 1)
  
  data_new <- data_new |> 
    rename(potencial_bar = bar) |>
    mutate(fecha = ymd(dates_new[ind][x]),
           .before = codigo) |>
    mutate(sitio = sit[x],
           .before = fecha) |>
    mutate(unidad = rep(1:3,5)) |>
    group_by(sitio, fecha, codigo)
  
  data_potencial <- rbind(data_potencial, data_new)
  
}

data_potencial <- data_potencial[order(data_potencial$fecha),]

write_rds(data_potencial,'data/data_processed/rds/data_potencial.rds')
