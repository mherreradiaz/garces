library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(agvAPI)
library(ggplot2)
library(ggpubr)
library(cowplot)
options(timeout = max(5000, getOption("timeout")))
source('script/funciones/read_yara.R')
dia <- function(año, mes) {
  primer_dia_ajustado <- as.Date(paste(año, mes, "01", sep = "-")) - days(1)
  ultimo_dia_ajustado <- as.Date(paste(año, mes, "01", sep = "-")) + months(1) 
  return(c(primer_dia_ajustado, ultimo_dia_ajustado))
}
cor_clima <- function(turgor,temperatura,vpd) {
  
  if (nrow(na.omit(data.frame(turgor,temperatura,vpd)))==0) {return(NA)} else {
    
    cor_temperatura <- cor(turgor,temperatura, use = 'complete.obs')^2
    cor_vpd <- cor(turgor,vpd, use = 'complete.obs')^2
    
    cor_index <- (cor_temperatura+cor_vpd)/2
    
    return(cor_index)
    
  }
}
dif_mean <- function(turgor,temperatura,vpd) {
  
  if (nrow(na.omit(data.frame(turgor,temperatura,vpd)))==0) {return(NA)} else {
    
    dif_t <- mean(abs(turgor-temperatura))
    dif_vpd <- mean(abs(turgor-vpd))
    
    dif_index <- (dif_t+dif_vpd)/2
    
    return(dif_index)
    
  }
  
}
modal <- function(x) {
  # Calcular la tabla de frecuencias
  tabla_frecuencia <- table(x)
  
  # Encontrar el valor con la frecuencia máxima (la moda)
  moda <- as.numeric(names(tabla_frecuencia)[which.max(tabla_frecuencia)])
  
  return(moda)
}

# Turgor

metadata <- read_csv2('data/metadata/metadata_yara.csv')
codigo_tur <- read_csv2('data/metadata/codigos_zim_turgor.csv')

data_codigo_tur <- left_join(codigo_tur,metadata,by=c('sensor' = 'identificador'))

device_id_tur <- data_codigo_tur |> 
  filter(grepl('Yara Water-Sensor',tipo)) |> 
  pull(device_id)

data_old <- read_rds('data/data_processed/zim_turgor_mediahora.rds')
from_date <- data_old$fecha |> max() |> substr(1,16)

dest <- paste0('data/data_raw/zim_turgor_mediahora',
               substr(gsub("[^0-9]", "",as.character(lubridate::now())),1,8),'.csv')

read_yara(device_id_tur,from_date = from_date,
          until_date = as.character(lubridate::now()+days(1)),
          dest_file = dest)
  
data_new <- read_delim(dest,delim = '\t') |> 
  select(1,contains('Yara')) |> 
  mutate(across(where(is.character),as.numeric)) |> 
  pivot_longer(-1,names_to = 'sensor') |> 
  rename_with(.fn = \(x) 'datetime',.cols = starts_with('Times')) |>
  mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |> 
  mutate(sensor =str_extract(str_extract(sensor,'[0-9]{4}\\['),'[0-9]{4}')) |> 
  group_by(sensor,fecha = floor_date(datetime,'30 minutes')) |> 
  summarize(value = mean(value,na.rm = T)) |> 
  mutate(sensor = as.numeric(sensor)) |> 
  left_join(codigo_tur,by = 'sensor') |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |> 
  rename(turgor = value) |>
  mutate(temporada = '2023-2024',
         unidad = factor(unidad, levels = 1:3),
         hora = format(as.POSIXct(fecha), format = "%H:%M"),
         zim = substr(codigo,nchar(codigo)-1,nchar(codigo)),
         codigo = substr(codigo,1,nchar(codigo)-2),
         fecha = format(as.POSIXct(fecha), format = "%Y-%m-%d")) |>
  select(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,zim,sensor,turgor) |>
  arrange(fecha,sitio,hora,tratamiento,unidad,zim) |>
  ungroup()

data_turgor <- data_old |>
  bind_rows(data_new) |>
  distinct() |>
  arrange(fecha,sitio,hora,tratamiento,unidad,zim)

write_rds(data_turgor,'data/data_processed/zim_turgor_mediahora.rds')

# Clima

data_old <- read_rds('data/data_processed/clima_mediahora.rds')

periodo <- c(as.Date(max(data_old$fecha))-1,substr(now(),1,10))

data_t <- getDataAGV_clima(station_id ='00205018', var = 'Temperature',
                           time_span = periodo) |>
  mutate(sitio = 'la_esperanza', .before = datetime) |> 
  rbind(getDataAGV_clima(station_id ='00203E6E', var = 'Temperature',
                         time_span = periodo) |>
          mutate(sitio = 'rio_claro', .before = datetime)) |> 
  mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |> 
  group_by(sitio,datetime = floor_date(datetime,'30 minutes')) |> 
  summarize(t_media = mean(`avg (°C)`,na.rm = T))

data_vpd <- getDataAGV_clima(station_id ='00205018', var = 'VPD',
                             time_span = periodo) |>
  mutate(sitio = 'la_esperanza', .before = datetime) |> 
  rbind(getDataAGV_clima(station_id ='00203E6E', var = 'VPD',
                         time_span = periodo) |>
          mutate(sitio = 'rio_claro', .before = datetime)) |> 
  mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |> 
  group_by(sitio,datetime = floor_date(datetime,'30 minutes')) |> 
  summarize(vpd_media = mean(`avg (mbar)`,na.rm = T))

data_new <- data_t |> 
  left_join(data_vpd, by = c('sitio','datetime')) |> 
  mutate(fecha = format(as.POSIXct(datetime), format = "%Y-%m-%d"),
         hora = format(as.POSIXct(datetime), format = "%H:%M"),
         .before = datetime) |> 
  select(-datetime)

data_clima <- data_old |>
  bind_rows(data_new) |>
  distinct() |>
  arrange(temporada,sitio,fecha,hora)

write_rds(data_clima, 'data/data_processed/clima_mediahora.rds')

# Preprocesado

data_turgor <- read_rds('data/data_processed/zim_turgor_mediahora.rds') |> 
  group_by(sitio,temporada,fecha,tratamiento,unidad,codigo,zim,sensor) |> 
  mutate(turgor_sc = scale(turgor)) |> 
  ungroup()

data_clima <- read_rds('data/data_processed/clima_mediahora.rds') |> 
  mutate(temporada = ifelse(fecha < '2023-06-01','2022-2023','2023-2024'),
         .before = fecha) |> 
  group_by(sitio,temporada,fecha) |> 
  mutate(t_sc = scale(t_media),
         vpd_sc = scale(log(vpd_media+1))) |> 
  ungroup()

data_ccf <- data_turgor |> 
  left_join(data_clima, by = c('sitio','temporada','fecha','hora')) |>  
  na.omit() |> 
  group_by(sitio,temporada,sensor) |> 
  summarise(t_lag = ccf(as.numeric(t_sc), as.numeric(turgor_sc), plot = F)$lag[
    which.max(as.numeric(ccf(as.numeric(t_sc), as.numeric(turgor_sc), plot = F)$acf))],
            vpd_lag = ccf(as.numeric(vpd_sc), as.numeric(turgor_sc), plot = F)$lag[
    which.max(as.numeric(ccf(as.numeric(vpd_sc), as.numeric(turgor_sc), plot = F)$acf))]) |> 
  ungroup() |> 
  group_by(sitio,temporada) |> 
  summarise(t_lag = modal(t_lag),
            vpd_lag = modal(vpd_lag)) |> 
  ungroup()

data_t <- data_clima |> 
  left_join(data_ccf,by=c('sitio','temporada')) |> 
  select(sitio,temporada,fecha,hora,t_sc,t_lag) |>
  mutate(datetime = as.POSIXct(paste(fecha,hora),format = '%Y-%m-%d %H:%M')-(30*60)*t_lag,
         fecha = format(datetime,format = '%Y-%m-%d'),
         hora = format(datetime,format = '%H:%M')) |> 
  select(-datetime) |> 
  distinct(sitio,temporada,fecha,hora,.keep_all = T)

data_vpd <- data_clima |> 
  left_join(data_ccf,by=c('sitio','temporada')) |> 
  select(sitio,temporada,fecha,hora,vpd_sc,vpd_lag) |>
  mutate(datetime = as.POSIXct(paste(fecha,hora),format = '%Y-%m-%d %H:%M')-(30*60)*vpd_lag,
         fecha = format(datetime,format = '%Y-%m-%d'),
         hora = format(datetime,format = '%H:%M')) |> 
  select(-datetime) |> 
  distinct(sitio,temporada,fecha,hora,.keep_all = T)

data_clima_lag <- data_t |> 
  left_join(data_vpd,by=c('sitio','temporada','fecha','hora')) |> 
  select(sitio,temporada,fecha,hora,t_sc,vpd_sc) |> 
  rename(t_sc_lag = t_sc, vpd_sc_lag = vpd_sc)

data_clima_lag |> 
  left_join(data_clima, by = c('sitio','temporada','fecha','hora')) |> 
  write_rds('data/data_processed/clima_mediahora.rds')

data_cor_lag <- data_turgor |> 
  left_join(data_clima_lag,by=c('sitio','temporada','fecha','hora')) |> 
  group_by(sitio,temporada,fecha,sensor) |> 
  summarise(cor_index = as.numeric(cor_clima(turgor_sc,t_sc_lag,vpd_sc_lag)),
            dif_index = dif_mean(turgor_sc,t_sc_lag,vpd_sc_lag)) |>
  ungroup()

filtro <- data_cor_lag |> 
  filter(cor_index > .5,
         dif_index < 1) |> 
  mutate(filtro = paste(sitio,fecha,sensor)) |> 
  pull(filtro)

data_limpia <- data_turgor |> 
  filter(paste(sitio,fecha,sensor) %in% filtro)

write_rds(data_limpia,'data/data_processed/zim_turgor_mediahora_preprocesado.rds')
