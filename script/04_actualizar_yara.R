library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(agvAPI)
options(timeout = max(3600, getOption("timeout")))
source('script/funciones/read_yara.R')

# download yara data

metadata <- read_csv2('data/metadata/metadata_yara.csv')
codigo_sm <- read_csv2('data/metadata/codigos_zim_sm.csv')
codigo_tur <- read_csv2('data/metadata/codigos_zim_turgor.csv')

# DATOS HUMEDAD DE SUELO ####

data_codigo_sm <- left_join(codigo_sm,metadata,by=c('sensor' = 'identificador'))

device_id_sm <- data_codigo_sm |> 
  filter(grepl('Soil Moisture',tipo)) |> 
  pull(device_id)

data_old <- read_rds('data/data_processed/zim_sm.rds')
from_date <- data_old$fecha |> max() |> substr(1,16)

dest <- paste0('data/data_raw/zim/zim_sm_update',
               substr(gsub("[^0-9]", "",as.character(lubridate::now())),1,8),'.csv')

read_yara(device_id_sm,from_date = from_date,
          until_date = as.character(lubridate::now()+days(1)),
          dest_file = dest)

data_new <- read_delim(dest,delim = '\t') |> 
  select(1,contains('Soil Moisture')) |> 
  pivot_longer(-1,names_to = 'sensor') |> 
  rename_with(.fn = \(x) 'datetime',.cols = starts_with('Times')) |> 
  mutate(sensor =str_extract(str_extract(sensor,'[0-9]{4}\\['),'[0-9]{4}'),
         datetime = as_datetime(datetime,tz = 'America/Santiago')) |> 
  group_by(sensor,fecha = floor_date(datetime,'1 hour')) |> 
  summarize(value = mean(value,na.rm = T)) |> 
  mutate(sensor = as.numeric(sensor)) |> 
  left_join(codigo_sm,by = 'sensor') |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |>
  rename(sm = value) |>
  mutate(temporada = '2023-2024',
         unidad = factor(unidad, levels = 1:3),
         hora = as.numeric(format(as.POSIXct(fecha), format = "%H")),
         fecha = format(as.POSIXct(fecha), format = "%Y-%m-%d")) |>
  select(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,sensor,sm) |>
  arrange(fecha, sitio, hora, tratamiento, unidad) |>
  ungroup()

data_sm <- data_old |>
  bind_rows(data_new) |>
  distinct(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,sensor,.keep_all = T) |>
  arrange(fecha, sitio, hora, tratamiento, unidad) 

write_rds(data_sm, 'data/data_processed/zim_sm.rds')

# data_sm |> 
#   filter(temporada == '2023-2024') |>
#   ggplot(aes(fecha, sm, color = codigo)) +
#   geom_line(linewidth = 0.5) +
#   facet_grid(tratamiento ~ sitio) +
#   theme_light() +
#   guides(color = guide_legend(override.aes = list(size = 2)))

# DATOS TURGOR ####

data_codigo_tur <- left_join(codigo_tur,metadata,by=c('sensor' = 'identificador'))

device_id_tur <- data_codigo_tur |> 
  filter(grepl('Yara Water-Sensor',tipo)) |> 
  pull(device_id)

data_old <- read_rds('data/data_processed/zim_turgor.rds')
from_date <- data_old$fecha |> max() |> substr(1,16)

dest <- paste0('data/data_raw/zim/zim_turgor_update',
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
  group_by(sensor,fecha = floor_date(datetime,'1 hour')) |> 
  summarize(value = mean(value,na.rm = T)) |> 
  mutate(sensor = as.numeric(sensor)) |> 
  left_join(codigo_tur,by = 'sensor') |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |> 
  rename(turgor = value) |>
  mutate(temporada = '2023-2024',
         unidad = factor(unidad, levels = 1:3),
         hora = as.numeric(format(as.POSIXct(fecha), format = "%H")),
         zim = substr(codigo,nchar(codigo)-1,nchar(codigo)),
         codigo = substr(codigo,1,nchar(codigo)-2),
         fecha = format(as.POSIXct(fecha), format = "%Y-%m-%d")) |>
  select(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,zim,sensor,turgor) |>
  arrange(fecha,sitio,hora,tratamiento,unidad,zim) |>
  ungroup()

data_turgor <- data_old |>
  bind_rows(data_new) |>
  distinct(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,zim,sensor,.keep_all = T) |>
  arrange(fecha,sitio,hora,tratamiento,unidad,zim)

write_rds(data_turgor,'data/data_processed/zim_turgor.rds')

# data_turgor |>
#   filter(temporada == '2023-2024',
#          sitio == 'rio_claro') |>
#   drop_na() |>
#   ggplot(aes(fecha,turgor,color=unidad)) +
#   geom_point(size = .1) +
#   facet_grid(unidad~tratamiento) +
#   theme_light()

# data_turgor |>
#  group_by(sitio,codigo,tratamiento,hora) |>
#  summarize(presion = mean(turgor,na.rm = TRUE)) |>
#  ggplot(aes(hora,presion,color=codigo)) +
#  geom_point(size=.1) +
#  geom_line(size=.1) +
#  facet_grid(tratamiento~.)

