
library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
options(timeout = max(3600, getOption("timeout")))

# download yara data

source('script/funciones/read_yara.R')

metadata <- read_csv2('data/metadata/metadata_yara.csv')
codigo_sm <- read_csv2('data/metadata/codigos_zim_sm.csv')
codigo_tur <- read_csv2('data/metadata/codigos_zim_turgor.csv')

# datos yara humedad de suelo
data_codigo_sm <- left_join(codigo_sm,metadata,by=c('sensor' = 'identificador'))

device_id_sm <- data_codigo_sm |> 
  filter(grepl('Soil Moisture',tipo)) |> 
  pull(device_id)

data_old <- read_rds('data/data_processed/rds/data_zim_sm.rds')
from_date <- data_old$fecha |> last() |> substr(1,16)

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
  summarize(value = mean(value,na.rm = TRUE)) |> 
  mutate(sensor = as.numeric(sensor)) |> 
  left_join(codigo_sm,by = 'sensor') |> 
  separate(codigo,2,into =c('tratamiento','codigo')) 

data_sm <- data_old |>
  bind_rows(data_new) |>
  distinct(sensor,fecha,sitio,codigo,unidad,.keep_all = TRUE) |>
  arrange(sensor,fecha,sitio)

write_rds(data_sm, 'data/data_processed/rds/data_zim_sm.rds')

#data_sm |> 
#  ggplot(aes(fecha, value, color = as.factor(codigo))) +
#  geom_point(size = 0.1) +
#  facet_grid(tratamiento ~ sitio) +
#  theme_light() +
#  guides(color = guide_legend(override.aes = list(size = 2)))

# datos zim turgor
data_codigo_tur <- left_join(codigo_tur,metadata,by=c('sensor' = 'identificador'))

device_id_tur <- data_codigo_tur |> 
  filter(grepl('Yara Water-Sensor',tipo)) |> 
  pull(device_id)

data_old <- read_rds('data/data_processed/rds/data_zim_turgor.rds')
from_date <- data_old$fecha |> last() |> substr(1,16)

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
  summarize(value = mean(value,na.rm = TRUE)) |> 
  mutate(sensor = as.numeric(sensor)) |> 
  left_join(codigo_tur,by = 'sensor') |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |> 
  mutate(zim = substr(codigo,6,7))

data_turgor <- data_old |>
  bind_rows(data_new) |>
  distinct(sensor,fecha,sitio,codigo,unidad,zim,.keep_all = TRUE) |>
  arrange(sensor,fecha,sitio)

write_rds(data_turgor,'data/data_processed/rds/data_zim_turgor.rds')

#data_turgor |> 
#  drop_na() |> 
#  ggplot(aes(fecha,value,color=unidad)) +
#  geom_point(size=.1) +
#  facet_grid(unidad~tratamiento) +
#  theme_light()

#data_turgor |> 
#  mutate(hora_dia = as.numeric(format(fecha,"%H"))) |> 
#  group_by(sitio,codigo,tratamiento,hora_dia) |>
#  summarize(presion = mean(value,na.rm = TRUE)) |> 
#  ggplot(aes(hora_dia,presion,color=codigo)) +
#  geom_point(size=.1) +
#  geom_line(size=.1) +
#  facet_grid(tratamiento~.)



