
library(fs)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(dplyr)
library(xlsx)

# fluorescencia

data_fluo_new <- read_rds('data/processed/rds/fluorescencia.rds')
data_fluo_old <- read_rds('data/processed_old/rds/fluorescencia.rds')

data_fluo <- rbind(data_fluo_old,data_fluo_new) |>
  arrange(fecha,codigo)

write_rds(data_fluo,'data/processed/fluorescencia.rds')

# ceptometro

data_cepto_new <- read_rds('data/processed_new/rds/ceptometro.rds')
data_cepto_old <- read_delim('data/processed_old/csv/ceptometro.csv') |>
  rename(fecha = data, sitio = field, tratamiento = treatment, latitud = latitude, longitud = length) |>
  mutate(fecha = as.Date(fecha, format = "%d %m %Y"),
         latitud = -latitud,
         longitud = -longitud) |>
  select(sitio, everything())

data_cepto <- rbind(data_cepto_old,data_cepto_new) |>
  arrange(fecha,tratamiento) 

write_rds(data_cepto,'data/processed/ceptometro.rds')

# potencial

data_potencial_new <- read_rds('data/processed_new/rds/potencial.rds')
data_potencial_old <- read_delim('data/processed_old/csv/potencial.csv') |>
  select(sitio, fecha, codigo, potencial_bar, unidad)

data_potencial <- rbind(data_potencial_old,data_potencial_new) |>
  arrange(fecha,codigo) 

write_rds(data_potencial,'data/processed/potencial.rds')

# sm

data_sm_new <- read_rds('data/processed_new/rds/zim_sm.rds')
data_sm_old <- read_rds('data/processed_old/rds/zim_sm.rds') |>
  rename(fecha = hora,
         sm = value) |>
  mutate(temporada = '2022-2023',
         unidad = as.factor(unidad),
         hora = as.numeric(format(as.POSIXct(fecha), format = "%H"))) |>
  mutate(fecha = format(as.POSIXct(fecha), format = "%Y-%m-%d")) |>
  select(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,sensor,sm) |>
  arrange(fecha, sitio, hora, tratamiento, unidad)

data_sm <- rbind(data_sm_old,data_sm_new) |>
  arrange(sensor,fecha,sitio)

write_rds(data_sm,'data/processed/zim_sm.rds')

# turgor

data_turgor_new <- read_rds('data/processed_new/rds/zim_turgor.rds')
data_turgor_old <- read_rds('data/processed_old/rds/zim_turgor.rds') |>
  rename(fecha = hora,
         turgor = value) |>
  mutate(temporada = '2022-2023',
         unidad = as.factor(unidad),
         hora = as.numeric(format(as.POSIXct(fecha), format = "%H")),
         codigo = substr(codigo,1,nchar(codigo)-2)) |>
  mutate(fecha = format(as.POSIXct(fecha), format = "%Y-%m-%d")) |>
  select(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,zim,sensor,turgor) |>
  arrange(fecha,sitio,hora,tratamiento,unidad,zim)

data_turgor <- rbind(data_turgor_old,data_turgor_new) |>
  arrange(sensor,fecha,sitio)

write_rds(data_turgor,'data/processed/zim_turgor.rds')

