library(fs)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(dplyr)
library(xlsx)

data_apariencia <- read.xlsx('data/data_raw/produccion/produccion.xlsx', sheetName = 'apariencia')
data_brix <- read.xlsx('data/data_raw/produccion/produccion.xlsx', sheetName = 'brix')

# defectos

data_defecto <- read.xlsx('data/data_raw/produccion/produccion.xlsx', sheetName = 'defectos')

data_defecto <- data_defecto |> 
  select(sitio,fecha,codigo,cantidad,total,severo.1,parcial.1,ninguno.1) |>
  rename(daño_total = total,daño_severo = severo.1,daño_parcial = parcial.1,daño_nulo = ninguno.1) |>
  arrange(fecha,sitio,codigo)

write_rds(data_defecto, 'data/data_processed/produccion/defectos.rds')

# pedicelo

data_pedicelo <- read.xlsx('data/data_raw/produccion/produccion.xlsx', sheetName = 'pedicelo')

data_pedicelo <- data_pedicelo |>
  select(sitio,everything()) |>
  rename(daño_nulo = nada, daño_parcial = parcial, daño_total = daño.total) |>
  arrange(fecha,sitio,codigo)

write_rds(data_pedicelo, 'data/data_processed/produccion/pedicelos.rds')


