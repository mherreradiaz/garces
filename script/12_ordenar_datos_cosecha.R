
library(fs)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(dplyr)
library(xlsx)

# daño

data_defecto <- read.xlsx('data/data_raw/cosecha/calidad.xlsx', sheetName = 'defectos')
data_pedicelo <- read.xlsx('data/data_raw/cosecha/calidad.xlsx', sheetName = 'pedicelo')

data_defecto <- data_defecto |> 
  select(sitio,fecha,codigo,cantidad,dobles,daño.total,severo,parcial,ninguno) |>
  rename(d_total = daño.total,d_severo = severo,d_parcial = parcial,d_nulo = ninguno) |>
  arrange(fecha,sitio,codigo)

data_pedicelo <- data_pedicelo |>
  select(sitio,everything()) |>
  rename(d_p_nulo = nada, d_p_parcial = parcial, d_p_total = daño.total) |>
  arrange(fecha,sitio,codigo)

cols <- setdiff(names(data_pedicelo), names(data_defecto))

data_daño <- data_defecto |> bind_cols(select(data_pedicelo, all_of(cols)))

write_rds(data_daño, 'data/data_processed/cosecha/daño.rds')

# apariencia

data_apariencia <- read.xlsx('data/data_raw/cosecha/calidad.xlsx', sheetName = 'apariencia') |>
  arrange(fecha,sitio,codigo, muestra) |>
  rename(peso = peso_gr, diametro = diametro_mm)

write_rds(data_apariencia, 'data/data_processed/cosecha/apariencia.rds')

# brix

data_brix <- read.xlsx('data/data_raw/cosecha/calidad.xlsx', sheetName = 'brix') |>
  arrange(fecha,sitio,codigo, muestra)

write_rds(data_brix, 'data/data_processed/cosecha/brix.rds')

# produccion

data_prod <- read.xlsx('data/data_raw/cosecha/produccion.xlsx', sheetIndex = 1) |>
  arrange(fecha,sitio,codigo)

write_rds(data_prod, 'data/data_processed/cosecha/produccion.rds')

