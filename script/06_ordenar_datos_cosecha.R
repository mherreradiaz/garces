
library(fs)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(dplyr)
library(xlsx)

# daño

data_defecto <- read.xlsx('data/raw/cosecha/calidad.xlsx', sheetName = 'defectos')
data_pedicelo <- read.xlsx('data/raw/cosecha/calidad.xlsx', sheetName = 'pedicelo')
codigos <- read_csv2('data/metadata/codigos_sp.csv')

data_defecto <- data_defecto |> 
  mutate(temporada = ifelse(fecha > as.Date('2023-06-01'),'2023-2024','2022-2023'),
         tratamiento = substr(codigo,1,2),
         codigo = stringr::str_replace(codigo, "H0", "H")) |>
  left_join(codigos, by = c('sitio','codigo','temporada','tratamiento')) |>
  mutate(unidad = factor(unidad,levels = 1:3),
         codigo = substr(codigo,3,nchar(codigo))) |>
  rename(n = cantidad,d_total = daño.total,d_severo = severo,d_parcial = parcial,d_nulo = ninguno) |>
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,n,dobles,d_total,d_severo,d_parcial,d_nulo) |>
  arrange(fecha,sitio,tratamiento,unidad)

data_pedicelo <- data_pedicelo |>
  mutate(temporada = ifelse(fecha > as.Date('2023-06-01'),'2023-2024','2022-2023'),
         tratamiento = substr(codigo,1,2),
         codigo = stringr::str_replace(codigo, "H0", "H")) |>
  left_join(codigos, by = c('sitio','codigo','temporada','tratamiento')) |>
  mutate(unidad = factor(unidad,levels = 1:3),
         codigo = substr(codigo,3,nchar(codigo))) |>
  rename(d_p_nulo = nada, d_p_parcial = parcial, d_p_total = daño.total) |>
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,d_p_total,d_p_parcial,d_p_nulo) |>
  arrange(fecha,sitio,tratamiento,unidad)

cols <- setdiff(names(data_pedicelo), names(data_defecto))

data_daño <- data_defecto |> bind_cols(select(data_pedicelo, all_of(cols))) |>
  mutate(n_p = 20,
         .before = d_p_total)

write_rds(data_daño, 'data/processed/cosecha/daño.rds')

# apariencia

data_apariencia <- read.xlsx('data/raw/cosecha/calidad.xlsx', sheetName = 'apariencia') |>
  mutate(tratamiento = substr(codigo,1,2)) |>
  left_join(codigos, by = c('sitio','codigo','temporada','tratamiento')) |>
  mutate(unidad = factor(unidad,levels = 1:3),
         codigo = substr(codigo,3,nchar(codigo))) |>
  rename(peso = peso_gr, diametro = diametro_mm) |>
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |>
  arrange(fecha,sitio,tratamiento,unidad, muestra)

write_rds(data_apariencia, 'data/processed/cosecha/apariencia.rds')

# brix

data_brix <- read.xlsx('data/raw/cosecha/calidad.xlsx', sheetName = 'brix') |>
  mutate(tratamiento = substr(codigo,1,2)) |>
  left_join(codigos, by = c('sitio','codigo','temporada','tratamiento')) |>
  mutate(unidad = factor(unidad,levels = 1:3),
         codigo = substr(codigo,3,nchar(codigo))) |>
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |>
  arrange(fecha,sitio,tratamiento,unidad, muestra)

write_rds(data_brix, 'data/processed/cosecha/brix.rds')

# produccion

# read.xlsx('data/raw/cosecha/produccion.xlsx', sheetIndex = 1) |>
#   mutate(kg = as.numeric(kg)) |>
#   group_by(codigo) |>
#   summarise(peso = mean(kg,na.rm=T)) |>
#   ungroup() |>
#   write.xlsx('data/raw/cosecha/produccion.xlsx')

data_prod <- read.xlsx('data/raw/cosecha/produccion.xlsx', sheetIndex = 1) |>
  mutate(tratamiento = substr(codigo,1,2)) |>
  left_join(codigos, by = c('sitio','codigo','temporada','tratamiento')) |>
  mutate(unidad = factor(unidad,levels = 1:3),
         codigo = substr(codigo,3,nchar(codigo))) |>
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |>
  arrange(fecha,sitio,tratamiento,unidad)

write_rds(data_prod, 'data/processed/cosecha/produccion.rds')

