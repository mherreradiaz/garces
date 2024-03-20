library(dplyr)
library(readr)
library(readxl)
library(tibble)

data_potencial <- read_xlsx('data/data_raw/potencial/ciclo_diario/potencial_horario.xlsx') |>
  mutate(unidad = factor(unidad,levels = 1:3),
         across(bar_8:bar_20,as.numeric),
         fecha = as.Date(fecha))

write_rds(data_potencial, 'data/data_processed/potencial_horario.rds')
