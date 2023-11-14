
library(tidyverse)
library(highcharter)
library(gt)
library(readr)

data_fluo <- read_rds('data/data_processed/rds/data_fluorescencia.rds')
data_potencial <- read_rds('data/data_processed/rds/data_potencial.rds')
data_lai <- read_rds('data/data_processed/rds/data_ceptometro.rds')
data_sm <- read_rds('data/data_processed/rds/data_zim_sm.rds')
data_turgor <- read_rds('data/data_processed/rds/data_zim_turgor.rds')

codigos <- read.csv('data/metadata/codigos_arboles.csv', sep = ';')

data_info <- codigos |>
  select(codigo,unidad)

data_fluo |> # BOXPLOT FLUORESCENCIA
  left_join(data_info, by = 'codigo') |>
  mutate(unidad = as.factor(unidad)) |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |>
  ggplot(aes(unidad,Fv/Fm)) +
  geom_boxplot() +
  facet_grid(sitio~tratamiento) +
  geom_text(aes(unidad,1,label = codigo),size=3) +
  theme_light()

data_fluo |> # SERIE DE TIEMPO FLUORESCENCIA
  left_join(data_info, by = 'codigo') |>
  mutate(unidad = as.factor(unidad)) |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |>
  ggplot(aes(fecha,Fv/Fm,color=unidad)) +
  geom_point(size=1.2) +
  geom_line(linewidth = .7) +
  facet_grid(tratamiento~sitio) +
  theme_bw()

data_potencial |> # BOXPLOT POTENCIAL
  mutate(unidad = as.factor(unidad)) |> 
  mutate(MPa = -potencial_bar/10) |>
  separate(codigo,2,into =c('tratamiento','codigo')) |>
  ggplot(aes(unidad,MPa)) +
  geom_boxplot() +
  facet_grid(sitio~tratamiento) +
  geom_text(aes(unidad,-.4,label = codigo),size=3) +
  theme_light()

data_potencial |> # SERIE DE TIEMPO POTENCIAL
  mutate(unidad = as.factor(unidad)) |> 
  mutate(MPa = -potencial_bar/10) |>
  separate(codigo,2,into =c('tratamiento','codigo')) |>
  ggplot(aes(fecha,MPa,color=unidad)) +
  geom_point(size=1.2) +
  geom_line(linewidth = .7) +
  facet_grid(tratamiento~sitio) +
  theme_bw()

data_lai |> # BOXPLOT LAI
  mutate(tratamiento = as.factor(tratamiento)) |>
  ggplot(aes(tratamiento,lai)) +
  geom_boxplot() +
  facet_grid(~sitio) +
  geom_text(aes(tratamiento,4.5,label = tratamiento),size=3) +
  theme_light()

data_lai |> # SERIE DE TIEMPO LAI
  mutate(tratamiento = as.factor(tratamiento)) |>
  ggplot(aes(fecha,lai,color=tratamiento)) +
  geom_point(size=1.2) +
  geom_line(linewidth = .7) +
  facet_grid(~sitio) +
  theme_bw()

