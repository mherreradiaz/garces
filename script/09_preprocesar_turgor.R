library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(dbscan)
library(purrr)
library(grid)
library(cowplot)
library(ggpubr)
library(scales)
redondear <- function(valor) {
  parte_decimal <- valor - floor(valor)
  
  if (parte_decimal < 0.5) {
    return(floor(valor))
  } else {
    return(ceiling(valor))
  }
}

# CLUSTERING

data_turgor <- read_rds('data/data_processed/zim_turgor.rds') |>
  mutate(cluster = NA)

codigos <- data_turgor |>
  select(temporada,sitio,tratamiento,unidad,zim, sensor) |>
  distinct() |>
  arrange(temporada,sitio,tratamiento,unidad,zim)

for (x in 1:nrow(codigos)) {
  
  data_sensor <- data_turgor |>
    filter(sensor == codigos$sensor[x],
           temporada == codigos$temporada[x]) |>
    spread(key = hora, value = turgor, sep = "_") |>
    filter(complete.cases(across(hora_0:hora_23)))
  
  if (nrow(data_sensor) == 0) {next}
  
  c <- redondear(0.7992507 + 0.03522533*nrow(data_sensor) + 0.0002579095 * nrow(data_sensor)^2)
  c <- ifelse(c>7,7,c)
  c <- ifelse(c == 1,2,c)
  
  clusters <- data_sensor |>
    rowwise() |>
    mutate(amplitud = max(c_across(starts_with("hora_")), na.rm = T) -
             min(c_across(starts_with("hora_")), na.rm = T),
           media = mean(c_across(starts_with('hora_')), na.rm=T)) |>
    select(hora_0:hora_23) |>
    prcomp(scale. = T) |> 
    pluck('x') |>
    as.data.frame() |>
    select(PC1,PC2) |>
    kmeans(centers = c) |>
    pluck('cluster')
    
  data_sensor <- data_sensor |>
    mutate(cluster = clusters) |>
    select(fecha,temporada,sensor,cluster)
  
  data_turgor <- data_turgor |>
    left_join(data_sensor, by = c('temporada','fecha','sensor'), suffix = c('','.x')) |>
    mutate(cluster = coalesce(cluster,cluster.x)) |>
    select(-cluster.x)
  
}

data_turgor <- data_turgor |>
  mutate(cluster = as.factor(cluster))

write_rds(data_turgor,'analisis/01_data_turgor_clusterizado.rds')

# LIMPIEZA

data_clima <- read_rds('data/data_processed/clima.rds') |>
  select(sitio,temporada,fecha,hora,t_media,vpd_media,eto) |>
  group_by(temporada,sitio) |>
  mutate(t_media = scale(t_media),
         vpd_media = scale(vpd_media),
         eto = scale(eto)) |>
  ungroup()

data_turgor <- read_rds('analisis/01_data_turgor_clusterizado.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"),
         .before = tratamiento)

data_cor <- data_turgor |>
  left_join(data_clima,by=c('sitio','temporada','fecha','hora'))

data_cor$vpd_trans <- scale(log(data_cor$vpd_media + 1))

cor_summary <- data_cor |>
  group_by(temporada,sensor,cluster) |>
  drop_na() |>
  mutate(turgor_sc = scale(turgor)) |>
  ungroup() |>
  group_by(temporada,sensor,cluster) |>
  summarise(t_cor = cor(turgor_sc,t_media, use = 'complete.obs'),
            vpd_cor = cor(turgor_sc,vpd_trans, use = 'complete.obs')) |>
  ungroup() |>
  mutate(cor = (t_cor+vpd_cor)/2) |>
  filter(cor > .5) |>
  select(-t_cor,-vpd_cor,-cor) |>
  mutate(filtro = 1)

data_turgor <- data_turgor |>
  left_join(cor_summary, by = c('temporada','sensor','cluster')) |>
  filter(filtro == 1) |>
  select(-filtro,-fecha_hora)

write_rds(data_turgor,'analisis/02_data_turgor_limpiado.rds')

# NORMALIZAR Y JUNTAR

data_turgor <- read_rds('analisis/02_data_turgor_limpiado.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_merge <- data_turgor |>
  group_by(temporada,sensor,cluster) |>
  drop_na() |>
  mutate(turgor_sc = scale(turgor)) |>
  ungroup()

data_filter <- data_merge |>
  group_by(temporada,sensor) |>
  summarise(lim_sup = quantile(turgor_sc, 0.75) + 1.5 * IQR(turgor_sc),
            lim_inf = quantile(turgor_sc, 0.25) - 1.5 * IQR(turgor_sc))

data_merge <- data_merge |>
  left_join(data_filter, by = c('temporada','sensor')) |>
  mutate(turgor_sc = ifelse(between(turgor_sc,lim_inf,lim_sup),turgor_sc,NA)) |>
  filter(!is.na(turgor_sc)) |>
  select(-lim_inf,-lim_sup)

write_rds(data_merge,'analisis/03_data_turgor_sensor_prepro.rds')

# UNIFICAR TURGOR POR UNIDAD

data_turgor <- read_rds('analisis/03_data_turgor_sensor_prepro.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_unidad <- data_turgor |>
  group_by(sitio,temporada,fecha,hora,fecha_hora,tratamiento,unidad) |>
  summarise(turgor_sc = mean(turgor_sc,na.rm=T)) |>
  arrange(temporada,sitio,tratamiento,unidad,fecha_hora) |>
  ungroup() |>
  select(-fecha_hora) |>
  write_rds('analisis/04_data_turgor_unidad_prepro.rds')

# UNIFICAR TURGOR POR TRATAMIENTO

data_turgor <- read_rds('analisis/04_data_turgor_unidad_prepro.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_trat <- data_turgor |>
  group_by(sitio,temporada,fecha,hora,fecha_hora,tratamiento) |>
  summarise(turgor_sc = mean(turgor_sc,na.rm=T)) |>
  arrange(temporada,sitio,tratamiento,fecha_hora) |>
  ungroup()  |>
  select(-fecha_hora) |>
  write_rds('analisis/05_data_turgor_tratamiento_prepro.rds')
  
