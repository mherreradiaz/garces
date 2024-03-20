
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
redondear <- function(valor) {
  parte_decimal <- valor - floor(valor)
  
  if (parte_decimal < 0.5) {
    return(floor(valor))
  } else {
    return(ceiling(valor))
  }
}

# ULTIMA SEMANA ####

data_sm <- read_rds('data/data_processed/zim_sm.rds') |>
  filter(temporada == '2023-2024') |>
  filter(fecha >= substr(now()-days(7),1,10)) |>
  mutate(cluster = NA)

for (x in 1:length(unique(data_sm$sensor))) {
  
  data_sensor <- data_sm |>
    filter(sensor == unique(data_sm$sensor)[x]) |>
    spread(key = hora, value = sm, sep = "_") |>
    filter(complete.cases(across(hora_0:hora_23)))
  
  if (nrow(data_sensor) == 0) {next}
  
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
    kmeans(centers = 2) |>
    pluck('cluster')
  
  data_sensor <- data_sensor |>
    mutate(cluster = clusters) |>
    select(fecha,sensor,cluster)
  
  data_sm <- data_sm |>
    left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
    mutate(cluster = coalesce(cluster,cluster.x)) |>
    select(-cluster.x)
  
}

data_sm <- data_sm |>
  mutate(cluster = as.factor(cluster)) |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  na.omit()

codigos <- data_sm |>
  select(sitio,tratamiento,unidad, sensor) |>
  distinct() |>
  arrange(sitio,tratamiento,unidad)

for (x in 1:nrow(codigos)) {
  
  sensores <- codigos$sensor[x]
  
  plot_name <- paste(str_to_title(str_replace_all(codigos$sitio[x], "_", " ")),
                     codigos$tratamiento[x],
                     'Unidad', codigos$unidad[x])
  
  data_sensor <- data_sm |>
    filter(sensor == sensores) |>
    na.omit()
  
  p1 <- data_sensor |>
    ggplot(aes(fecha_hora, sm, color = as.factor(cluster))) +
    geom_point(size = .7) +
    labs(title = paste('Sensor', sensores),
         x = "Fecha", y = "sm",
         color = 'Cluster') +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
    scale_x_datetime(limits = c(min(as.POSIXct(data_sm$fecha)), max(as.POSIXct(data_sm$fecha))))
  
  p2 <- data_sensor |>
    group_by(sensor, cluster) |>
    drop_na() |>
    mutate(sm_sc = scale(sm)) |>
    group_by(sitio, tratamiento, cluster, hora) |>
    summarize(sm_hora = mean(sm_sc, na.rm = TRUE)) |>
    ggplot(aes(hora, sm_hora, color = as.factor(cluster))) +
    geom_point(size = 1) +
    geom_line(linewidth = .7) +
    labs(title = 'Ciclo horario del día',
         x = 'Hora',
         y = 'sm estandarizado',
         color = 'Cluster') +
    theme_bw()
  
  p1 <- p1 + theme(plot.margin = margin(10, 10, 10, 10))
  p2 <- p2 + theme(plot.margin = margin(10, 10, 10, 10))
  
  combined_plot <- plot_grid(p1, p2, ncol = 1, rel_widths = c(1, 1))
    
  ggsave(paste0('reporte/analisis_sm/plot_sm_ultima_semana/', str_replace_all(plot_name, "\\s", "_"), '.png'),
         plot = combined_plot, width = 14, height = 7, units = "in", dpi = 300)
}
