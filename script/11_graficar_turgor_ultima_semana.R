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

data_turgor <- read_rds('data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2023-2024') |>
  filter(fecha >= substr(now()-days(7),1,10)) |>
  mutate(cluster = NA)

for (x in 1:length(unique(data_turgor$sensor))) {
  
  data_sensor <- data_turgor |>
    filter(sensor == unique(data_turgor$sensor)[x]) |>
    spread(key = hora, value = turgor, sep = "_") |>
    filter(complete.cases(across(hora_0:hora_23)))
  
  if (nrow(data_sensor) < 2) {next}
  
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
  
  data_turgor <- data_turgor |>
    left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
    mutate(cluster = coalesce(cluster,cluster.x)) |>
    select(-cluster.x)
  
}

data_turgor <- data_turgor |>
  mutate(cluster = as.factor(ifelse(is.na(cluster),'no_cluster',cluster)),
         fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  na.omit()

codigos <- data_turgor |>
  select(sitio,tratamiento,unidad,zim, sensor) |>
  distinct() |>
  arrange(sitio,tratamiento,unidad,zim) |>
  spread(key = 'zim', value = 'sensor')

for (x in 1:nrow(codigos)) {
  
  sensores <- codigos[x,] |>
    select(Z1, Z2) |>
    as.numeric() |>
    na.omit() |>
    as.numeric()
  
  plot_name <- paste(str_to_title(str_replace_all(codigos[x,]$sitio, "_", " ")),
                     codigos[x,]$tratamiento,
                     'Unidad', codigos[x,]$unidad)
  
  combined_plot_final <- NULL
  
  for (s in 1:length(sensores)) {
    
    data_sensor <- data_turgor |>
      filter(sensor == sensores[s]) |>
      na.omit()
    
    p1 <- data_sensor |>
      ggplot(aes(fecha_hora, turgor, color = as.factor(cluster), group = turgor)) +
      geom_point(size = 1) +
      labs(title = paste('Sensor', sensores[s]),
           x = "Fecha", y = "Presión de parche (bar)",
           color = 'Cluster') +
      theme_bw() +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
      scale_x_datetime(limits = c(min(as.POSIXct(data_turgor$fecha)), max(as.POSIXct(data_turgor$fecha))))
    
    p3 <- data_sensor |>
      group_by(sensor, cluster) |>
      drop_na() |>
      mutate(turgor_sc = scale(turgor)) |>
      group_by(sitio, tratamiento, cluster, hora) |>
      summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |>
      ggplot(aes(hora, turgor_hora, color = as.factor(cluster))) +
      geom_point(size = 1) +
      geom_line(linewidth = .7) +
      labs(title = 'Ciclo horario del día',
           x = 'Hora',
           y = 'Presión de parche (estand.)',
           color = 'Cluster') +
      facet_wrap(~cluster, scales = 'free_y', ncol = 1, strip.position = 'right') +
      theme_bw()
    
    p1 <- p1 + theme(plot.margin = margin(10, 10, 10, 10))
    p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
    
    combined_plot <- plot_grid(p1, p3, ncol = 1, rel_widths = c(1, 1))
    
    if (is.null(combined_plot_final)) {
      combined_plot_final <- combined_plot
    } else {
      combined_plot_final <- plot_grid(combined_plot_final, combined_plot, ncol = 2, rel_widths = c(1, 1))
    }
  }
  ggsave(paste0('reporte/plots/00_turgor_ultima_semana/', str_replace_all(plot_name, "\\s", "_"), '.png'),
         plot = combined_plot_final, width = 14, height = 7, units = "in", dpi = 300)
}