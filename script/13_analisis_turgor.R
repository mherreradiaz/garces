
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
library(stringr)
library(cowplot)

# HORA MIN, MAX Y AMPLITUD ####

data_2023 <- read_rds('data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2023-2024') |>
  group_by(sitio,fecha,tratamiento,unidad,codigo,zim, sensor) |>
  summarise(min = min(turgor),
            max = max(turgor),
            amplitud = max(turgor) - min(turgor),
            hora_min = ifelse(!is.na(amplitud),hora[which.min(turgor)],NA),
            hora_max = ifelse(!is.na(amplitud),hora[which.max(turgor)],NA)) |>
  ungroup() |>
  mutate(fecha = as.Date(fecha))

#data_turgor |> write_rds('data/misc/analisis_turgor.rds')

data_2023 |>
  group_by(sitio,tratamiento,codigo,zim) |> 
  ggplot(aes(unidad,hora_min,color=zim)) +
  geom_boxplot() +
  labs(title = "Hora de mínimo turgor diario (Temporada 2023-2024)",
       x = "Unidad",
       y = "Hora") +
  facet_grid(sitio~tratamiento) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2)))

data_2023 |>
  group_by(sitio,tratamiento,codigo,zim) |> 
  ggplot(aes(unidad,hora_max,color=zim)) +
  geom_boxplot() +
  labs(title = "Hora de máximo turgor diario (Temporada 2023-2024)",
       x = "Unidad",
       y = "Hora") +
  facet_grid(sitio~tratamiento) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2)))

data_2022 <- read_rds('data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2022-2023') |>
  group_by(sitio,fecha,tratamiento,unidad,codigo,zim, sensor) |>
  summarise(min = min(turgor),
            max = max(turgor),
            amplitud = max(turgor) - min(turgor),
            hora_min = ifelse(!is.na(amplitud),hora[which.min(turgor)],NA),
            hora_max = ifelse(!is.na(amplitud),hora[which.max(turgor)],NA)) |>
  ungroup() |>
  mutate(fecha = as.Date(fecha))

data_2022 |>
  group_by(sitio,tratamiento,codigo,zim) |> 
  ggplot(aes(unidad,hora_min,color=zim)) +
  geom_boxplot() +
  labs(title = "Hora de mínimo turgor diario (Temporada 2022-2023)",
       x = "Unidad",
       y = "Hora") +
  facet_grid(sitio~tratamiento) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2)))

data_2022 |>
  group_by(sitio,tratamiento,codigo,zim) |> 
  ggplot(aes(unidad,hora_max,color=zim)) +
  geom_boxplot() +
  labs(title = "Hora de máximo turgor diario (Temporada 2022-2023)",
       x = "Unidad",
       y = "Hora") +
  facet_grid(sitio~tratamiento) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2)))


# CLUSTERING ####

data_turgor <- read_rds('data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2023-2024') |>
  mutate(fecha = as.Date(fecha)) |>
  mutate(cluster = NA)

data_dia <- data_turgor |>
  group_by(fecha,sensor) |>
  summarise(mean = mean(turgor),
            min = min(turgor),
            max = max(turgor),
            amplitud = max(turgor)-min(turgor),
            hora_min = ifelse(!is.na(amplitud),hora[which.min(turgor)],NA),
            hora_max = ifelse(!is.na(amplitud),hora[which.max(turgor)],NA)) |>
  ungroup() |>
  mutate(fecha = as.Date(fecha))

for (x in 1:length(unique(data_turgor$sensor))) {
  
  data_sensor <- data_dia |>
    filter(sensor == unique(data_turgor$sensor)[x]) |>
    na.omit()
  
  if (nrow(data_sensor) == 0) {next}
  
  clusters <- data_sensor |>
    select(amplitud,min, max) |>
    scale() |>
    dbscan(eps = 0.5, minPts = 5) |>
    pluck('cluster')
  
  data_sensor$cluster <- clusters

  data_sensor <- data_sensor |>
    select(fecha,sensor,cluster)

  data_turgor <- data_turgor |>
    left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
    mutate(cluster = coalesce(cluster,cluster.x)) |>
    select(-cluster.x)
}

write_rds(data_turgor ,'reporte/analisis_turgor/data_turgor_cluster.rds')

data_turgor |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  na.omit() |>
  ggplot(aes(fecha_hora,turgor,color = as.factor(cluster))) +
  geom_point(size = .7) +
  facet_grid(tratamiento+sitio~unidad) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2)))

# ANALISIS POR SENSOR ####

data_turgor <- read_rds('reporte/analisis_turgor/data_turgor_cluster.rds')

data_dia <- data_turgor |>
  group_by(fecha,sensor) |>
  summarise(mean = mean(turgor),
            min = min(turgor),
            max = max(turgor),
            amplitud = max(turgor)-min(turgor),
            hora_min = ifelse(!is.na(amplitud),hora[which.min(turgor)],NA),
            hora_max = ifelse(!is.na(amplitud),hora[which.max(turgor)],NA)) |>
  ungroup() |>
  mutate(fecha = as.Date(fecha))

data_turgor <- data_turgor |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  left_join(data_dia, by = c('fecha','sensor')) |>
  pivot_longer(cols = c(hora_min, hora_max),
               names_to = "variable",
               values_to = "hora_minmax") |>
  na.omit()

codigos <- data_turgor |>
  select(sitio,tratamiento,unidad,zim, sensor) |>
  distinct()

for (x in 1:23) {
  
  filtro <- codigos |>
    select(sitio, tratamiento, unidad) |>
    distinct() |>
    slice(x) |>
    mutate(filtro = 1)
  sensores <- codigos |> 
    left_join(filtro,by = c('sitio','tratamiento','unidad')) |>
    filter(filtro == 1) |>
    pull(sensor)
  plot_name <- paste(str_to_title(str_replace_all(filtro$sitio, "_", " ")),
                     filtro$tratamiento,
                     'Unidad',filtro$unidad)
  
  combined_plot_final <- NULL
  
  for (s in 1:length(sensores)) {
    
    data_sensor <- data_turgor |>
      filter(sensor == sensores[s]) |>
      na.omit()

    if (length(sensores) < 2) {
      
      p1 <- data_sensor |>
        ggplot(aes(fecha_hora,turgor,color = as.factor(cluster))) +
        geom_point(size = .7) +
        labs(title = paste0(plot_name,': Sensor ',unique(data_sensor$sensor)),
             x = "Fecha", y = "Turgor",
             color = 'Cluster') +
        theme_bw() +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        theme(plot.title = element_text(hjust = 0.5))
      
      p2 <- data_sensor |>
        ggplot(aes(x = as.factor(cluster), y = hora_minmax, color = as.factor(cluster))) +
        geom_boxplot(position = "dodge") +
        facet_wrap(~variable, scales = "free_y", ncol = 1, strip.position = 'right') +
        labs(title = 'Hora del mín y max por cluster',
             x = "Cluster", y = "Hora",
             color = 'Cluster') +
        theme_bw()
      
      p3 <- data_sensor |> 
        group_by(sensor, cluster) |>
        drop_na() |> 
        mutate(turgor_sc = scale(turgor)) |> 
        group_by(sitio, tratamiento, cluster, hora) |> 
        summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |> 
        ggplot(aes(hora, turgor_hora, color = as.factor(cluster))) +
        geom_point(size = 1) +
        geom_line(linewidth = .7) +
        labs(title = 'Ciclo horario del día por cluster',
             x = 'Hora',
             y = 'Turgor normalizado',
             color = 'Cluster') +
        facet_wrap(~cluster, scales = 'free_y', ncol = 1, strip.position = 'right') +
        theme_bw()
      
      p1 <- p1 + theme(plot.margin = margin(10, 10, 10, 10))
      p2 <- p2 + theme(plot.margin = margin(10, 10, 10, 10))
      p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
      
      combined_plot_final <- plot_grid(p1,
                                 plot_grid(p2, p3, ncol = 2, rel_widths = c(1, 1)),
                                 ncol = 1, rel_heights = c(1, 1))
      next
    } else {
      
      p1 <- data_sensor |>
        ggplot(aes(fecha_hora,turgor,color = as.factor(cluster))) +
        geom_point(size = .7) +
        labs(title = paste('Sensor',unique(data_sensor$sensor)),
             x = "Fecha", y = "Turgor",
             color = 'Cluster') +
        theme_bw() +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        theme(plot.title = element_text(hjust = 0.5))
      
      p2 <- data_sensor |>
        ggplot(aes(x = as.factor(cluster), y = hora_minmax, color = as.factor(cluster))) +
        geom_boxplot(position = "dodge") +
        facet_wrap(~variable, scales = "free_y", ncol = 1, strip.position = 'right') +
        labs(title = 'Hora del mín y max por cluster',
             x = "Cluster", y = "Hora",
             color = 'Cluster') +
        theme_bw()
      
      p3 <- data_sensor |> 
        group_by(sensor, cluster) |>
        drop_na() |> 
        mutate(turgor_sc = scale(turgor)) |> 
        group_by(sitio, tratamiento, cluster, hora) |> 
        summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |> 
        ggplot(aes(hora, turgor_hora, color = as.factor(cluster))) +
        geom_point(size = 1) +
        geom_line(linewidth = .7) +
        labs(title = 'Ciclo horario del día por cluster',
             x = 'Hora',
             y = 'Turgor normalizado',
             color = 'Cluster') +
        facet_wrap(~cluster, scales = 'free_y', ncol = 1, strip.position = 'right') +
        theme_bw()
      
      p1 <- p1 + theme(plot.margin = margin(10, 10, 10, 10))
      p2 <- p2 + theme(plot.margin = margin(10, 10, 10, 10))
      p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
      
      combined_plot <- plot_grid(p1,
                                 plot_grid(p2, p3, ncol = 2, rel_widths = c(1, 1)),
                                 ncol = 1, rel_heights = c(1, 1))
      
      if (is.null(combined_plot_final)) {
        combined_plot_final <- combined_plot
      } else {
        combined_plot_final <- plot_grid(combined_plot_final, combined_plot, ncol = 2, rel_widths = c(1, 1))
      }
    }
  }
  
  if (length(sensores) < 2) {
    ggsave(paste0('reporte/analisis_turgor/plot/',str_replace_all(plot_name, "\\s", "_"),'.png'), 
           plot = combined_plot_final, width = 14, height = 7, units = "in", dpi = 300)
  } else {
    combined_plot_final <- combined_plot_final +
      labs(title = plot_name) +
      theme(plot.title = element_text(hjust = 0, size = 14),
            plot.margin = margin(15, 10, 10, 10))
    
    ggsave(paste0('reporte/analisis_turgor/plot/',str_replace_all(plot_name, "\\s", "_"),'.png'), 
           plot = combined_plot_final,
           width = 14, height = 7, units = "in", dpi = 300)
    }
}

# ULTIMA SEMANA ####

data_turgor <- read_rds('reporte/analisis_turgor/data_turgor_cluster.rds') |>
  filter(fecha >= '2024-01-07')

data_dia <- data_turgor |>
  group_by(fecha,sensor) |>
  summarise(mean = mean(turgor),
            min = min(turgor),
            max = max(turgor),
            amplitud = max(turgor)-min(turgor),
            hora_min = ifelse(!is.na(amplitud),hora[which.min(turgor)],NA),
            hora_max = ifelse(!is.na(amplitud),hora[which.max(turgor)],NA)) |>
  ungroup() |>
  mutate(fecha = as.Date(fecha))

data_turgor <- data_turgor |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  left_join(data_dia, by = c('fecha','sensor')) |>
  pivot_longer(cols = c(hora_min, hora_max),
               names_to = "variable",
               values_to = "hora_minmax") |>
  na.omit()

codigos <- data_turgor |>
  select(sitio,tratamiento,unidad,zim, sensor) |>
  distinct()

for (x in 1:23) {
  
  filtro <- codigos |>
    select(sitio, tratamiento, unidad) |>
    distinct() |>
    slice(x) |>
    mutate(filtro = 1)
  sensores <- codigos |> 
    left_join(filtro,by = c('sitio','tratamiento','unidad')) |>
    filter(filtro == 1) |>
    pull(sensor)
  plot_name <- paste(str_to_title(str_replace_all(filtro$sitio, "_", " ")),
                     filtro$tratamiento,
                     'Unidad',filtro$unidad)
  
  combined_plot_final <- NULL
  
  for (s in 1:length(sensores)) {
    
    data_sensor <- data_turgor |>
      filter(sensor == sensores[s]) |>
      na.omit()
    
    if (length(sensores) < 2) {
      
      p1 <- data_sensor |>
        ggplot(aes(fecha_hora,turgor,color = as.factor(cluster))) +
        geom_point(size = .7) +
        labs(title = paste0(plot_name,': Sensor ',unique(data_sensor$sensor)),
             x = "Fecha", y = "Turgor",
             color = 'Cluster') +
        theme_bw() +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        theme(plot.title = element_text(hjust = 0.5))
      
      p2 <- data_sensor |>
        ggplot(aes(x = as.factor(cluster), y = hora_minmax, color = as.factor(cluster))) +
        geom_boxplot(position = "dodge") +
        facet_wrap(~variable, scales = "free_y", ncol = 1, strip.position = 'right') +
        labs(title = 'Hora del mín y max por cluster',
             x = "Cluster", y = "Hora",
             color = 'Cluster') +
        theme_bw()
      
      p3 <- data_sensor |> 
        group_by(sensor, cluster) |>
        drop_na() |> 
        mutate(turgor_sc = scale(turgor)) |> 
        group_by(sitio, tratamiento, cluster, hora) |> 
        summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |> 
        ggplot(aes(hora, turgor_hora, color = as.factor(cluster))) +
        geom_point(size = 1) +
        geom_line(linewidth = .7) +
        labs(title = 'Ciclo horario del día por cluster',
             x = 'Hora',
             y = 'Turgor normalizado',
             color = 'Cluster') +
        facet_wrap(~cluster, scales = 'free_y', ncol = 1, strip.position = 'right') +
        theme_bw()
      
      p1 <- p1 + theme(plot.margin = margin(10, 10, 10, 10))
      p2 <- p2 + theme(plot.margin = margin(10, 10, 10, 10))
      p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
      
      combined_plot_final <- plot_grid(p1,
                                       plot_grid(p2, p3, ncol = 2, rel_widths = c(1, 1)),
                                       ncol = 1, rel_heights = c(1, 1))
      next
    } else {
      
      p1 <- data_sensor |>
        ggplot(aes(fecha_hora,turgor,color = as.factor(cluster))) +
        geom_point(size = .7) +
        labs(title = paste('Sensor',unique(data_sensor$sensor)),
             x = "Fecha", y = "Turgor",
             color = 'Cluster') +
        theme_bw() +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        theme(plot.title = element_text(hjust = 0.5))
      
      p2 <- data_sensor |>
        ggplot(aes(x = as.factor(cluster), y = hora_minmax, color = as.factor(cluster))) +
        geom_boxplot(position = "dodge") +
        facet_wrap(~variable, scales = "free_y", ncol = 1, strip.position = 'right') +
        labs(title = 'Hora del mín y max por cluster',
             x = "Cluster", y = "Hora",
             color = 'Cluster') +
        theme_bw()
      
      p3 <- data_sensor |> 
        group_by(sensor, cluster) |>
        drop_na() |> 
        mutate(turgor_sc = scale(turgor)) |> 
        group_by(sitio, tratamiento, cluster, hora) |> 
        summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |> 
        ggplot(aes(hora, turgor_hora, color = as.factor(cluster))) +
        geom_point(size = 1) +
        geom_line(linewidth = .7) +
        labs(title = 'Ciclo horario del día por cluster',
             x = 'Hora',
             y = 'Turgor normalizado',
             color = 'Cluster') +
        facet_wrap(~cluster, scales = 'free_y', ncol = 1, strip.position = 'right') +
        theme_bw()
      
      p1 <- p1 + theme(plot.margin = margin(10, 10, 10, 10))
      p2 <- p2 + theme(plot.margin = margin(10, 10, 10, 10))
      p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
      
      combined_plot <- plot_grid(p1,
                                 plot_grid(p2, p3, ncol = 2, rel_widths = c(1, 1)),
                                 ncol = 1, rel_heights = c(1, 1))
      
      if (is.null(combined_plot_final)) {
        combined_plot_final <- combined_plot
      } else {
        combined_plot_final <- plot_grid(combined_plot_final, combined_plot, ncol = 2, rel_widths = c(1, 1))
      }
    }
  }
  
  if (length(sensores) < 2) {
    ggsave(paste0('reporte/analisis_turgor/plot_ultima_semana/',str_replace_all(plot_name, "\\s", "_"),'.png'), 
           plot = combined_plot_final, width = 14, height = 7, units = "in", dpi = 300)
  } else {
    combined_plot_final <- combined_plot_final +
      labs(title = plot_name) +
      theme(plot.title = element_text(hjust = 0, size = 14),
            plot.margin = margin(15, 10, 10, 10))
    
    ggsave(paste0('reporte/analisis_turgor/plot_ultima_semana/',str_replace_all(plot_name, "\\s", "_"),'.png'), 
           plot = combined_plot_final,
           width = 14, height = 7, units = "in", dpi = 300)
  }
}
