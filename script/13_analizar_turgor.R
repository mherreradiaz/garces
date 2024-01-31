
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


# CLUSTERING POR MIN MAX ####

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
  
  #if (nrow(data_sensor) == 0) {next}
  
  clusters <- data_sensor |>
    select(mean) |>
    scale() |>
    #dbscan(eps = .6, minPts = 20) |>
    kmeans(centers = 2) |>
    pluck('cluster')

  data_sensor <- data_sensor |>
    mutate(cluster = clusters) |>
    select(fecha,sensor,cluster)

  # data_turgor <- data_turgor |>
  #   left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
  #   mutate(cluster = coalesce(cluster,cluster.x)) |>
  #   select(-cluster.x)
  
  data_turgor |> 
    left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
    mutate(cluster = coalesce(cluster,cluster.x)) |>
    select(-cluster.x) |>
    filter(sensor == unique(data_turgor$sensor)[x]) |>
    mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
    na.omit() |>
    ggplot(aes(fecha_hora,turgor,color = as.factor(cluster))) +
    geom_point(size = .7) +
    theme_bw()
  
  data_turgor |>
    left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
    mutate(cluster = coalesce(cluster,cluster.x)) |>
    select(-cluster.x) |>
    mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
    left_join(data_dia, by = c('fecha','sensor')) |>
    pivot_longer(cols = c(hora_min, hora_max),
                 names_to = "variable",
                 values_to = "hora_minmax") |>
    filter(sensor == unique(data_turgor$sensor)[x]) |>
    na.omit() |>
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

# CLUSTERING POR VALOR BRUTO ####

data_turgor <- read_rds('data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2023-2024') |>
  mutate(fecha = as.Date(fecha)) |>
  mutate(cluster = NA)

for (x in 1:length(unique(data_turgor$sensor))) {
  
  data_sensor <- data_turgor |>
    filter(sensor == unique(data_turgor$sensor)[x])
  
  #if (nrow(data_sensor) == 0) {next}
  
  clusters <- data_sensor |>
    select(turgor) |>
    na.omit() |>
    #dbscan(eps = .2, minPts = 50) |>
    kmeans(centers = 3) |>
    pluck('cluster')
  
  table(clusters)
  
  data_sensor <- data_sensor |>
    filter(complete.cases(turgor)) |>
    mutate(cluster = clusters) |>
    select(fecha,hora,sensor,cluster)
  
  # data_turgor <- data_turgor |>
  #   left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
  #   mutate(cluster = coalesce(cluster,cluster.x)) |>
  #   select(-cluster.x)
  
  data_turgor |> 
    left_join(data_sensor, by = c('fecha','hora','sensor'), suffix = c('','.x')) |>
    mutate(cluster = coalesce(cluster,cluster.x)) |>
    select(-cluster.x) |>
    filter(sensor == unique(data_turgor$sensor)[x]) |>
    mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
    na.omit() |>
    ggplot(aes(fecha_hora,turgor,color = as.factor(cluster))) +
    geom_point(size = .7) +
    theme_bw()
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

# CLUSTERING POR VALOR SEGÚN HORA ####

# Análisis PCA

data_turgor <- read_rds('data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2023-2024') |>
  mutate(fecha = as.Date(fecha))

for (x in 1:length(unique(data_turgor$sensor))) {
  
  data_sensor <- data_turgor |>
    filter(sensor == unique(data_turgor$sensor)[x]) |>
    spread(key = hora, value = turgor, sep = "_") |>
    filter(complete.cases(across(hora_0:hora_23)))
 
  data_sensor[, paste0("hora_", 0:23)] |>
    prcomp(scale. = T) |> 
    pluck('x') |> 
    cbind(data_sensor) |>
    ggplot(aes(PC1, PC2)) +
    geom_point() +
    labs(title = "Gráfico de Dispersión en el Espacio de Componentes Principales",
         x = "Componente Principal 1",
         y = "Componente Principal 2") +
    theme_minimal()
  
}

# Clustering

data_turgor <- read_rds('data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2023-2024') |>
  #mutate(fecha = as.Date(fecha)) |>
  mutate(cluster = NA)

for (x in 1:length(unique(data_turgor$sensor))) {
  
  data_sensor <- data_turgor |>
    filter(sensor == unique(data_turgor$sensor)[x]) |>
    spread(key = hora, value = turgor, sep = "_") |>
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
    kmeans(centers = 6) |>
    #dbscan(eps = 2, minPts = 10) |>
    pluck('cluster')
    
  data_sensor <- data_sensor |>
    mutate(cluster = clusters) |>
    select(fecha,sensor,cluster)
  
  data_turgor <- data_turgor |>
    left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
    mutate(cluster = coalesce(cluster,cluster.x)) |>
    select(-cluster.x)
  
  # data_turgor |>
  #   left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
  #   mutate(cluster = coalesce(cluster,cluster.x)) |>
  #   select(-cluster.x) |>
  #   filter(sensor == unique(data_turgor$sensor)[x]) |>
  #   mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  #   na.omit() |>
  #   ggplot(aes(fecha_hora,turgor,color = as.factor(cluster))) +
  #   geom_point(size = .7) +
  #   theme_bw()

  # data_turgor |>
  #   left_join(data_sensor, by = c('fecha','sensor'), suffix = c('','.x')) |>
  #   mutate(cluster = coalesce(cluster,cluster.x)) |>
  #   select(-cluster.x) |>
  #   mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  #   left_join(data_dia, by = c('fecha','sensor')) |>
  #   pivot_longer(cols = c(hora_min, hora_max),
  #                names_to = "variable",
  #                values_to = "hora_minmax") |>
  #   filter(sensor == unique(data_turgor$sensor)[x]) |>
  #   na.omit() |>
  #   group_by(sensor, cluster) |>
  #   drop_na() |>
  #   mutate(turgor_sc = scale(turgor)) |>
  #   group_by(sitio, tratamiento, cluster, hora) |>
  #   summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |>
  #   ggplot(aes(hora, turgor_hora, color = as.factor(cluster))) +
  #   geom_point(size = 1) +
  #   geom_line(linewidth = .7) +
  #   labs(title = 'Ciclo horario del día por cluster',
  #        x = 'Hora',
  #        y = 'Turgor normalizado',
  #        color = 'Cluster') +
  #   facet_wrap(~cluster, scales = 'free_y', ncol = 1, strip.position = 'right') +
  #   theme_bw()
  
}

data_turgor |>
  mutate(cluster = as.factor(cluster)) |>
  write_rds('analisis/data_turgor_cluster.rds')

# data_turgor |>
#   mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
#   na.omit() |>
#   ggplot(aes(fecha_hora,turgor,color = as.factor(cluster))) +
#   geom_point(size = .7) +
#   facet_grid(tratamiento+sitio~unidad) +
#   theme_bw() +
#   guides(color = guide_legend(override.aes = list(size = 2)))

# ANALISIS POR SENSOR ####

data_turgor <- read_rds('analisis/data_turgor_cluster.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
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
      ggplot(aes(fecha_hora, turgor, color = as.factor(cluster))) +
      geom_point(size = .7) +
      labs(title = if (length(sensores) < 2) paste0(plot_name, ': Sensor ', unique(data_sensor$sensor))
           else paste('Sensor', unique(data_sensor$sensor)),
           x = "Fecha", y = "Turgor",
           color = 'Cluster') +
      theme_bw() +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_datetime(limits = c(min(as.POSIXct(data_turgor$fecha)), max(as.POSIXct(data_turgor$fecha))))
    
    p2 <- data_sensor |>
      group_by(cluster) |>
      drop_na() |>
      mutate(turgor_sc = scale(turgor)) |>
      group_by(cluster,fecha) |>
      summarise(min = hora[which.min(turgor_sc)],
                max = hora[which.max(turgor_sc)]) |>
      pivot_longer(cols = c(min,max),names_to = 'hora_var') |>
      mutate(value = ifelse(hora_var == "min",
                            ifelse(value > 11, value - 12, value + 12),
                            value)) |>
      ggplot(aes(x = cluster, y = value, color = as.factor(cluster))) +
      geom_boxplot(position = "dodge") +
      facet_wrap(. ~ hora_var, ncol = 1, strip.position = 'left',
                 labeller = as_labeller(c(max = 'Hora max (am - pm)',
                                          min = 'Hora min (pm - am)'))) +
      ylab(NULL) +
      scale_y_continuous(limits = c(0,23), breaks = c(0, 6, 12, 18, 23), labels = c(12, 6, 11, 6, 11)) +
      labs(title = 'Hora del mín y max',
           x = "Cluster",
           color = 'Cluster') +
      theme(strip.background = element_blank(),
            strip.placement = 'outside') +
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
      labs(title = 'Ciclo horario del día',
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
  ggsave(paste0('reporte/analisis_turgor/plot/', str_replace_all(plot_name, "\\s", "_"), '.png'),
         plot = combined_plot_final, width = 14, height = 7, units = "in", dpi = 300)
}

# ULTIMA SEMANA ####

data_turgor <- read_rds('analisis/data_turgor_cluster.rds') |>
  filter(fecha >= substr(now()-days(7),1,10)) |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
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
      ggplot(aes(fecha_hora, turgor, color = as.factor(cluster))) +
      geom_point(size = .7) +
      labs(title = if (length(sensores) < 2) paste0(plot_name, ': Sensor ', unique(data_sensor$sensor))
           else paste('Sensor', unique(data_sensor$sensor)),
           x = "Fecha", y = "Turgor",
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
           y = 'Turgor normalizado',
           color = 'Cluster') +
      facet_wrap(~cluster, scales = 'free_y', ncol = 1, strip.position = 'right') +
      theme_bw()
    
    p1 <- p1 + theme(plot.margin = margin(10, 10, 10, 10))
    p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
    
    combined_plot <- plot_grid(p1, p3, ncol = 2, rel_widths = c(1, 1))
    
    if (is.null(combined_plot_final)) {
      combined_plot_final <- combined_plot
    } else {
      combined_plot_final <- plot_grid(combined_plot_final, combined_plot, ncol = 2, rel_widths = c(1, 1))
    }
  }
  ggsave(paste0('reporte/analisis_turgor/plot_utlima_semana/', str_replace_all(plot_name, "\\s", "_"), '.png'),
         plot = combined_plot_final, width = 14, height = 7, units = "in", dpi = 300)
}

# LIMPIEZA DE DATOS ####

data_turgor <- read_rds('analisis/data_turgor_cluster.rds')

filtro <- data_turgor |>
  group_by(sensor,cluster) |>
  drop_na() |>
  mutate(turgor_sc = scale(turgor)) |>
  group_by(sensor,cluster,hora) |>
  summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |>
  group_by(sensor,cluster) |>
  summarise(min = hora[which.min(turgor_hora)],
            max = hora[which.max(turgor_hora)]) |>
  filter(!(max >= 21 | max <=5),
         !(min >=10 & min <= 22)) |>
  mutate(filtro = 1) |>
  select(-min,-max)

data_turgor <- data_turgor |>
  left_join(filtro, by = c('sensor','cluster')) |>
  filter(filtro == 1) |>
  select(-filtro)

{
  data_turgor <- data_turgor |>
    mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
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
        ggplot(aes(fecha_hora, turgor, color = as.factor(cluster))) +
        geom_point(size = .7) +
        labs(title = if (length(sensores) < 2) paste0(plot_name, ': Sensor ', unique(data_sensor$sensor))
             else paste('Sensor', unique(data_sensor$sensor)),
             x = "Fecha", y = "Turgor",
             color = 'Cluster') +
        theme_bw() +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_datetime(limits = c(min(as.POSIXct(data_turgor$fecha)), max(as.POSIXct(data_turgor$fecha))))
      
      p2 <- data_sensor |>
        group_by(cluster) |>
        drop_na() |>
        mutate(turgor_sc = scale(turgor)) |>
        group_by(cluster,fecha) |>
        summarise(min = hora[which.min(turgor_sc)],
                  max = hora[which.max(turgor_sc)]) |>
        pivot_longer(cols = c(min,max),names_to = 'hora_var') |>
        mutate(value = ifelse(hora_var == "min",
                                    ifelse(value > 11, value - 12, value + 12),
                                    value)) |>
        ggplot(aes(x = cluster, y = value, color = as.factor(cluster))) +
        geom_boxplot(position = "dodge") +
        facet_wrap(. ~ hora_var, ncol = 1, strip.position = 'left',
                   labeller = as_labeller(c(max = 'Hora max (am - pm)',
                                            min = 'Hora min (pm - am)'))) +
        ylab(NULL) +
        scale_y_continuous(limits = c(0,23), breaks = c(0, 6, 12, 18, 23), labels = c(12, 6, 11, 6, 11)) +
        labs(title = 'Hora del mín y max',
             x = "Cluster",
             color = 'Cluster') +
        theme(strip.background = element_blank(),
              strip.placement = 'outside') +
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
        labs(title = 'Ciclo horario del día',
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
    ggsave(paste0('reporte/analisis_turgor/plot_filtro_1/', str_replace_all(plot_name, "\\s", "_"), '.png'),
           plot = combined_plot_final, width = 14, height = 7, units = "in", dpi = 300)
  }
  }

write_rds(data_turgor,'analisis/data_turgor_f1.rds')

# NORMALIZAR Y JUNTAR

data_turgor <- read_rds('analisis/data_turgor_f1.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

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
  
  for (s in 1:length(sensores)) {
    
    data_sensor <- data_turgor |>
      filter(sensor == sensores[s]) |>
      na.omit()
    
    p1 <- data_sensor |>
      ggplot(aes(fecha_hora, turgor, color = as.factor(cluster))) +
      geom_point(size = .7) +
      labs(title = if (length(sensores) < 2) paste0(plot_name, ': Sensor ', unique(data_sensor$sensor))
           else paste('Sensor', unique(data_sensor$sensor)),
           x = "Fecha", y = "Turgor",
           color = 'Cluster') +
      theme_bw() +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_datetime(limits = c(min(as.POSIXct(data_turgor$fecha)), max(as.POSIXct(data_turgor$fecha))))
  
  data_unida <- data_sensor |>
    group_by(sensor,cluster) |>
    drop_na() |>
    mutate(turgor_sc = scale(turgor)) |>
    ungroup()
  
  join <- data_sensor |>
    select(sitio,temporada,tratamiento,unidad,codigo,zim,sensor) |>
    distinct() |>
    arrange(sitio,tratamiento,unidad,zim)
  
  p <- tibble(fecha_hora = seq(min(data_sensor$fecha_hora),max(data_sensor$fecha_hora),by = '1 hour')) |>
    mutate(!!as.name(as.character(sensores[s])):= sensores[s]) |>
    pivot_longer(cols = c(!!as.name(as.character(sensores[s]))),
                 names_to = 'sensor') |>
    select(-value) |>
    mutate(sensor = as.numeric(sensor)) |>
    left_join(join, by='sensor') |>
    left_join(data_unidad,by= NULL) |>
    mutate(fecha = substr(as.character(fecha_hora),1,10),
           hora = hour(fecha_hora)) |>
    ggplot(aes(fecha_hora,turgor_sc)) +
    geom_line() +
    labs(title = plot_name,
         y = 'Turgor normalizado',
         x = 'Fecha') +
    theme_bw() +
    scale_x_datetime(limits = c(min(as.POSIXct(data_turgor$fecha)), max(as.POSIXct(data_turgor$fecha))))
  
  }
    
    ggsave(plot = p, paste0('reporte/analisis_turgor/plot_merge/', str_replace_all(plot_name, "\\s", "_"), '.png'),
           width = 10, height = 5, units = "in", dpi = 300)
  
}

data_unidad <- data_turgor |>
  filter(sensor %in% sensores) |>
  group_by(sensor,cluster) |>
  drop_na() |>
  mutate(turgor_sc = scale(turgor)) |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  ungroup()

join <- data_turgor |>
  select(sitio,temporada,tratamiento,unidad,codigo,zim,sensor) |>
  distinct() |>
  arrange(sitio,tratamiento,unidad,zim)

p <- tibble(fecha_hora = seq(min(data_unidad$fecha_hora),max(data_unidad$fecha_hora),by = '1 hour')) |>
  mutate(!!as.name(as.character(sensores[1])):= sensores[1],
         !!as.name(as.character(sensores[2])):= sensores[2]) |>
  pivot_longer(cols = c(!!as.name(as.character(sensores[1])),
                        !!as.name(as.character(sensores[2]))),
               names_to = 'sensor') |>
  select(-value) |>
  mutate(sensor = as.numeric(sensor)) |>
  left_join(join, by='sensor') |>
  left_join(data_unidad,by= NULL) |>
  mutate(fecha = substr(as.character(fecha_hora),1,10),
         hora = hour(fecha_hora)) |>
  ggplot(aes(fecha_hora,turgor_sc,color = zim)) +
  geom_line() +
  labs(title = plot_name,
       y = 'Turgor normalizado',
       x = 'Fecha',
       color = 'Zim') +
  facet_wrap(~zim, ncol = 1, labeller = as_labeller(c(Z1 = paste('Sensor',sensores[1]),
                                                      Z2 = paste('Sensor',sensores[1])))) +
  theme_bw()
