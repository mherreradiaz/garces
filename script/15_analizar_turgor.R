
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

# ULTIMA SEMANA ####

data_turgor <- read_rds('data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2023-2024') |>
  filter(fecha >= substr(now()-days(7),1,10)) |>
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
  mutate(cluster = as.factor(cluster)) |>
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
      labs(title = paste('Sensor', sensores[s]),
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
           y = 'Turgor estandarizado',
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
  ggsave(paste0('reporte/analisis_turgor/plot_utlima_semana/', str_replace_all(plot_name, "\\s", "_"), '.png'),
         plot = combined_plot_final, width = 14, height = 7, units = "in", dpi = 300)
}


# CLUSTERING POR VALOR SEGÚN HORA ####

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

# PLOT 

data_turgor <- read_rds('analisis/01_data_turgor_clusterizado.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  na.omit()

codigos <- data_turgor |>
  select(sitio,temporada,tratamiento,unidad,zim, sensor) |>
  distinct() |>
  arrange(temporada,sitio,tratamiento,unidad,zim) |>
  spread(key = 'zim', value = 'sensor')

for (x in 1:nrow(codigos)) {
  
  sensores <- c(codigos$Z1[x],codigos$Z2[x]) |>
    na.omit() |> as.numeric()
  
  plot_name <- paste(codigos$temporada[x],
                     str_to_title(str_replace_all(codigos$sitio[x], "_", " ")),
                     codigos$tratamiento[x],
                     'Unidad', codigos$unidad[x])
  
  if (codigos$temporada[x] == '2022-2023') {lim <- c(as.POSIXct('2022-09-30'),as.POSIXct('2023-04-01'))
  } else {lim <- c(as.POSIXct('2023-09-30'), max(as.POSIXct(data_turgor$fecha)))}
  
  combined_plot_final <- NULL
  
  for (s in 1:length(sensores)) {
    
    data_sensor <- data_turgor |>
      filter(sensor == sensores[s],
             temporada == codigos$temporada[x]) |>
      na.omit()
    
    p1 <- data_sensor |>
        ggplot(aes(fecha_hora, turgor, color = as.factor(cluster))) +
        geom_point(size = .7) +
        labs(title = paste('Sensor', sensores[s]),
             x = "Fecha", y = "Presión de parche (kPa)",
             color = 'Cluster') +
        theme_bw() +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_datetime(limits = lim)
    
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
      group_by(cluster) |>
      drop_na() |>
      mutate(turgor_sc = scale(turgor)) |>
      group_by(cluster,hora) |>
      summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |>
      ggplot(aes(hora, turgor_hora, color = as.factor(cluster))) +
      geom_point(size = 1) +
      geom_line(linewidth = .7) +
      labs(title = 'Ciclo horario del día',
           x = 'Hora',
           y = 'Presión de parche (estand.)',
           color = 'Cluster') +
      facet_wrap(~cluster, ncol = 1, strip.position = 'right') +
      theme_bw() +
      theme(axis.text.y = element_text(size = 8))
    
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
  png(paste0('reporte/plots/01_turgor_sensor/', gsub('-','_',str_replace_all(plot_name, "\\s", "_")), '.png'),
      width = 14 * 300, height = 7 * 300, units = "px", res = 300)
  print(combined_plot_final)
  dev.off()
}

# LIMPIEZA POR CORRELACIÓN CON CLIMA ####

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

# PLOT 

data_turgor <- read_rds('analisis/02_data_turgor_limpiado.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  na.omit()

codigos <- data_turgor |>
  select(temporada,sitio,tratamiento,unidad,zim, sensor) |>
  distinct() |>
  arrange(temporada,sitio,tratamiento,unidad,zim) |>
  spread(key = 'zim', value = 'sensor')

for (x in 1:nrow(codigos)) {
  
  sensores <- c(codigos$Z1[x],codigos$Z2[x]) |>
    na.omit() |> as.numeric()
  
  plot_name <- paste(codigos$temporada[x],
                     str_to_title(str_replace_all(codigos$sitio[x], "_", " ")),
                     codigos$tratamiento[x],
                     'Unidad', codigos$unidad[x])
  
  if (codigos$temporada[x] == '2022-2023') {lim <- c(as.POSIXct('2022-09-30'),as.POSIXct('2023-04-01'))
  } else {lim <- c(as.POSIXct('2023-09-30'), max(as.POSIXct(data_turgor$fecha)))}
  
  combined_plot_final <- NULL
  
  for (s in 1:length(sensores)) {
    
    data_sensor <- data_turgor |>
      filter(sensor == sensores[s],
             temporada == codigos$temporada[x]) |>
      na.omit()
    
    p1 <- data_sensor |>
      ggplot(aes(fecha_hora, turgor, color = as.factor(cluster))) +
      geom_point(size = .7) +
      labs(title = paste('Sensor', sensores[s]),
           x = "Fecha", y = "Presion de parche (kPa)",
           color = 'Cluster') +
      theme_bw() +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_datetime(limits = lim)
    
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
           y = 'Presión de parche (estand.)',
           color = 'Cluster') +
      facet_wrap(~cluster, ncol = 1, strip.position = 'right') +
      theme_bw() +
      theme(axis.text.y = element_text(size = 8))
    
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
  # plot_final <- plot_grid(ggdraw() + draw_label(paste('Unidad',codigos$unidad[x]), size = 14, hjust = .5),
  #                         combined_plot_final, 
  #                         ncol = 1, rel_heights = c(.07,1))
  png(paste0('reporte/plots/02_turgor_limpiado/', gsub('-','_',str_replace_all(plot_name, "\\s", "_")), '.png'),
      width = 14 * 300, height = 7 * 300, units = "px", res = 300)
  print(combined_plot_final)
  dev.off()
  
}

# NORMALIZAR Y JUNTAR ####

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

# PLOT 

data_clima <- read_rds('data/data_processed/clima.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  select(sitio,temporada,fecha_hora,t_media,vpd_media) |>
  group_by(temporada,sitio) |>
  mutate(t_media = scale(t_media),
         vpd_media = scale(vpd_media),
         vpd_trans = scale(log(vpd_media + 1))) |>
  ungroup()

data_turgor <- read_rds('analisis/03_data_turgor_sensor_prepro.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  left_join(data_clima,by=c('sitio','temporada','fecha_hora'))

codigos <- data_turgor |>
  select(sitio,temporada,tratamiento,unidad,zim, sensor) |>
  distinct() |>
  arrange(temporada,sitio,tratamiento,unidad,zim) |>
  spread(key = 'zim', value = 'sensor')

for (x in 1:nrow(codigos)) {
  
  sensores <- c(codigos$Z1[x],codigos$Z2[x]) |>
    na.omit() |> as.numeric()
  
  plot_name <- paste(codigos$temporada[x],
                     str_to_title(str_replace_all(codigos$sitio[x], "_", " ")),
                     codigos$tratamiento[x],
                     'Unidad', codigos$unidad[x])
  
  combined_plot_final <- NULL
  
  if (codigos$temporada[x] == '2022-2023') {lim <- c(as.POSIXct('2022-09-30'),as.POSIXct('2023-04-01'))
  } else {lim <- c(as.POSIXct('2023-09-30'), max(as.POSIXct(data_turgor$fecha)))}
  
  for (s in 1:length(sensores)) {
    
    data_sensor <- data_turgor |>
      filter(sensor == sensores[s],
             temporada == codigos$temporada[x]) |>
      na.omit()
    
    p1 <- data_sensor |>
      pivot_longer(cols = c(turgor,turgor_sc),names_to = 'proceso',values_to = 'valor') |>
      mutate(cluster = as.factor(ifelse(proceso == 'turgor_sc','Merge',cluster))) |>
      ggplot(aes(fecha_hora,valor, color = cluster)) +
      geom_point(size = .7) +
      facet_wrap(~proceso, ncol = 1, scales = "free_y", labeller = as_labeller(c(turgor = 'Presión de parche (clusterizada)',
                                                                                 turgor_sc = 'Clusters estandarizados y unidos'))) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 15)),
            legend.position = 'none') +
      scale_x_datetime(limits = lim) +
      xlab(NULL) +
      labs(title = paste('Sensor', sensores[s]),
           y = 'Turgor') +
      scale_color_manual(values = c(hue_pal()(length(unique(pull(data_sensor,cluster)))),'black'))
    
    max <- max(data_sensor$turgor_sc)-.1*(max(data_sensor$turgor_sc)-min(data_sensor$turgor_sc))
    
    p2 <- data_sensor |>
      pivot_longer(cols = c(vpd_trans,t_media), names_to = 'variable',values_to = 'valor') |>
      ggplot(aes(valor,turgor_sc)) +
      geom_point() +
      geom_smooth(method = 'lm', se = F) +
      stat_cor(method = 'pearson', label.y = max, color = 'black',geom = 'label') +
      facet_wrap(~variable, scale = 'fixed', ncol = 2, strip.position = 'top',
                 labeller = as_labeller(c(t_media = 'T° media (estand.)',
                                          vpd_trans = 'VPD medio (log-estand.)'))) +
      labs(title = 'Correlación con Temperatura y VPD',
           y = 'Presión de parche (estand.)',
           x = NULL) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
    p3 <- data_sensor |>
      group_by(cluster) |>
      drop_na() |>
      mutate(turgor_sc = scale(turgor)) |>
      ungroup() |>
      group_by(hora) |>
      summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |>
      mutate(color = as.factor(1)) |>
      ggplot(aes(hora, turgor_hora, color = color)) +
      geom_point(size = 1) +
      geom_line(linewidth = .7) +
      labs(title = 'Ciclo horario del día',
           x = 'Hora',
           y = 'Presión de parche (estand.)') +
      theme_bw() +
      scale_color_manual(values = "black") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      guides(color = guide_legend(override.aes = list(color = NA)))
    
    p1 <- p1 + theme(plot.margin = margin(10, 10, 10, 10))
    p2 <- p2 + theme(plot.margin = margin(10, 10, 10, 10))
    p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
    
    combined_plot <- plot_grid(p1, plot_grid(p2,p3,ncol = 2, rel_widths = c(2,1)), 
                               ncol = 1, rel_heights = c(1.5, 1))
    
    if (is.null(combined_plot_final)) {
      combined_plot_final <- combined_plot
    } else {
      combined_plot_final <- plot_grid(combined_plot_final, combined_plot, ncol = 2, rel_widths = c(1, 1))
    }
  }
  png(paste0('reporte/plots/03_turgor_union/', gsub('-','_',str_replace_all(plot_name, "\\s", "_")), '.png'),
      width = 14 * 300, height = 7 * 300, units = "px", res = 300)
  print(combined_plot_final)
  dev.off()
  
}

# UNIFICAR TURGOR POR UNIDAD ####

data_turgor <- read_rds('analisis/03_data_turgor_sensor_prepro.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_unidad <- data_turgor |>
  group_by(sitio,temporada,fecha,hora,fecha_hora,tratamiento,unidad) |>
  summarise(turgor_sc = mean(turgor_sc,na.rm=T)) |>
  arrange(temporada,sitio,tratamiento,unidad,fecha_hora) |>
  ungroup() |>
  select(-fecha_hora) |>
  write_rds('analisis/04_data_turgor_unidad_prepro.rds')

# PLOT

data_unidad <- read_rds('analisis/04_data_turgor_unidad_prepro.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_clima <- read_rds('data/data_processed/clima.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  select(sitio,temporada,fecha_hora,t_media,vpd_media) |>
  group_by(temporada,sitio) |>
  mutate(t_media = scale(t_media),
         vpd_media = scale(vpd_media),
         vpd_trans = scale(log(vpd_media + 1))) |>
  ungroup()

data_unidad <- data_unidad |>
  left_join(data_clima,by=c('sitio','temporada','fecha_hora'))

codigos <- data_unidad |>
  select(sitio,temporada,tratamiento,unidad) |>
  distinct() |>
  arrange(temporada,sitio,tratamiento,unidad)

for (x in 1:nrow(codigos)) {
  
  plot_name <- paste(codigos$temporada[x],
                     str_to_title(str_replace_all(codigos$sitio[x], "_", " ")),
                     codigos$tratamiento[x],
                     'Unidad', codigos$unidad[x])
  
  if (codigos$temporada[x] == '2022-2023') {lim <- c(as.POSIXct('2022-09-30'),as.POSIXct('2023-04-01'))
  } else {lim <- c(as.POSIXct('2023-09-30'), max(as.POSIXct(data_turgor$fecha)))}
  
  data <- data_unidad |>
    filter(temporada == codigos$temporada[x],
           sitio == codigos$sitio[x],
           tratamiento == codigos$tratamiento[x],
           unidad == codigos$unidad[x]) |>
    pivot_longer(cols = c(vpd_trans,t_media), names_to = 'variable',values_to = 'valor') |>
    na.omit()
  
  p1 <- data |>
    ggplot(aes(fecha_hora,turgor_sc, group = fecha)) +
    geom_line(color = "darkblue", alpha = 0.5) +
    labs(title = 'Serie temporal',
         y = 'Presión de parche (estand.') +
    xlab(NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 15)),
          legend.position = 'none') +
    scale_x_datetime(limits = lim)
  
  max <- max(data$turgor_sc)-.2*(max(data$turgor_sc)-min(data$turgor_sc))
  
  p2 <- data |>
    ggplot(aes(valor,turgor_sc)) +
    geom_point() +
    geom_smooth(method = 'lm', se = F) +
    stat_cor(method = 'pearson', label.y = max, color = 'black',geom = 'label') +
    facet_wrap(~variable, scale = 'fixed', ncol = 1, strip.position = 'bottom',
               labeller = as_labeller(c(t_media = 'Temperatura media (estandarizada)',
                                        vpd_trans = 'VPD medio (logaritmico-estandarizado)'))) +
    labs(title = 'Correlación con Temperatura y VPD',
         y = 'Presión de parche (estand.)',
         x = NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
    
  p3 <- data |>
    group_by(hora) |>
    summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |>
    mutate(color = as.factor(1)) |>
    ggplot(aes(hora, turgor_hora)) +
    geom_point(size = 1) +
    geom_line(linewidth = .7) +
    labs(title = 'Ciclo horario del día',
         x = 'Hora',
         y = 'Presión de parche (estand.)\n') +
    theme_bw() +
    scale_color_manual(values = "black") +
    scale_y_continuous(position = "right") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
  p1 <- p1 + theme(plot.margin = margin(10, 10, 5, 10))
  p2 <- p2 + theme(plot.margin = margin(10, 10, 10, 10))
  p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
  
  p4 <- plot_grid(ggdraw(),p3,ggdraw(),ncol = 1, rel_heights = c(.4,1,.4))
  
  p5 <- plot_grid(p2,p4,ncol = 2,rel_widths = c(1,.7))
  
  combined_plot <- plot_grid(p1, p5, ncol = 1, rel_heights = c(.8,1))
  
  png(paste0('reporte/plots/04_turgor_unidad/', gsub('-','_',str_replace_all(plot_name, "\\s", "_")), '.png'),
      width = 10 * 300, height = 7 * 300, units = "px", res = 300)
  print(combined_plot)
  dev.off()
  
}

# UNIFICAR TURGOR POR TRATAMIENTO ####

data_turgor <- read_rds('analisis/04_data_turgor_unidad_prepro.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_trat <- data_turgor |>
  group_by(sitio,temporada,fecha,hora,fecha_hora,tratamiento) |>
  summarise(turgor_sc = mean(turgor_sc,na.rm=T)) |>
  arrange(temporada,sitio,tratamiento,fecha_hora) |>
  ungroup()  |>
  select(-fecha_hora) |>
  write_rds('analisis/05_data_turgor_tratamiento_prepro.rds')

# PLOT

data_trat <- read_rds('analisis/05_data_turgor_tratamiento_prepro.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_clima <- read_rds('data/data_processed/clima.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  select(sitio,temporada,fecha_hora,t_media,vpd_media) |>
  group_by(temporada,sitio) |>
  mutate(t_media = scale(t_media),
         vpd_media = scale(vpd_media),
         vpd_trans = scale(log(vpd_media + 1))) |>
  ungroup()

data_trat <- data_trat |>
  left_join(data_clima,by=c('sitio','temporada','fecha_hora'))

codigos <- data_trat |>
  select(sitio,temporada,tratamiento) |>
  distinct() |>
  arrange(temporada,sitio,tratamiento)

for (x in 1:nrow(codigos)) {
  
  plot_name <- paste(codigos$temporada[x],
                     str_to_title(str_replace_all(codigos$sitio[x], "_", " ")),
                     codigos$tratamiento[x])
  
  if (codigos$temporada[x] == '2022-2023') {lim <- c(as.POSIXct('2022-09-30'),as.POSIXct('2023-04-01'))
  } else {lim <- c(as.POSIXct('2023-09-30'), max(as.POSIXct(data_turgor$fecha)))}
  
  data <- data_trat |>
    filter(temporada == codigos$temporada[x],
           sitio == codigos$sitio[x],
           tratamiento == codigos$tratamiento[x]) |>
    pivot_longer(cols = c(vpd_trans,t_media), names_to = 'variable',values_to = 'valor') |>
    na.omit()
  
  p1 <- data |>
    ggplot(aes(fecha_hora,turgor_sc, group = fecha)) +
    geom_line(color = "darkblue", alpha = 0.5) +
    labs(title = 'Serie temporal',
         y = 'Presión de parche (estand.)') +
    xlab(NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 15)),
          legend.position = 'none') +
    scale_x_datetime(limits = lim)
  
  max <- max(data$turgor_sc)-.2*(max(data$turgor_sc)-min(data$turgor_sc))
  
  p2 <- data |>
    ggplot(aes(valor,turgor_sc)) +
    geom_point() +
    geom_smooth(method = 'lm', se = F) +
    stat_cor(method = 'pearson', label.y = max, color = 'black',geom = 'label') +
    facet_wrap(~variable, scale = 'fixed', ncol = 1, strip.position = 'bottom',
               labeller = as_labeller(c(t_media = 'Temperatura media (estandarizada)',
                                        vpd_trans = 'VPD medio (logaritmico-estandarizado)'))) +
    labs(title = 'Correlación con Temperatura y VPD',
         y = 'Presión de parche (estand.)',
         x = NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p3 <- data |>
    group_by(hora) |>
    summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |>
    mutate(color = as.factor(1)) |>
    ggplot(aes(hora, turgor_hora)) +
    geom_point(size = 1) +
    geom_line(linewidth = .7) +
    labs(title = 'Ciclo horario del día',
         x = 'Hora',
         y = 'Presión de parche (estand.)\n') +
    theme_bw() +
    scale_color_manual(values = "black") +
    scale_y_continuous(position = "right") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
  p1 <- p1 + theme(plot.margin = margin(10, 10, 5, 10))
  p2 <- p2 + theme(plot.margin = margin(10, 10, 10, 10))
  p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
  
  p4 <- plot_grid(ggdraw(),p3,ggdraw(),ncol = 1, rel_heights = c(.4,1,.4))
  
  p5 <- plot_grid(p2,p4,ncol = 2,rel_widths = c(1,.7))
  
  combined_plot <- plot_grid(p1, p5, ncol = 1, rel_heights = c(.8,1))
  
  png(paste0('reporte/plots/05_turgor_tratamiento/', gsub('-','_',str_replace_all(plot_name, "\\s", "_")), '.png'),
      width = 10 * 300, height = 7 * 300, units = "px", res = 300)
  print(combined_plot)
  dev.off()
  
}

# CORRELACIÓN VPD Y TEMPERATURA ####

data_clima <- read_rds('data/data_processed/clima.rds') |>
  select(sitio,temporada,fecha,hora,t_media,vpd_media,eto) |>
  group_by(temporada,sitio) |>
  mutate(t_media = scale(t_media),
         vpd_media = scale(vpd_media),
         eto = scale(eto)) |>
  ungroup()

data_turgor <- read_rds('analisis/data_turgor_merge_clima.rds') |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_cor <- data_turgor |>
  left_join(data_clima,by=c('sitio','temporada','fecha','hora'))

data_cor$vpd_trans <- scale(log(data_cor$vpd_media + 1))

{

codigos <- data_cor |>
  select(sitio,temporada,tratamiento,unidad,zim, sensor) |>
  distinct() |>
  arrange(temporada,sitio,tratamiento,unidad,zim) |>
  spread(key = 'zim', value = 'sensor')

for (x in 1:nrow(codigos)) {
  
  sensores <- c(codigos$Z1[x],codigos$Z2[x]) |>
    na.omit() |> as.numeric()
  
  plot_name <- paste(codigos$temporada[x],
                     str_to_title(str_replace_all(codigos$sitio[x], "_", " ")),
                     codigos$tratamiento[x],
                     'Unidad', codigos$unidad[x])
  
  combined_plot_final <- NULL
  
  for (s in 1:length(sensores)) {
    
    data_sensor <- data_cor |>
      filter(sensor == sensores[s],
             temporada == codigos$temporada[x]) |>
      pivot_longer(cols = c(vpd_trans,t_media), names_to = 'variable',values_to = 'valor') |>
      na.omit()
      
    plot <- data_sensor |>
      ggplot(aes(valor,turgor_sc)) +
      geom_point() +
      geom_smooth(method = 'lm', se = F) +
      stat_cor(method = 'pearson', label.y = 1.8, color = 'black',geom = 'label') +
      facet_wrap(~variable, scale = 'free', ncol = 1, strip.position = 'bottom',
                 labeller = as_labeller(c(t_media = 'Temperatura media (estandarizada)',
                                          vpd_trans = 'VPD medio (logaritmico-estandarizado)'))) +
      labs(title = paste('Sensor',sensores[s]),
           y = 'Turgor (estandarizado)',
           x = NULL) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

    if (is.null(combined_plot_final)) {
      combined_plot_final <- plot
    } else {
      combined_plot_final <- plot_grid(combined_plot_final, plot, ncol = 2, rel_widths = c(1, 1))
    }
  }
  
  png(paste0('reporte/analisis_turgor/plot_cor_cor/', str_replace_all(plot_name, "\\s", "_"), '.png'),
      width = 14 * 300, height = 7 * 300, units = "px", res = 300)
  print(combined_plot_final)
  dev.off()
  
}

}

# data_cor |>
#   filter(sensor == 8569) |>
#   ggplot(aes(fecha_hora,turgor_sc)) +
#   geom_line(aes(color = 'Turgor_sc')) +
#   geom_line(aes(y=vpd_trans,color = 'VPD_media_sc')) +
#   theme_bw() +
#   labs(title = 'Turgor y VPD media (estandarizados)',
#        y = 'Escala estandarizada',
#        x = 'Fecha',
#        color = 'Variable')
# 
# data_cor |>
#   filter(sensor == 8569) |>
#   ggplot(aes(fecha_hora,turgor_sc)) +
#   geom_line(aes(color = 'Turgor_sc')) +
#   geom_line(aes(y=t_media,color = 'Temperatura_media_sc')) +
#   theme_bw() +
#   labs(title = 'Turgor y Temperatura media (estandarizados)',
#        y = 'Escala estandarizada',
#        x = 'Fecha',
#        color = 'Variable')

cor_summary <- data_cor |>
  group_by(sitio,temporada,tratamiento,unidad,zim,sensor) |>
  summarise(t_cor = cor(turgor_sc,t_media, use = 'complete.obs'),
            vpd_cor = cor(turgor_sc,vpd_trans, use = 'complete.obs'))

min(cor_summary$t_cor)
min(cor_summary$vpd_cor)

hist(cor_summary$t_cor)
hist(cor_summary$vpd_cor)

filtro <- cor_summary |>
  filter(t_cor > .5,
         vpd_cor > .5) |>
  select(sitio:sensor) |>
  mutate(filtro = 1)

data_turgor <- data_turgor |>
  left_join(filtro,by=c('sitio','temporada','tratamiento','unidad','zim','sensor')) |>
  filter(filtro == 1) |>
  select(-filtro)

write_rds(data_turgor,'analisis/turgor_procesado.rds')

{
data_eto <- data_clima |>
  select(sitio,fecha,eto) |>
  distinct()
  
data_cor_eto <- data_turgor |>
  group_by(sitio,fecha,sensor) |>
  summarise(turgor_dia = mean(turgor_sc,na.rm=T)) |>
  ungroup() |>
  left_join(data_eto, by= c('sitio','fecha')) |>
  mutate(fecha = as.Date(fecha))

data_cor_eto |>
  filter(sensor == 8569) |>
  ggplot(aes(fecha,turgor_dia)) +
  geom_line(aes(color = 'Turgor_sc')) +
  geom_line(aes(y=eto,color = 'ET0_sc')) +
  theme_bw() +
  labs(title = 'Turgor y ET0 (estandarizados)',
       y = 'Escala estandarizada',
       x = 'Fecha',
       color = 'Variable')
  } # ET0
  

