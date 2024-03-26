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

{
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
}

# LIMPIEZA

{
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
}

# UNION

{
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
}

# PREPROCESADO POR UNIDAD

{
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
    
    p1 <- tibble(fecha_hora = seq(min(data$fecha_hora),
                                  max(data$fecha_hora),
                                  by = '1 hour')) |>
      left_join(data,by='fecha_hora') |>
      ggplot(aes(fecha_hora,turgor_sc)) +
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
    
    png(paste0('reporte/plots/04_turgor_unidad/', gsub('-','_',str_replace_all(plot_name, "\\s", "_")), '.png'),
        width = 10 * 300, height = 7 * 300, units = "px", res = 300)
    print(combined_plot)
    dev.off()
    
  }
}

# PREPROCESADO POR TRATAMIENTO

{
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
    
    p1 <- tibble(fecha_hora = seq(min(data$fecha_hora),
                                  max(data$fecha_hora),
                                  by = '1 hour')) |>
      left_join(data,by='fecha_hora') |>
      ggplot(aes(fecha_hora,turgor_sc)) +
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
}

