library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(agvAPI)
library(ggplot2)
library(ggpubr)
library(cowplot)

data_turgor_old <- read_rds('data/data_processed/zim_turgor_mediahora.rds') |> 
  left_join(read_rds('data/data_processed/zim_turgor_mediahora_preprocesado.rds'),
            by = c('sitio','temporada','fecha','hora','tratamiento','unidad','codigo','zim','sensor','turgor')) |> 
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_turgor <- read_rds('data/data_processed/zim_turgor_mediahora_preprocesado.rds') |> 
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_clima <- read_rds('data/data_processed/clima_mediahora.rds')

data_turgor <- data_turgor |> 
  left_join(data_clima, by = c('sitio','temporada','fecha','hora'))

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
             temporada == codigos$temporada[x])
    
    data_sensor_old <- data_turgor_old |>
      filter(sensor == sensores[s],
             temporada == codigos$temporada[x])
    
    p1 <- data_sensor_old |>
      pivot_longer(cols = c(turgor,turgor_sc),names_to = 'proceso',values_to = 'valor') |>
      ggplot(aes(fecha_hora,valor)) +
      geom_point(size = .5) +
      facet_wrap(~proceso, ncol = 1, scales = "free_y", 
                 labeller = as_labeller(c(turgor = 'Serie original (kPa)',
                                          turgor_sc = 'Serie estandarizada y filtrada'))) +
      scale_x_datetime(date_breaks = "2 month", date_labels = "%b",
                       limits = lim) +
      labs(title = paste('Sensor', sensores[s]),
           y = 'Presión de parche',
           x = 'Meses') +
      theme_light() +
      theme(text = element_text(size = 13),
            plot.title = element_text(hjust = 0.5, margin = margin(b = 15)),
            axis.title.y = element_text(margin = margin(r = 17)),
            axis.title.x = element_text(margin = margin(t = 17)),
            legend.position = 'none') +
      guides(colour = guide_legend(override.aes = list(size=4)))
    
    max <- max(data_sensor$turgor_sc)-.1*(max(data_sensor$turgor_sc)-min(data_sensor$turgor_sc))
    
    p2 <- data_sensor |>
      pivot_longer(cols = c(vpd_sc_lag,t_sc_lag), names_to = 'variable',values_to = 'valor') |>
      ggplot(aes(valor,turgor_sc)) +
      geom_point() +
      geom_smooth(method = 'lm', se = F) +
      stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
               method = 'pearson', label.y = max, color = 'black',geom = 'label') +
      facet_wrap(~variable, scale = 'fixed', ncol = 2, strip.position = 'top',
                 labeller = as_labeller(c(t_sc_lag = 'T° media (est-lag)',
                                          vpd_sc_lag = 'VPD medio (log-est-lag)'))) +
      labs(title = 'Correlación con Temperatura y VPD',
           y = 'Presión de parche (est)',
           x = NULL) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
    p3 <- data_sensor |>
      group_by(hora) |>
      summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |>
      mutate(color = as.factor(1)) |>
      ggplot(aes(hora, turgor_hora, color = color)) +
      geom_point(size = 1) +
      geom_line(linewidth = .7) +
      labs(title = 'Ciclo horario del día',
           x = 'Hora',
           y = 'Presión de parche (est)') +
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
  png(paste0('reporte/plots/01_turgor_preprocesado_3/', gsub('-','_',str_replace_all(plot_name, "\\s", "_")), '.png'),
      width = 14 * 300, height = 7 * 300, units = "px", res = 300)
  print(combined_plot_final)
  dev.off()
  
}