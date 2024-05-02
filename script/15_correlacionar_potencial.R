source('script/funciones/paquetes.R')

# leer datos y unificar

data_turgor <- read_rds('data/data_processed/turgor.rds')

data_clima <- read_rds('data/data_processed/clima_lag.rds')

data <- data_turgor |> 
  left_join(data_clima, by = c('sitio','temporada','fecha','hora')) |> 
  group_by(across(sitio:codigo)) |> 
  summarise(turgor = mean(turgor,na.rm=T),
            turgor_filtrado = mean(turgor_filtrado,na.rm=T),
            turgor_relleno = mean(turgor_relleno,na.rm=T),
            t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup()

data_potencial_dia <- read_rds('data/data_processed/potencial.rds') |> 
  mutate(fecha = as.character(fecha),
         .before = tratamiento) |> 
  rename(potencial = potencial_bar)

data_cor_dia <- data |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(across(sitio:fecha),across(tratamiento:codigo)) |> 
  summarise(turgor = mean(turgor,na.rm=T),
         turgor_filtrado = mean(turgor_filtrado,na.rm=T),
         turgor_relleno = mean(turgor_relleno,na.rm=T),
         t_media = mean(t_media,na.rm=T),
         rh_media = mean(rh_media,na.rm=T),
         vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup() |> 
  left_join(data_potencial_dia,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) |> 
  select(sitio:turgor_filtrado,turgor_relleno,potencial,everything())

data_potencial_hora <- read_rds('data/data_processed/potencial_horario.rds') |> 
  rename(potencial = bar)

data_cor_hora <- data |> 
  filter(paste(sitio,fecha) %in% unique(paste(data_potencial_hora$sitio,data_potencial_hora$fecha))) |> 
  left_join(data_potencial_hora,by=c('sitio','temporada','fecha','hora','tratamiento','unidad','codigo')) |> 
  select(sitio:turgor_relleno,potencial,everything())

write_rds(data_cor_dia,'data/data_processed/turgor_potencial_dia.rds')
write_rds(data_cor_hora,'data/data_processed/turgor_potencial_hora.rds')

# visualización serie 

data |>
  mutate(fecha_hora = fecha_hora_f(fecha,hora)) |>
  filter(temporada == '2022-2023',
         fecha == '2023-01-13') |>
  ggplot(aes(fecha_hora,turgor_relleno)) +
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(y = expression("P"[p]),
       x = 'Fecha',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))


data |>
  mutate(fecha_hora = fecha_hora_f(fecha,hora)) |>
  filter(temporada == '2022-2023') |>
  pivot_longer(cols = c('turgor','turgor_filtrado','turgor_relleno'),
               names_to = 'turgor',values_to = 'valor') |>
  ggplot(aes(fecha_hora,valor,color=turgor)) +
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
  scale_color_hue(labels = c("Original", "Filtrado",'Relleno')) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(y = expression("P"[p]),
       x = 'Fecha',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

data |>
  mutate(fecha_hora = fecha_hora_f(fecha,hora)) |>
  filter(temporada == '2023-2024') |>
  pivot_longer(cols = c('turgor','turgor_filtrado','turgor_relleno'),
               names_to = 'turgor',values_to = 'valor') |>
  ggplot(aes(fecha_hora,valor,color=turgor)) +
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio,labeller = as_labeller(names)) +
  scale_color_hue(labels = c("Original", "Filtrado",'Relleno')) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(y = expression("P"[p]),
       x = 'Fecha',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

# visualización serie valor único diario

data_cor_dia |>
  mutate(fecha = as.Date(fecha)) |> 
  filter(temporada == '2022-2023') |>
  pivot_longer(cols = c('turgor','turgor_filtrado','turgor_relleno'),
               names_to = 'turgor',values_to = 'valor') |>
  ggplot(aes(fecha,valor,color=turgor)) +
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
  scale_color_hue(labels = c("Original", "Filtrado",'Relleno')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(y = expression("P"[p]),
       x = 'Fecha',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

data_cor_dia |>
  mutate(fecha = as.Date(fecha)) |> 
  filter(temporada == '2023-2024') |>
  pivot_longer(cols = c('turgor','turgor_filtrado','turgor_relleno'),
               names_to = 'turgor',values_to = 'valor') |>
  ggplot(aes(fecha,valor,color=turgor)) +
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
  scale_color_hue(labels = c("Original", "Filtrado",'Relleno')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(y = expression("P"[p]),
       x = 'Fecha',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

# visualización serie ciclo diario

data_cor_hora |> 
  filter(temporada == '2022-2023') |>
  mutate(hora = as.numeric(substr(hora,1,2))) |> 
  pivot_longer(cols = c('turgor','turgor_relleno','potencial','t_media'),
               names_to = 'tipo',values_to = 'valor') |>
  group_by(temporada,sitio,codigo,fecha,tipo) |> 
  mutate(valor = as.numeric(scale(valor))) |> 
  ggplot(aes(hora,valor,color=tipo)) +
  geom_point(size=.7) +
  geom_line()+
  facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
  labs(y = expression("P"[p]),
       x = 'Hora',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

data_cor_hora |> 
  filter(temporada == '2023-2024') |>
  mutate(hora = as.numeric(substr(hora,1,2))) |> 
  pivot_longer(cols = c('turgor','turgor_relleno','potencial','t_media'),
               names_to = 'tipo',values_to = 'valor') |>
  group_by(temporada,sitio,codigo,fecha,tipo) |> 
  mutate(valor = as.numeric(scale(valor))) |> 
  ggplot(aes(hora,valor,color=tipo)) +
  geom_point(size=.7) +
  geom_line()+
  facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
  labs(y = expression("P"[p]),
       x = 'Hora',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

data |> 
  filter(temporada == '2022-2023',
         sitio == 'rio_claro',
         tratamiento == 'T2',
         unidad == 1,
         month(fecha) == 1) |> 
  pivot_longer(cols = c('turgor_filtrado','t_media','turgor_relleno'),
               names_to = 'tipo',values_to = 'valor') |>
  group_by(tipo) |> 
  mutate(valor = as.numeric(scale(valor))) |>
  ggplot(aes(fecha_hora_f(fecha,hora),valor,color=tipo))+ 
  geom_line()

# correlación

data_cor_hora

# La correlación con datos filtrados solo es buena a nivel de unidad, porque después
# considera distintas magnitudes en el mismos subset y provoca baja correlación

data_cor_hora |> 
  filter(temporada == '2022-2023') |> 
  ggplot(aes(turgor_relleno,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor modelado (est)',
       y = 'Potencial (MPa)')

data_cor_hora |> 
  filter(temporada == '2023-2024') |> 
  ggplot(aes(turgor_relleno,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor modelado (est)',
       y = 'Potencial (MPa)')



























data_cor_dia_t <- data_cor_dia |>  
  group_by(across(sitio:tratamiento)) |> 
  summarise(turgor_filtrado = mean(turgor_filtrado,na.rm=T),
            turgor_relleno = mean(turgor_relleno,na.rm=T),
            potencial = mean(potencial,na.rm=T),
            t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup()

data_cor_dia_t |> 
  filter(temporada=='2022-2023') |> 
  ggplot(aes(fecha,turgor_relleno)) +
  geom_point()+
  facet_grid(tratamiento~sitio)


data_cor_dia |> 
  filter(temporada == '2022-2023') |> 
  ggplot(aes(fecha,turgor_relleno,color=zim,group=zim)) +
  geom_point() +
  #geom_point(aes(fecha,potencial_sc), color = 'red',size = 1) +
  facet_grid(tratamiento+unidad~sitio)

data_cor_dia |> 
  filter(temporada == '2022-2023',
         sitio == 'la_esperanza',
         tratamiento == 'T1',
         unidad == 1) |> 
  group_by(sitio,temporada,sensor) |> 
  mutate(turgor_sc = as.numeric(scale(turgor_relleno))) |> 
  ggplot(aes(fecha,turgor_sc,color=zim)) +
  geom_point() +
  facet_grid(tratamiento~unidad)



data_cor_dia |> 
  filter(temporada == '2022-2023',
         sitio == 'la_esperanza') |> 
  group_by(sitio,temporada,sensor) |> 
  mutate(turgor_sc = as.numeric(scale(turgor_filtrado)),
         potencial_sc = as.numeric(scale(potencial))) |> 
  ungroup() |> 
  ggplot(aes(turgor_sc,potencial_sc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  facet_grid(zim~tratamiento+unidad)





# data diaria

data_dia <- data_cor |> 
  filter(between(as.POSIXct(hora, format = "%H:%M"),
                 as.POSIXct('13:00', format = "%H:%M"),
                 as.POSIXct('14:00', format = "%H:%M"))) |> 
  group_by(sitio,temporada,fecha,tratamiento,unidad,codigo) |> 
  summarise(turgor = mean(turgor,na.rm=T),
            turgor_sc = mean(turgor_sc,na.rm=T),
            potencial = mean(potencial,na.rm=T),
            potencial_sc = mean(potencial_sc,na.rm=T),
            t = mean(t_media,na.rm=T),
            t_sc = mean(t_sc,na.rm=T),
            t_sc_dia = mean(t_sc_dia,na.rm=T),
            vpd = mean(vpd_media,na.rm=T),
            vpd_sc = mean(vpd_sc,na.rm=T),
            vpd_sc_dia = mean(vpd_sc_dia,na.rm=T)) |> 
  ungroup() |> 
  mutate(fecha_hora = as.POSIXct(paste(fecha,'13:30'),format='%Y-%m-%d %H:%M'),
         .before = tratamiento) |> 
  filter()

# data horaria

data_hora <- data_cor |> 
  filter(paste(sitio,fecha) %in% unique(paste(data_potencial_hora$sitio,data_potencial_hora$fecha)),
         between(as.POSIXct(hora, format = "%H:%M"),
                 as.POSIXct('07:00', format = "%H:%M"),
                 as.POSIXct('22:00', format = "%H:%M"))) |> 
  mutate(fecha_hora = as.POSIXct(paste(fecha,hora),format='%Y-%m-%d %H:%M'),
         .before = tratamiento) |> 
  filter(!(temporada == '2023-2024' & hora == '13:30'))

# Serie turgor dia

data_dia_2022 <- data_dia |> 
  filter(temporada == '2022-2023')

data_dia_2023 <- data_dia |> 
  filter(temporada == '2023-2024')

data_dia_2022 |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_dia_2022,aes(fecha_hora,turgor_sc, color = 'Turgor'), size = .7) +
  geom_line(data = data_dia_2022,aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_dia_2023 |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_dia_2023,aes(fecha_hora,turgor_sc, color = 'Turgor'), size = .7) +
  geom_line(data = data_dia_2023,aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

# Serie diaria tratamiento

data_dia_2022 <- data_dia |> 
  filter(temporada == '2022-2023') |> 
  group_by(sitio,temporada,fecha,fecha_hora,tratamiento) |> 
  summarise(potencial_sc = mean(potencial_sc,na.rm=T),
            turgor_sc = mean(turgor_sc,na.rm=T)) |> 
  ungroup()

data_dia_2023 <- data_dia |> 
  filter(temporada == '2023-2024') |> 
  group_by(sitio,temporada,fecha,fecha_hora,tratamiento) |> 
  summarise(potencial_sc = mean(potencial_sc,na.rm=T),
            turgor_sc = mean(turgor_sc,na.rm=T)) |> 
  ungroup()
  
data_dia_2022 |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_dia_2022,aes(fecha_hora,turgor_sc, color = 'Turgor'), size = .7) +
  geom_line(data = data_dia_2022,aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_dia_2023 |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_dia_2023,aes(fecha_hora,turgor_sc, color = 'Turgor'), size = .7) +
  geom_line(data = data_dia_2023,aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

# Serie horaria

data_hora |> 
  filter(temporada == '2022-2023') |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = 1) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(aes(fecha_hora,turgor_sc, color = 'Turgor'), size = 1) +
  geom_line(aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Hora',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_hora |> 
  filter(temporada == '2023-2024') |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = 1) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(aes(fecha_hora,turgor_sc, color = 'Turgor'), size = 1) +
  geom_line(aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Hora',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

# Scatterplot dia

data_dia |> 
  ggplot(aes(turgor_sc,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor',
       y = 'Potencial') +
  theme_light()  +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names))

data_dia |> 
  ggplot(aes(turgor_sc,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor',
       y = 'Potencial') +
  theme_light()

# Scatterplot hora

data_hora |> 
  ggplot(aes(turgor_sc,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor',
       y = 'Potencial') +
  theme_light()  +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names))

data_hora |> 
  ggplot(aes(turgor_sc,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor',
       y = 'Potencial') +
  theme_light() 

# Modelo en función de datos horarios

ms <- lm(potencial~ turgor_sc, data = data_hora)

data_ms_dia <- data_dia |> 
  mutate(potencial_mod = ms$coefficients[[2]]*turgor_sc+ms$coefficients[[1]]) 

data_ms_hora <- data_hora |> 
  mutate(potencial_mod = ms$coefficients[[2]]*turgor_sc+ms$coefficients[[1]])

# Graficar serie modelada diaria

data_ms_dia_2022 <- data_ms_dia |> 
  filter(temporada == '2022-2023')

data_ms_dia_2023 <- data_ms_dia |> 
  filter(temporada == '2023-2024')

data_ms_dia_2022 |> 
  filter(!is.na(potencial)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_ms_dia_2022,aes(fecha_hora,potencial_mod, color = 'Potencial modelado'), size = .7) +
  geom_line(data = data_ms_dia_2022,aes(fecha_hora,potencial_mod, color = 'Potencial modelado')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_ms_dia_2023 |> 
  filter(!is.na(potencial)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_ms_dia_2023,aes(fecha_hora,potencial_mod, color = 'Potencial modelado'), size = .7) +
  geom_line(data = data_ms_dia_2023,aes(fecha_hora,potencial_mod, color = 'Potencial modelado')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_ms_dia |> 
  na.omit() |> 
  filter(temporada == '2022-2023') |> 
  

# Graficar serie modelada horaria

data_ms_hora |>
  mutate(fecha = case_when(temporada == '2023-2024' ~ as.POSIXct(fecha)-years(1))) |> 
  ggplot(aes(hora,potencial)) +
  geom_point(aes(color = as.factor('Potencial')), size = 1.2) +
  geom_point(aes(hora,turgor_sc, color = as.factor('Turgor estandarizado')), size = 1.2) +
  geom_point(aes(hora, potencial_mod, color = as.factor('Potencial modelado')), size = 1.2, alpha = .5) +
  labs(x = 'Hora',
       y = 'Potencial',
       color = '') +
  theme_light() +
  scale_x_discrete(breaks = paste0(sprintf('%02d',seq(0, 24, by = 3)),':00'),
                   labels = seq(0, 24, by = 3)) +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names)) +
  theme(text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 13)),
        axis.title.x = element_text(margin = margin(t = 13)))

# Sctarreplot potencial medido vs modelado


# Modelo múltiple

mm <- lm(potencial~ turgor_sc + t_media + vpd_media, data = data_hora)

data_mm_dia <- data_dia |> 
  mutate(potencial_mod = mm$coefficients[[1]]+turgor_sc*mm$coefficients[[2]]+
                                              t*mm$coefficients[[3]]+
                                              vpd*mm$coefficients[[4]]) 
data_mm_hora <- data_hora |> 
  mutate(potencial_mod = mm$coefficients[[1]]+turgor_sc*mm$coefficients[[2]]+
           t_media*mm$coefficients[[3]]+
           vpd_media*mm$coefficients[[4]])

# Graficar serie modelada diaria

data_mm_dia_2022 <- data_mm_dia |> 
  filter(temporada == '2022-2023')

data_mm_dia_2023 <- data_mm_dia |> 
  filter(temporada == '2023-2024')

data_mm_dia_2022 |> 
  filter(!is.na(potencial)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_mm_dia_2022,aes(fecha_hora,potencial_mod, color = 'Potencial modelado'), size = .7) +
  geom_line(data = data_mm_dia_2022,aes(fecha_hora,potencial_mod, color = 'Potencial modelado')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_mm_dia_2023 |> 
  filter(!is.na(potencial)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_mm_dia_2023,aes(fecha_hora,potencial_mod, color = 'Potencial modelado'), size = .7) +
  geom_line(data = data_mm_dia_2023,aes(fecha_hora,potencial_mod, color = 'Potencial modelado')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

# Graficar serie modelada horaria

data_mm_hora |>
  mutate(fecha = case_when(temporada == '2023-2024' ~ as.POSIXct(fecha)-years(1))) |> 
  ggplot(aes(hora,potencial)) +
  geom_point(aes(color = as.factor('Potencial')), size = 1.2) +
  geom_point(aes(hora,turgor_sc, color = as.factor('Turgor estandarizado')), size = 1.2) +
  geom_point(aes(hora, potencial_mod, color = as.factor('Potencial modelado')), size = 1.2, alpha = .5) +
  labs(x = 'Hora',
       y = 'Potencial',
       color = '') +
  theme_light() +
  scale_x_discrete(breaks = paste0(sprintf('%02d',seq(0, 24, by = 3)),':00'),
                   labels = seq(0, 24, by = 3)) +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names)) +
  theme(text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 13)),
        axis.title.x = element_text(margin = margin(t = 13)))

# Graficar serie modelada horaria

data_modelo |> 
  mutate(fecha_hora = as.POSIXct(paste(fecha,hora),format='%Y-%m-%d %H:%M'),
         .before=tratamiento) |> 
  left_join(fechas,by=c('sitio','fecha')) |> 
  filter(is.na(periodo)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point() +
  geom_line() +
  facet_grid(tratamiento+unidad~temporada,scales = 'free_x')

# Scatterplot

data_modelo |> 
  left_join(fechas,by=c('sitio','fecha')) |> 
  filter(is.na(periodo)) |> 
  ggplot(aes(potencial_mod,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Potencial modelado',
       y = 'Potencial observado') +
  theme_light()
  #facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names))
  
# data_modelo |> 
#   na.omit() |> 
#   select(potencial,potencial_pred) |> 
#   write_csv('C:/Hemera/garces/prueba_2.csv')
    
data_modelo |> 
  ggplot(aes(potencial,potencial_pred)) +
  geom_point() +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Potencial observado',
       y = 'Potencial modelado') +
  theme_light()

data_modelo |> 
  ggplot(aes(potencial,potencial_pred)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Potencial observado',
       y = 'Potencial modelado') +
  theme_light() +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names))

data_modelo |> 
  ggplot(aes(potencial,potencial_pred)) +
  geom_point() +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Potencial observado',
       y = 'Potencial modelado') +
  theme_light()

