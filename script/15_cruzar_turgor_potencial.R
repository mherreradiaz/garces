source('script/funciones/paquetes.R')

# leer datos y unificar

data_turgor <- read_rds('data/data_processed/turgor_procesado.rds')
data_clima <- read_rds('data/data_processed/clima_lag.rds')

data <- data_turgor |> 
  left_join(data_clima, by = c('sitio','temporada','fecha','hora')) |> 
  group_by(across(sitio:codigo)) |> 
  summarise(turgor = mean(turgor,na.rm=T),
            turgor_filtrado = mean(turgor_filtrado,na.rm=T),
            turgor_modelado = mean(turgor_modelado,na.rm=T),
            turgor_modelado_sc = mean(turgor_modelado_sc,na.rm=T),
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
         turgor_modelado = mean(turgor_modelado,na.rm=T),
         turgor_modelado_sc = mean(turgor_modelado_sc,na.rm=T),
         t_media = mean(t_media,na.rm=T),
         rh_media = mean(rh_media,na.rm=T),
         vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup() |> 
  left_join(data_potencial_dia,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) |> 
  select(sitio:turgor_filtrado,turgor_modelado,turgor_modelado_sc,potencial,everything())

data_potencial_hora <- read_rds('data/data_processed/potencial_horario.rds') |> 
  rename(potencial = bar)

data_cor_hora <- data |> 
  filter(paste(sitio,fecha) %in% unique(paste(data_potencial_hora$sitio,data_potencial_hora$fecha))) |> 
  left_join(data_potencial_hora,by=c('sitio','temporada','fecha','hora','tratamiento','unidad','codigo')) |> 
  select(sitio:turgor_modelado,turgor_modelado_sc,potencial,everything())

write_rds(data_cor_dia,'data/data_processed/turgor_potencial_dia.rds')
write_rds(data_cor_hora,'data/data_processed/turgor_potencial_hora.rds')

# visualización serie 

data |>
  mutate(fecha_hora = fecha_hora_f(fecha,hora)) |>
  filter(temporada == '2022-2023',
         fecha == '2023-01-13') |>
  ggplot(aes(fecha_hora,turgor_modelado)) +
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
  pivot_longer(cols = c('turgor','turgor_filtrado','turgor_modelado'),
               names_to = 'turgor',values_to = 'valor') |>
  ggplot(aes(fecha_hora,valor,color=turgor)) +
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
  scale_color_hue(labels = c("Original", "Filtrado",'modelado')) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(y = expression("P"[p]),
       x = 'Fecha',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

data |>
  mutate(fecha_hora = fecha_hora_f(fecha,hora)) |>
  filter(temporada == '2023-2024') |>
  pivot_longer(cols = c('turgor','turgor_filtrado','turgor_modelado'),
               names_to = 'turgor',values_to = 'valor') |>
  ggplot(aes(fecha_hora,valor,color=turgor)) +
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio,labeller = as_labeller(names)) +
  scale_color_hue(labels = c("Original", "Filtrado",'modelado')) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(y = expression("P"[p]),
       x = 'Fecha',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

# visualización serie valor único diario

data_cor_dia |>
  mutate(fecha = as.Date(fecha)) |> 
  filter(temporada == '2022-2023') |>
  pivot_longer(cols = c('turgor','turgor_filtrado','turgor_modelado'),
               names_to = 'turgor',values_to = 'valor') |>
  ggplot(aes(fecha,valor,color=turgor)) +
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
  scale_color_hue(labels = c("Original", "Filtrado",'modelado')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(y = expression("P"[p]),
       x = 'Fecha',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

data_cor_dia |>
  mutate(fecha = as.Date(fecha)) |> 
  filter(temporada == '2023-2024') |>
  pivot_longer(cols = c('turgor','turgor_filtrado','turgor_modelado'),
               names_to = 'turgor',values_to = 'valor') |>
  ggplot(aes(fecha,valor,color=turgor)) +
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
  scale_color_hue(labels = c("Original", "Filtrado",'modelado')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(y = expression("P"[p]),
       x = 'Fecha',
       color = 'Turgor') +
  guides(color = guide_legend(override.aes = list(size = 3)))

# visualización serie ciclo diario

data_cor_hora |> 
  filter(temporada == '2022-2023') |>
  mutate(hora = as.numeric(substr(hora,1,2))) |> 
  pivot_longer(cols = c('turgor','turgor_modelado','potencial','t_media'),
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
  pivot_longer(cols = c('turgor','turgor_modelado','potencial','t_media'),
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
  pivot_longer(cols = c('turgor_filtrado','t_media','turgor_modelado'),
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
  ggplot(aes(turgor_modelado,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor modelado (est)',
       y = 'Potencial (MPa)')

data_cor_hora |> 
  filter(temporada == '2023-2024') |> 
  ggplot(aes(turgor_modelado,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor modelado (est)',
       y = 'Potencial (MPa)')

