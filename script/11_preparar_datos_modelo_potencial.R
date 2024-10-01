source('script/funciones/paquetes.R')

# smooth índices y biopar

data_vi_smooth <- read_rds('data/processed/sentinel_vi_smooth.rds')
data_vi_raw <- read_rds('data/processed/sentinel_vi_raw.rds')

data_vi <- data_vi_smooth |> 
  anti_join(data_vi_raw, by = c("sitio", "temporada", "fecha", "tratamiento", 
                                "unidad", "codigo")) |> 
  bind_rows(data_vi_raw) |> 
  arrange(sitio,fecha,tratamiento,unidad)

data_biopar_smooth <- read_rds('data/processed/sentinel_biopar_smooth.rds')
data_biopar_raw <- read_rds('data/processed/sentinel_biopar_raw.rds')

data_biopar <- data_biopar_smooth |> 
  anti_join(data_biopar_raw, by = c("sitio", "temporada", "fecha", "tratamiento", 
                                "unidad", "codigo")) |> 
  bind_rows(data_biopar_raw) |> 
  arrange(sitio,fecha,tratamiento,unidad)

data_sen2 <- bind_rows(data_vi,data_biopar) |> 
  distinct(across(sitio:codigo)) |> 
  left_join(data_vi) |> 
  left_join(data_biopar)

data_potencial <- read_rds('data/processed/potencial.rds')

data_clima_1 <- read_rds('data/processed/clima_hora.rds') |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(eto = max(eto,na.rm=T),
            pp = sum(pp,na.rm=T)) |> 
  ungroup()

data_clima <- read_rds('data/processed/clima_hora.rds') |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup() |> 
  left_join(data_clima_1,by=c('sitio','temporada','fecha'))
  
data <- data_sen2|>
  left_join(data_clima) |> 
  left_join(data_potencial) |> 
  arrange(temporada,fecha,sitio,tratamiento,unidad) |> 
  select(sitio:codigo,potencial_bar,everything())

write_rds(data,'data/processed/modelo_potencial_combinado.rds')

# raw índices y biopar

data_vi <- read_rds('data/processed/sentinel_vi_raw.rds') |> 
  mutate(fecha = as.Date(fecha)) |> 
  rowwise() |> 
  mutate(fecha_minus_1 = fecha - 1,
         fecha_plus_1 = fecha + 1) |> 
  ungroup() |> 
  pivot_longer(cols = c(fecha_minus_1, fecha, fecha_plus_1), 
               names_to = "tipo_fecha", values_to = "nueva_fecha") |> 
  mutate(fecha = nueva_fecha,
         .before = tratamiento) |> 
  select(-tipo_fecha,-nueva_fecha) |> 
  arrange(temporada,fecha,sitio,tratamiento,unidad)

data_biopar <- read_rds('data/processed/sentinel_biopar_raw.rds') |> 
  mutate(fecha = as.Date(fecha)) |> 
  rowwise() |> 
  mutate(fecha_minus_1 = fecha - 1,
         fecha_plus_1 = fecha + 1) |> 
  ungroup() |> 
  pivot_longer(cols = c(fecha_minus_1, fecha, fecha_plus_1), 
               names_to = "tipo_fecha", values_to = "nueva_fecha") |> 
  mutate(fecha = nueva_fecha,
         .before = tratamiento) |> 
  select(-tipo_fecha,-nueva_fecha) |> 
  arrange(temporada,fecha,sitio,tratamiento,unidad)

data_sen2 <- data_vi |> 
  left_join(data_biopar) |> 
  mutate(fecha = as.character(fecha)) |> 
  arrange(temporada,fecha,sitio,tratamiento,unidad)

data_potencial <- read_rds('data/processed/potencial.rds')

data_clima_1 <- read_rds('data/processed/clima_hora.rds') |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(eto = max(eto,na.rm=T),
            pp = sum(pp,na.rm=T)) |> 
  ungroup()

data_clima <- read_rds('data/processed/clima_hora.rds') |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup() |> 
  left_join(data_clima_1,by=c('sitio','temporada','fecha'))

data <- data_sen2|>
  left_join(data_clima) |> 
  left_join(data_potencial) |> 
  arrange(temporada,fecha,sitio,tratamiento,unidad) |> 
  select(sitio:codigo,potencial_bar,everything())

write_rds(data,'data/processed/modelo_potencial_raw.rds')

# visualizar

smooth <- read_rds('data/processed/modelo_potencial_smooth.rds')
raw <- read_rds('data/processed/modelo_potencial_raw.rds')

raw |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(cig = mean(cig,na.rm=T)) |> 
  ungroup() |> 
  ggplot(aes(fecha,cig)) +
  geom_point() +
  facet_grid(sitio~temporada,scales='free_x')
  
