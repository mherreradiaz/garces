source('script/funciones/read_yara.R')
source('script/funciones/paquetes.R')
options(timeout = max(3600, getOption("timeout")))

# descargar datos yara y clima del último mes

metadata <- read_csv2('data/metadata/metadata_yara.csv')
codigo_tur_2022 <- read_csv('data/metadata/codigos_zim_turgor_2022.csv')
codigo_tur_2023 <- read_csv2('data/metadata/codigos_zim_turgor.csv')

data_codigo_tur_2022 <- left_join(codigo_tur_2022,metadata,by=c('sensor' = 'identificador'))
data_codigo_tur_2023 <- left_join(codigo_tur_2023,metadata,by=c('sensor' = 'identificador'))

device_id_tur_2022 <- data_codigo_tur_2022 |> 
  filter(grepl('Yara Water-Sensor',tipo)) |> 
  pull(device_id)

device_id_tur_2023 <- data_codigo_tur_2023 |> 
  filter(grepl('Yara Water-Sensor',tipo)) |> 
  pull(device_id)

año <- year(now())
mes <- month(now())

fechas <- dia(año,mes) 

dest <- paste0('data/data_raw/turgor/turgor_',
               año,'_',sprintf('%02d',mes),'.csv')

read_yara(si(fechas[1] < '2023-06-01',device_id_tur_2022,device_id_tur_2023),
          from_date = fechas[1],
          until_date = fechas[2],
          dest_file = dest)

# descargar datos clima

año <- year(now())
mes <- month(now())
  
fechas <- dia(año,mes) 

data_clima_le <- clima('00205018',c('Temperature','VPD','Eto','Precipitation','Humidity'),fechas) |> 
  mutate(sitio = 'la_esperanza',
         .before = datetime)
data_clima_rc <- clima('00203E6E',c('Temperature','VPD','Eto','Precipitation','Humidity'),fechas) |> 
  mutate(sitio = 'rio_claro',
         .before = datetime)

data_clima <- bind_rows(data_clima_le,data_clima_rc)

write_rds(data_clima,paste0('data/data_raw/clima/clima_',año,'_',sprintf('%02d',mes),'.rds'))

# procesar y combinar datos brutos de turgor y clima

files_turgor <- list.files('data/data_raw/turgor/',full.names = T)

data_turgor_new <- list()

for (a in 1:length(files_turgor)) {
  
  año <- gsub(".*turgor_(\\d{4}).*","\\1", files_turgor[a])
  mes <- gsub(".*_([0-9]{2}).*","\\1", files_turgor[a])
  
  fecha_actual <- as.Date(paste(año,mes,'01',sep = '-'))
  
  data_turgor_new_x <- read_delim(files_turgor[a],delim = '\t') |> 
    select(1,contains('Yara')) |> 
    mutate(across(where(is.character),as.numeric)) |> 
    pivot_longer(-1,names_to = 'sensor') |> 
    rename_with(.fn = \(x) 'datetime',.cols = starts_with('Times')) |>
    mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |> 
    mutate(sensor =str_extract(str_extract(sensor,'[0-9]{4}\\['),'[0-9]{4}')) |> 
    mutate(sensor = as.numeric(sensor)) 
  
  dif_max <- data_turgor_new_x |> 
    group_by(sensor,fecha = floor_date(datetime,'5 minutes')) |>
    summarise(value = mean(value,na.rm = T)) |> 
    ungroup() |> 
    arrange(sensor,fecha) |> 
    group_by(sensor,fecha = fecha_f(fecha)) |> 
    summarise(dif = max(abs(diff(value)),na.rm=T)) |> 
    ungroup() |> 
    mutate(dif = ifelse(is.infinite(dif),NA,dif))
    
  data_turgor_new[[a]] <- data_turgor_new_x |> 
    group_by(sensor,fecha = floor_date(datetime,'30 minutes')) |> 
    summarise(value = mean(value,na.rm = T)) |> 
    ungroup() |> 
    left_join(si(fecha_actual < '2023-06-01',codigo_tur_2022,codigo_tur_2023),by='sensor') |> 
    separate(codigo,2,into =c('tratamiento','codigo')) |> 
    rename(turgor = value) |>
    mutate(temporada = si(fecha_actual < '2023-06-01','2022-2023','2023-2024'),
           unidad = factor(unidad, levels = 1:3),
           hora = format(as.POSIXct(fecha), format = "%H:%M"),
           zim = substr(codigo,nchar(codigo)-1,nchar(codigo)),
           codigo = substr(codigo,1,nchar(codigo)-2),
           fecha = format(as.POSIXct(fecha), format = "%Y-%m-%d")) |>
    select(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,zim,sensor,turgor) |>
    arrange(fecha,sitio,hora,tratamiento,unidad,zim) |>
    ungroup() 
    # left_join(dif_max,by = c('fecha','sensor')) |> 
    # filter(dif < 8) |> 
    # select(-dif) |> 
    # ggplot(aes(fecha_hora_f(fecha,hora),turgor, color = zim)) +
    # geom_point(size = .7) +
    # facet_grid(tratamiento+unidad~sitio, scales = 'fixed')
}

data_turgor <- bind_rows(data_turgor_new) |>
  distinct(sitio,fecha,hora,sensor,.keep_all = T) |>
  arrange(fecha,hora,sitio,tratamiento,unidad,zim) 

files_clima <- list.files('data/data_raw/clima/',full.names = T)

data_clima <- bind_rows(lapply(files_clima,read_rds)) |> 
  distinct(sitio,datetime, .keep_all=T) |> 
  mutate(temporada = ifelse(datetime<'2023-06-01','2022-2023','2023-2024'),
         .before = datetime) |> 
  arrange(datetime,sitio)

data_clima |> 
  group_by(sitio,temporada,datetime = floor_date(datetime,'1 hour')) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            t_max = max(t_max,na.rm=T),
            t_min = min(t_min,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T),
            vpd_min = min(vpd_min,na.rm=T),
            eto = sum(eto,na.rm=T),
            pp = sum(pp,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            rh_max = max(rh_max,na.rmn=T),
            rh_min = min(rh_min,na.rm=T)) |> 
  ungroup() |> 
  mutate(fecha = fecha_f(datetime),
         hora = hora_f(datetime),
         .before = datetime) |> 
  select(-datetime) |> 
  write_rds('data/data_processed/clima.rds')

data_clima <- data_clima |> 
  mutate(fecha = fecha_f(datetime),
         hora = hora_f(datetime),
         .before = datetime) |> 
  select(-datetime)

data_turgor_30 <- data_turgor
data_clima_30 <- data_clima

# Preprocesado

data_ccf <- data_turgor |> 
  left_join(data_clima,by=c('sitio','temporada','fecha','hora')) |> 
  group_by(sitio,fecha,sensor) |> 
  mutate(turgor_sc = as.numeric(scale(turgor)),
         t_sc = as.numeric(scale(t_media)),
         rh_sc = as.numeric(scale(100-rh_media)),
         vpd_sc = as.numeric(scale(log(vpd_medio+1)))) |> 
  ungroup() |>  
  na.omit() |> 
  group_by(sitio,temporada,sensor) |> 
  summarise(t_lag = ccf(as.numeric(t_sc), as.numeric(turgor_sc), plot = F)$lag[
    which.max(as.numeric(ccf(as.numeric(t_sc), as.numeric(turgor_sc), plot = F)$acf))],
    rh_lag = ccf(as.numeric(rh_sc), as.numeric(turgor_sc), plot = F)$lag[
      which.max(as.numeric(ccf(as.numeric(rh_sc), as.numeric(turgor_sc), plot = F)$acf))],
    vpd_lag = ccf(as.numeric(vpd_sc), as.numeric(turgor_sc), plot = F)$lag[
    which.max(as.numeric(ccf(as.numeric(vpd_sc), as.numeric(turgor_sc), plot = F)$acf))]) |> 
  ungroup() |> 
  group_by(sitio,temporada) |> 
  summarise(t_lag = modal(t_lag),
            rh_lag = modal(rh_lag),
            vpd_lag = modal(vpd_lag)) |> 
  ungroup()

data_lag_disordered <- data_clima |> 
  left_join(data_ccf,by=c('sitio','temporada')) |>
  mutate(datetime_t = fecha_hora_f(fecha,hora)-(30*60)*t_lag,
         datetime_rh = fecha_hora_f(fecha,hora)-(30*60)*rh_lag,
         datetime_vpd = fecha_hora_f(fecha,hora)-(30*60)*vpd_lag)

data_lag_t <- data_lag_disordered |> select(sitio,datetime_t,t_media) |> 
  mutate(fecha = fecha_f(datetime_t),hora = hora_f(datetime_t)) |> select(-datetime_t) 
data_lag_rh <- data_lag_disordered |> select(sitio,datetime_rh,rh_media) |> 
  mutate(fecha = fecha_f(datetime_rh),hora = hora_f(datetime_rh)) |> select(-datetime_rh) 
data_lag_vpd <- data_lag_disordered |> select(sitio,datetime_vpd,vpd_medio) |> 
  mutate(fecha = fecha_f(datetime_vpd),hora = hora_f(datetime_vpd)) |> select(-datetime_vpd) 

data_lag_ordered <- data_lag_vpd |>
  left_join(data_lag_rh,by=c('sitio','fecha','hora'),relationship = "many-to-many") |> 
  left_join(data_lag_t,by=c('sitio','fecha','hora'),relationship = "many-to-many") |> 
  select(sitio,fecha,hora,t_media,rh_media,vpd_medio) |> 
  distinct(sitio,fecha,hora, .keep_all=T) |> 
  group_by(sitio,fecha) |> 
  mutate(t_sc = as.numeric(scale(t_media)),
         rh_sc = as.numeric(scale(100-rh_media)),
         vpd_sc = as.numeric(scale(log(vpd_medio+1)))) |> 
  ungroup()

data_lag_ordered |> 
  mutate(fecha_hora = fecha_hora_f(fecha,hora)) |> 
  group_by(sitio,fecha_hora = floor_date(fecha_hora,'1 hour')) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T),
            t_sc = mean(t_sc,na.rm=T),
            rh_sc = mean(rh_sc,na.rm=T),
            vpd_sc = mean(vpd_sc,na.rm=T)) |> 
  mutate(fecha = fecha_f(fecha_hora),
         hora = hora_f(fecha_hora),
         .before = fecha_hora) |> 
  select(-fecha_hora) |> 
  write_rds('data/data_processed/clima_lag.rds')
  
data_filter <- data_turgor |> 
  group_by(sitio,fecha,sensor) |> 
  mutate(turgor_sc = as.numeric(scale(turgor))) |> 
  ungroup() |> 
  left_join(data_lag_ordered,by=c('sitio','fecha','hora')) |> 
  group_by(sitio,fecha,sensor) |> 
  summarise(cor_index = as.numeric(cor_clima(turgor_sc,t_sc,rh_sc,vpd_sc)),
            dif = max(abs(diff(turgor_sc)),na.rm=T)) |>
  ungroup() |> 
  mutate(dif = ifelse(is.infinite(dif),NA,dif)) |> 
  mutate(filter = ifelse(cor_index > sqrt(.7) & dif < 1,1,0)) |> 
  select(sitio,fecha,sensor,filter)

data_limpia_30 <- data_turgor |> 
  left_join(data_filter,by=c('sitio','fecha','sensor')) |> 
  mutate(turgor_filtrado = ifelse(filter==1,turgor,NA)) |> 
  select(-filter)

data_limpia <- data_limpia_30 |> 
  mutate(datetime = fecha_hora_f(fecha,hora)) |> 
  group_by(sitio,temporada,datetime = floor_date(datetime,'1 hour'),tratamiento,unidad,codigo,zim,sensor) |> 
  summarise(turgor = mean(turgor,na.rm=T),
            turgor_filtrado = mean(turgor_filtrado,na.rm=T)) |> 
  ungroup() |> 
  mutate(fecha = fecha_f(datetime),
         hora = hora_f(datetime),
         .before = datetime) |> 
  select(-datetime)

write_rds(data_limpia,'data/data_processed/turgor.rds')
