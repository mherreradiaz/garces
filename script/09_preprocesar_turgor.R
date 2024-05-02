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
  distinct(sitio,fecha,hora, .keep_all=T)

data_lag_ordered |> 
  mutate(fecha_hora = fecha_hora_f(fecha,hora)) |> 
  group_by(sitio,fecha_hora = floor_date(fecha_hora,'1 hour')) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup() |> 
  mutate(temporada = ifelse(fecha_hora < '2023-06-01','2022-2023','2023-2024'),
         fecha = fecha_f(fecha_hora),
         hora = hora_f(fecha_hora),
         .before = fecha_hora) |> 
  select(-fecha_hora) |> 
  distinct(sitio,temporada,fecha,hora,.keep_all = T) |> 
  write_rds('data/data_processed/clima_lag.rds')

pca <- function(x1,x2,x3) {
  df <- data.frame(x1,x2,x3)
  pca_result <- predict(prcomp(df,scale. = T))
  return(data.frame(pca_result))
}
  
data_filter <- data_turgor |> 
  left_join(data_lag_ordered,by=c('sitio','fecha','hora')) |> 
  group_by(sitio,fecha,sensor) |> 
  summarise(cor_t = cor(turgor,t_media,use='na.or.complete'),
            cor_rh = cor(turgor,rh_media,use='na.or.complete'),
            dif = max(abs(diff(as.numeric(scale(turgor)))),na.rm=T)) |> 
  ungroup() |> 
  rowwise() |> 
  mutate(dif = ifelse(is.infinite(dif),NA,dif),
         filter_dif = ifelse(dif < 1,1,0),
         filter_cor_1 = ifelse(cor_t > 0 & cor_rh < 0,1,0),
         min_cor = min(abs(c(cor_t,cor_rh)),na.rm=T),
         min_cor = ifelse(is.infinite(min_cor),NA,min_cor),
         filter_cor_2 = ifelse(min_cor >= .5,1,0),
         filter = ifelse((filter_dif+filter_cor_1+filter_cor_2)==3,1,0)) |> 
  select(sitio,fecha,sensor,filter)

fechas <- data_turgor |> 
  filter(!is.na(turgor)) |> 
  group_by(temporada,sitio) |>
  summarise(fechas_min = as.character(as.Date(min(fecha,na.rm=T))-days(3)),
            fechas_max = as.character(as.Date(max(fecha,na.rm=T))+days(3)))

data_pca <- data_lag_ordered |> 
  mutate(temporada = ifelse(fecha < '2023-06-01','2022-2023','2023-2024'),
         .before = fecha) |> 
  na.omit() |> 
  group_by(sitio,temporada) |> 
  mutate(pca(t_media,rh_media,vpd_medio)) |> 
  ungroup() |> 
  select(-PC3)

data_turgor_filtrado <- data_turgor |> 
  left_join(data_filter,by=c('sitio','fecha','sensor')) |> 
  mutate(turgor_filtrado = ifelse(filter==1,turgor,NA)) |> 
  select(-filter) |> 
  left_join(fechas,by=c('temporada','sitio')) |> 
  filter(between(fecha,fechas_min,fechas_max)) |> 
  select(-fechas_min,-fechas_max) |> 
  left_join(data_pca, by = c('sitio','temporada','fecha','hora'))

data_cluster <- data_turgor_filtrado |> 
  group_by(sitio,temporada,fecha,sensor) |> 
  summarise(mediana = median(turgor_filtrado,na.rm=T),
            max = max(turgor_filtrado,na.rm=T),
            min = min(turgor_filtrado,na.rm=T)) |> 
  ungroup() |> 
  mutate(amplitud = ifelse(is.infinite(max),NA,max)-ifelse(is.infinite(min),NA,min),
         dia_n = as.numeric(as.Date(fecha) - as.Date('2022-09-01'))) |> 
  na.omit() |> 
  group_by(sitio,temporada,sensor) |> 
  mutate(mediana = as.numeric(scale(mediana)),
         amplitud = as.numeric(scale(amplitud)),
         dia_n = as.numeric(scale(dia_n))) |> 
  ungroup() |> 
  group_by(sitio,temporada,sensor) |> 
  mutate(cluster = dbscan(data.frame(mediana,amplitud), eps = 0.5, minPts = 5)$cluster) |> 
  ungroup() |>
  filter(cluster != 0) |> 
  select(sitio:sensor,cluster)

data_turgor_cluster <- data_turgor_filtrado |> 
  left_join(data_cluster,by=c('sitio','temporada','fecha','sensor'))
  
data_coef <- data_turgor_cluster |> 
  na.omit() |> 
  group_by(sitio,temporada,sensor,cluster) |> 
  summarise(lm_m1 = coeficientes(PC1,PC2,turgor_filtrado)$m1,
            lm_m2 = coeficientes(PC1,PC2,turgor_filtrado)$m2,
            lm_int = coeficientes(PC1,PC2,turgor_filtrado)$int,
            lm_r = coeficientes(PC1,PC2,turgor_filtrado)$r) |> 
  ungroup() |> 
  filter(lm_r>.4) |>
  select(-lm_r) |> 
  pivot_wider(names_from = cluster, values_from = c(lm_m1,lm_m2, lm_int), names_glue = "{.value}_{cluster}")

data_turgor_relleno <- data_turgor_cluster |> 
  left_join(data_coef,by=c('sitio','temporada','sensor')) |> 
  rowwise() |> 
  mutate(turgor_int_1 = PC1*lm_m1_1+PC2*lm_m2_1+lm_int_1,
         turgor_int_2 = PC1*lm_m1_2+PC2*lm_m2_2+lm_int_2,
         turgor_int_3 = PC1*lm_m1_3+PC2*lm_m2_3+lm_int_3) |> 
  group_by(sitio,temporada,sensor) |> 
  mutate(turgor_int_1_sc = as.numeric(scale(turgor_int_1)),
         turgor_int_2_sc = as.numeric(scale(turgor_int_2)),
         turgor_int_3_sc = as.numeric(scale(turgor_int_3))) |> 
  ungroup() |> 
  rowwise() |> 
  mutate(turgor_relleno = mean(c(turgor_int_1_sc,turgor_int_2_sc,turgor_int_3_sc),na.rm=T)) |> 
  select(sitio:turgor_filtrado,turgor_relleno)

data_turgor_relleno |> 
  mutate(datetime = fecha_hora_f(fecha,hora)) |> 
  group_by(sitio,temporada,datetime = floor_date(datetime,'1 hour'),tratamiento,unidad,codigo,zim,sensor) |> 
  summarise(turgor = mean(turgor,na.rm=T),
            turgor_filtrado = mean(turgor_filtrado,na.rm=T),
            turgor_relleno = mean(turgor_relleno,na.rm=T)) |> 
  ungroup() |> 
  mutate(fecha = fecha_f(datetime),
         hora = hora_f(datetime),
         .before = datetime) |> 
  select(-datetime) |> 
  distinct(sitio,temporada,fecha,hora,sensor,.keep_all=T) |> 
  write_rds('data/data_processed/turgor.rds')

read_rds('data/data_processed/turgor.rds') |> View()

data_turgor |> 
  filter(temporada == '2022-2023',
         month(fecha) == 11) |> 
  mutate(fecha_hora = fecha_hora_f(fecha,hora)) |>
  ggplot(aes(fecha_hora,turgor_relleno,color = zim)) + 
  geom_point(size = .5) +
  facet_grid(tratamiento+unidad~sitio)
