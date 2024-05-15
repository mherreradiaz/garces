source('script/funciones/paquetes.R')

# leer datos mediahora

data_turgor <- read_rds('data/processed/turgor_mediahora.rds')
data_clima <- read_rds('data/processed/clima_mediahora.rds')

# corregir lag del clima

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
  write_rds('data/processed/clima_lag.rds')

data_pca <- data_lag_ordered |> 
  mutate(temporada = ifelse(fecha < '2023-06-01','2022-2023','2023-2024'),
         .before = fecha) |> 
  na.omit() |> 
  group_by(sitio,temporada) |> 
  mutate(pca(t_media,rh_media,vpd_medio)) |> 
  ungroup() |> 
  select(-PC3)

data_pca |> 
  mutate(fecha_hora = fecha_hora_f(fecha,hora)) |> 
  group_by(sitio,fecha_hora = floor_date(fecha_hora,'1 hour')) |> 
  summarise(PC1 = mean(PC1,na.rm=T),
            PC2 = mean(PC2,na.rm=T)) |> 
  ungroup() |> 
  mutate(temporada = ifelse(fecha_hora < '2023-06-01','2022-2023','2023-2024'),
         fecha = fecha_f(fecha_hora),
         hora = hora_f(fecha_hora),
         .before = fecha_hora) |> 
  select(-fecha_hora) |> 
  distinct(sitio,temporada,fecha,hora,.keep_all = T) |> 
  write_rds('data/processed/clima_pca.rds')

# filtrar dias de turgor
  
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

data_turgor_filtrado <- data_turgor |> 
  left_join(data_filter,by=c('sitio','fecha','sensor')) |> 
  mutate(turgor_filtrado = ifelse(filter==1,turgor,NA)) |> 
  select(-filter) |> 
  left_join(fechas,by=c('temporada','sitio')) |> 
  filter(between(fecha,fechas_min,fechas_max)) |> 
  select(-fechas_min,-fechas_max)

# clusterizar datos de turgor

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

# calcular coeficientes 
  
data_coef <- data_turgor_filtrado |> 
  left_join(data_cluster,by=c('sitio','temporada','fecha','sensor')) |> 
  left_join(data_pca, by = c('sitio','temporada','fecha','hora')) |> 
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

data_turgor_modelado <- data_turgor_filtrado |> 
  left_join(data_cluster,by=c('sitio','temporada','fecha','sensor')) |> 
  left_join(data_coef,by=c('sitio','temporada','sensor')) |> 
  left_join(data_pca,by=c('sitio','temporada','fecha','hora')) |> 
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
  mutate(turgor_modelado = mean(c(turgor_int_1,turgor_int_2,turgor_int_3),na.rm=T),
         turgor_modelado_sc = mean(c(turgor_int_1_sc,turgor_int_2_sc,turgor_int_3_sc),na.rm=T)) |> 
  select(sitio:turgor_filtrado,turgor_modelado,turgor_modelado_sc)

write_rds(data_turgor_modelado,'data/processed/turgor_procesado_mediahora.rds')

data_turgor_modelado |> 
  mutate(datetime = fecha_hora_f(fecha,hora)) |> 
  group_by(sitio,temporada,datetime = floor_date(datetime,'1 hour'),tratamiento,unidad,codigo,zim,sensor) |> 
  summarise(turgor = mean(turgor,na.rm=T),
            turgor_filtrado = mean(turgor_filtrado,na.rm=T),
            turgor_modelado = mean(turgor_modelado,na.rm=T),
            turgor_modelado_sc = mean(turgor_modelado_sc,na.rm=T)) |> 
  ungroup() |> 
  mutate(fecha = fecha_f(datetime),
         hora = hora_f(datetime),
         .before = datetime) |> 
  select(-datetime) |> 
  distinct(sitio,temporada,fecha,hora,sensor,.keep_all=T) |> 
  write_rds('data/processed/turgor_procesado.rds')
