source('script/funciones/paquetes.R')

data_pca <- read_rds('data/processed/clima_pca.rds')

data_pca_dia <- data_pca |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(PC1 = mean(PC1,na.rm=T),
            PC2 = mean(PC2,na.rm=T)) |> 
  ungroup()
  
data_dia <- read_rds('data/processed/turgor_potencial_dia.rds') |>
  select(sitio:codigo,potencial) |> 
  na.omit() |> 
  left_join(data_pca_dia,by=c('sitio','temporada','fecha')) |>
  mutate(hora = '13:00',
         tipo = 'dia',
         .before = tratamiento)
  
data_hora <- read_rds('data/processed/turgor_potencial_hora.rds') |>
  select(sitio:codigo,potencial) |> 
  na.omit() |> 
  left_join(data_pca,by=c('sitio','temporada','fecha','hora')) |> 
  mutate(tipo = 'hora',
         .before = tratamiento)

data_modelo <- data_dia |> 
  sample_frac(0.8, replace = FALSE) |> 
  bind_rows(data_hora |> 
              sample_frac(0.8, replace = FALSE))

data_validacion <- bind_rows(data_dia,data_hora) |> 
  anti_join(data_modelo)

data_coef <- data_modelo |> 
  select(sitio,temporada,potencial,PC1,PC2) |> 
  na.omit() |> 
  group_by(sitio,temporada) |> 
  summarise(m1 = coeficientes(PC1,PC2,potencial)$m1,
            m2 = coeficientes(PC1,PC2,potencial)$m2,
            int = coeficientes(PC1,PC2,potencial)$int,
            r = coeficientes(PC1,PC2,potencial)$r) |> 
  ungroup()

data_val_mod <- data_validacion |>
  select(sitio:tratamiento,potencial,PC1,PC2) |> 
  na.omit() |> 
  left_join(data_coef,by=c('sitio','temporada')) |> 
  mutate(potencial_modelado = PC1*m1+PC2*m2+int) |> 
  select(sitio:potencial,potencial_modelado)

escala <- c('temporada','sitio','tratamiento')

data_metrica <- list()
for (x in 1:length(escala)) {
  
  data_metrica[[x]] <- data_val_mod |> 
    select(sitio,temporada,tratamiento,potencial,potencial_modelado) |> 
    group_by(!!!syms(escala[1:min(x, 3)])) |> 
    summarise(r2 = cor(potencial,potencial_modelado, use = 'na.or.complete')^2,
              rmse = rmse(potencial,potencial_modelado),
              mae = mae(potencial,potencial_modelado)) |> 
    ungroup() |> 
    mutate(texto = paste("R2:", round(r2, 2), 
                         "\nRMSE:", round(rmse, 2), 
                         "\nMAE:", round(mae, 2)))
}
names(data_metrica) <- escala

data_metrica_dia <- list()
for (x in 1:length(escala)) {
  
  data_metrica_dia[[x]] <- data_val_mod |> 
    filter(tipo == 'dia') |> 
    select(sitio,temporada,tratamiento,potencial,potencial_modelado) |> 
    group_by(!!!syms(escala[1:min(x, 3)])) |> 
    summarise(r2 = cor(potencial,potencial_modelado, use = 'na.or.complete')^2,
              rmse = rmse(potencial,potencial_modelado),
              mae = mae(potencial,potencial_modelado)) |> 
    ungroup() |> 
    mutate(texto = paste("R2:", round(r2, 2), 
                         "\nRMSE:", round(rmse, 2), 
                         "\nMAE:", round(mae, 2)))
}
names(data_metrica_dia) <- escala

data_metrica_hora <- list()
for (x in 1:length(escala)) {
  
  data_metrica_hora[[x]] <- data_val_mod |> 
    filter(tipo == 'hora') |> 
    select(sitio,temporada,tratamiento,potencial,potencial_modelado) |> 
    group_by(!!!syms(escala[1:min(x, 3)])) |> 
    summarise(r2 = cor(potencial,potencial_modelado, use = 'na.or.complete')^2,
              rmse = rmse(potencial,potencial_modelado),
              mae = mae(potencial,potencial_modelado)) |> 
    ungroup() |> 
    mutate(texto = paste("R2:", round(r2, 2), 
                         "\nRMSE:", round(rmse, 2), 
                         "\nMAE:", round(mae, 2)))
}
names(data_metrica_hora) <- escala

