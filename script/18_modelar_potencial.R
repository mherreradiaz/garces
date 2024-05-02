source('script/funciones/paquetes.R')

data_pca <- read_rds('data/data_processed/clima_pca.rds')

data_pca_dia <- data_pca |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(PC1 = mean(PC1,na.rm=T),
            PC2 = mean(PC2,na.rm=T)) |> 
  ungroup()
  
data_dia <- read_rds('data/data_processed/turgor_potencial_dia.rds') |> 
  left_join(data_pca_dia,by=c('sitio','temporada','fecha')) |>
  mutate(hora = '13:00',
         tipo = 'dia',
         .before = tratamiento)
  
data_hora <- read_rds('data/data_processed/turgor_potencial_hora.rds') |> 
  left_join(data_pca,by=c('sitio','temporada','fecha','hora')) |> 
  mutate(tipo = 'hora',
         .before = tratamiento)

data_modelo <- data_dia |> 
  sample_frac(0.8, replace = FALSE) |> 
  bind_rows(data_hora |> 
              sample_frac(0.8, replace = FALSE))

data_validacion <- bind_rows(data_dia,data_hora) |> 
  anti_join(data_modelo)

data_modelo |> 
  select(sitio,temporada,potencial,PC1,PC2) |> 
  na.omit() |> 
  group