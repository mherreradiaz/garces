source('script/funciones/paquetes.R')

data_sentinel <- read_rds('data/processed/sentinel.rds')
data_potencial <- read_rds('data/processed/potencial.rds')
data_clima <- read_rds('data/processed/clima_lag.rds') |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup()
  
data <- data_sentinel |>
  mutate(fecha = as.Date(fecha)) |> 
  group_by(sitio,temporada,tratamiento,unidad,codigo) |> 
  complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) |>
  mutate(across(B01:gci, ~ zoo::na.approx(., na.rm = FALSE))) |> 
  ungroup() |> 
  mutate(fecha = as.character(fecha)) |> 
  select(sitio,temporada,fecha,everything()) |> 
  left_join(data_potencial,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) |> 
  left_join(data_clima,by=c('sitio','temporada','fecha')) |> 
  na.omit() |> 
  select(sitio:codigo,potencial_bar,everything()) |> 
  arrange(temporada,fecha,sitio,tratamiento,unidad)

write_rds(data,'data/processed/modelo_potencial.rds')

# suavizado

gam_model <- gam(ndmi ~ s(dias), data = data_x, method = "REML")

plot(data_x$dias, data_x$ndmi, main = "Whittaker-Eilers Smoothing", col = "blue", pch = 20)
lines(data_x$dias, predict(gam_model), col = "red")
legend("topleft", legend = c("Original", "Suavizado"), col = c("blue", "red"), pch = 20)
