source('script/funciones/paquetes.R')

data_sentinel <- read_rds('data/processed/sentinel.rds')

data_potencial <- read_rds('data/processed/potencial.rds')

codigos_pote <- data_potencial |> 
  distinct(sitio,temporada,tratamiento,unidad,codigo) |> 
  arrange(sitio,temporada,tratamiento,unidad)
codigos_sent <- data_sentinel |> 
  distinct(sitio,temporada,tratamiento,unidad,codigo) |> 
  arrange(sitio,temporada,tratamiento,unidad)

data <- data_sentinel |>
  mutate(fecha = as.Date(fecha)) |> 
  group_by(sitio,temporada,tratamiento,unidad,codigo) |> 
  complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) |>
  mutate(across(B01:gci, ~ zoo::na.approx(., na.rm = FALSE))) |> 
  ungroup() |> 
  mutate(fecha = as.character(fecha)) |> 
  select(sitio,temporada,fecha,everything()) |> 
  left_join(data_potencial,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) |> 
  na.omit() |> 
  select(sitio:codigo,potencial_bar,everything())

list(unique(data_potencial$codigo),unique(data$codigo))


# data_x <- data |>
#   filter(temporada == '2022-2023',
#          sitio == 'la_esperanza',
#          tratamiento == 'T1') |>
#   mutate(fecha = as.Date(fecha),
#          dias = as.numeric(fecha - min(fecha))) |>
#   select(dias,ndmi)

data_complete |> 
  filter(temporada == '2022-2023') |> 
  mutate(fecha = as.POSIXct(fecha)) |> 
  ggplot(aes(fecha,B08)) +
  geom_point() +
  # geom_line() +
  facet_grid(tratamiento~sitio,scale='free')

data_x |> 
  ggplot(aes(fecha,ndwi)) +
  geom_point()

gam_model <- gam(ndmi ~ s(dias), data = data_x, method = "REML")

plot(data_x$dias, data_x$ndmi, main = "Whittaker-Eilers Smoothing", col = "blue", pch = 20)
lines(data_x$dias, predict(gam_model), col = "red")
legend("topleft", legend = c("Original", "Suavizado"), col = c("blue", "red"), pch = 20,)
