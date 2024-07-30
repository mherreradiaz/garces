source('script/00_setup.R')

eto <- read_rds('data/processed/clima_dia.rds') |> 
  mutate(fecha = as.Date(fecha),
         lamina_mm = eto) |> 
  select(sitio,fecha,lamina_mm) |> 
  mutate(tratamiento = 'ET0')

local_2022 <- read_csv('data/raw/riego/local_2022.csv') |> 
  mutate(fecha = as.Date(fecha_hora, format = "%d/%m/%Y"),
         .before = tratamiento) |> 
  select(-fecha_hora)
local_2023 <- read_csv2('data/raw/riego/local_2023.csv') |> 
  mutate(fecha = as.Date(fecha_hora, format = "%d/%m/%Y"),
         .before = tratamiento) |> 
  select(-fecha_hora) |> 
  mutate(lamina_mm=as.numeric(lamina_mm))

tratamiento_2022 <- read_csv('data/raw/riego/tratamientos_2022.csv') |> 
  mutate(fecha = as.Date(fecha_hora, format = "%d/%m/%Y"),
         .before = tratamiento) |> 
  select(-fecha_hora)
tratamiento_2023 <- read_csv2('data/raw/riego/tratamientos_2023.csv') |> 
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"),
         lamina_mm = as.numeric(lamina_mm))

local <- bind_rows(local_2022,local_2023)
tratamiento <- bind_rows(tratamiento_2022,tratamiento_2023)

riego <- bind_rows(local,tratamiento,eto) |> 
  mutate(temporada = ifelse(fecha < '2023-06-01','2022-2023','2023-2024'),
         .before = fecha) |> 
  arrange(sitio,tratamiento,fecha) |> 
  mutate(volumen_m3 = lamina_mm*0.48)

write_rds(riego,'data/processed/riego.rds')

