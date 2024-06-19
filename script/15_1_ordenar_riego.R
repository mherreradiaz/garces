source('script/00_setup.R')

eto <- read_rds('data/processed/clima_dia.rds') |> 
  mutate(fecha = as.Date(fecha),
         lamina_mm = eto) |> 
  select(sitio,temporada,fecha,lamina_mm) |> 
  group_by(sitio,temporada) |> 
  mutate(lamina_acum = cumsum(lamina_mm),
         tratamiento = 'ET0') |> 
  ungroup() |> 
  filter(!(temporada =='2022-2023' & (fecha < '2022-10-11' | fecha > '2023-04-13')),
         !(temporada =='2023-2024' & (fecha < '2023-10-11' | fecha > '2024-04-12')),)
  
riego_2022 <- read_csv('data/raw/riego/riego_2022.csv') |> 
  mutate(fecha = as.Date(fecha_hora, format = "%d/%m/%Y"),
         .before = tratamiento) |> 
  group_by(sitio,tratamiento) |> 
  mutate(lamina_acum = cumsum(lamina_mm)) |> 
  ungroup() |> 
  select(-fecha_hora)

riego_2023 <- read_csv('data/raw/riego/riego_2023.csv') |> 
  mutate(fecha = as.Date(fecha_hora, format = "%d/%m/%Y"),
         .before = tratamiento) |>
  group_by(sitio,tratamiento) |> 
  mutate(lamina_acum = cumsum(lamina_mm)) |> 
  ungroup() |> 
  select(-fecha_hora)

riego <- bind_rows(riego_2022,riego_2023) |> 
  mutate(temporada = ifelse(fecha < '2023-06-01','2022-2023','2023-2024'),
         .before = fecha) |> 
  bind_rows(eto) |> 
  arrange(sitio,tratamiento,fecha) |> 
  mutate(volumen_m3 = lamina_mm*0.48)

write_rds(riego,'data/processed/riego.rds')

