source('script/funciones/paquetes.R')

names <- c(la_esperanza = 'La Esperanza',rio_claro = 'Rio Claro',
           T1 = 'T1',T2 = 'T2',T3 = 'T3',T4 = 'T4',T0 = 'T0',
           '1' = '1','2' = '2','3' = '3', 
           '2022-2023' = '2022-2023', '2023-2024' = '2023-2024')

eto <- read_rds('data/processed/clima_dia.rds') |> 
  mutate(fecha = as.Date(fecha)) |> 
  select(sitio,temporada,fecha,eto) |> 
  group_by(sitio,temporada) |> 
  mutate(lamina_acum = cumsum(eto),
         tratamiento = 'et0') |> 
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

riego_garces <- read_csv2('data/raw/riego/riego_la_esperanza.csv') |> 
  mutate(sitio = 'la_esperanza',
         fecha = as.Date(From, format = "%d/%m/%Y"),
         tratamiento = 'T0',
         .before=From) |> 
  select(-From,-To,-volume_m3) |> 
  mutate(lamina_acum = cumsum(lamina_mm))

riego_tratamiento <- read_xlsx('data/raw/riego/riego_tratamientos.xlsx',sheet=1) |> 
  mutate(fecha = as.Date(fecha)) |> 
  pivot_longer(cols=c('T1','T2','T3','T4'),names_to = 'tratamiento',values_to = 'lamina_mm') |> 
  group_by(sitio,tratamiento) |> 
  mutate(lamina_acum = cumsum(lamina_mm)) |> 
  ungroup()

riego <- bind_rows(riego_2022,riego_garces,riego_tratamiento) |> 
  arrange(fecha,tratamiento) |> 
  mutate(temporada = ifelse(fecha < '2023-06-01','2022-2023','2023-2024')) |> 
  bind_rows(eto)

write_rds(riego,'data/processed/riego.rds')

# visualizar

riego |> 
  mutate(tratamiento = toupper(tratamiento)) |> 
  ggplot(aes(fecha,lamina_acum,fill=tratamiento)) +
  geom_area(alpha = .6,position = "identity") +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(sitio~temporada, scale = 'free_x',labeller = as_labeller(names)) +
  labs(y = 'cum mm',
       x = 'month',
       fill = 'treatment') +
  theme_light()
ggsave(paste0('output/figs/riego_lamina.png'),scale =3:5.5)

# déficit por tratamiento

fecha_t0_2022 <- riego |> 
  filter(temporada == '2022-2023',
         tratamiento == 'T0',
         sitio == 'rio_claro') |> 
  pull(fecha) |> max()

fecha_t1_2023 <- riego |> 
  filter(temporada == '2023-2024',
         tratamiento == 'T1',
         sitio == 'la_esperanza') |> 
  pull(fecha) |> max()

riego |> 
  filter(temporada == '2022-2023',
         fecha < fecha_t0_2022) |> 
  group_by(sitio,tratamiento) |> 
  summarise(lamina_sum = sum(lamina_mm)) |> 
  ungroup() |> 
  pivot_wider(values_from='lamina_sum',names_from='tratamiento') |> 
  mutate('T1%' = 1-T1/T0,'T2%' = 1-T2/T0,'T3%' = 1-T3/T0,'T4%' = 1-T4/T0)

riego |> 
  filter(temporada == '2023-2024',
         fecha < fecha_t1_2023) |> 
  group_by(sitio,tratamiento) |> 
  summarise(lamina_sum = sum(lamina_mm)) |> 
  ungroup() |> 
  pivot_wider(values_from='lamina_sum',names_from='tratamiento') |> 
  mutate('T1%' = 1-T1/T0,'T2%' = 1-T2/T0,'T3%' = 1-T3/T0,'T4%' = 1-T4/T0)
