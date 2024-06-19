source('script/funciones/paquetes.R')

library(wesanderson)

pal <- c(rev(wes_palette(n=5,name = 'Darjeeling1'))[1],
         sort(c(wes_palette(n=5,name = 'Darjeeling1')[-5],
           wes_palette(n=5,name = 'Royal2')[2])))
pal <- c(pal,'black')
names(pal) <- c('ET0','T0','T1','T2','T3','T4')


pal <- c(wes_palette(n=5,name = 'Darjeeling2')[4],
         wes_palette(n=5,name = 'Darjeeling2')[2],
         wes_palette(n=5,name = 'Rushmore1')[3],
         wes_palette(n=4,name = 'Chevalier1')[2],
         wes_palette(n=5,name = 'Darjeeling1')[4],
         wes_palette(n=5,name = 'Rushmore1')[5])

names <- c(la_esperanza = 'La Esperanza',rio_claro = 'Rio Claro',
           T1 = 'T1',T2 = 'T2',T3 = 'T3',T4 = 'T4',T0 = 'T0',
           '1' = '1','2' = '2','3' = '3', 
           '2022-2023' = '2022-2023', '2023-2024' = '2023-2024')

eto <- read_rds('data/processed/clima_dia.rds') |> 
  mutate(fecha = as.Date(fecha),
         lamina_mm = eto) |> 
  select(sitio,temporada,fecha,lamina_mm) |> 
  group_by(sitio,temporada) |> 
  mutate(lamina_acum = cumsum(lamina_mm),
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

riego_2023 <- read_csv('data/raw/riego/riego_2023.csv') |> 
  mutate(fecha = as.Date(fecha_hora, format = "%d/%m/%Y"),
         .before = tratamiento) |>
  group_by(sitio,tratamiento) |> 
  mutate(lamina_acum = cumsum(lamina_mm)) |> 
  ungroup() |> 
  select(-fecha_hora)

riego <- bind_rows(riego_2022,riego_2023) |> 
  arrange(fecha,tratamiento) |> 
  mutate(temporada = ifelse(fecha < '2023-06-01','2022-2023','2023-2024')) |> 
  bind_rows(eto)

write_rds(riego,'data/processed/riego.rds')

# visualizar

riego |> 
  mutate(tratamiento = toupper(tratamiento),
         fecha = as.Date(ifelse(temporada == '2022-2023',fecha+years(1),fecha)),
         sitio = factor(sitio,levels=c('rio_claro','la_esperanza'))) |> 
  ggplot(aes(fecha,lamina_acum,fill=tratamiento)) +
  geom_area(alpha = .6,position = "identity") +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(temporada~sitio, scale = 'fixed',labeller = as_labeller(names)) +
  labs(y = 'cum mm',
       x = 'month',
       fill = 'treatment') +
  scale_fill_manual(values=pal) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light()
ggsave(paste0('output/figs/riego_lamina.png'), width = 8, height = 6)

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

per_2022 <- riego |> 
  filter(temporada == '2022-2023',
         fecha < fecha_t0_2022) |> 
  group_by(sitio,tratamiento) |> 
  summarise(lamina_sum = sum(lamina_mm)) |> 
  ungroup() |> 
  pivot_wider(values_from='lamina_sum',names_from='tratamiento') |> 
  mutate(T1 = 100*T1/T0,T2 = 100*T2/T0,T3 = 100*T3/T0,T4 = 100*T4/T0) |> 
  select(-T0,-et0) |> 
  pivot_longer(cols=c('T1','T2','T3','T4'),names_to='tratamiento',values_to='porcentaje') |> 
  mutate(temporada = '2022-2023',.before=tratamiento)

per_2023 <- riego |> 
  filter(temporada == '2023-2024',
         fecha < fecha_t1_2023) |> 
  group_by(sitio,tratamiento) |> 
  summarise(lamina_sum = sum(lamina_mm)) |> 
  ungroup() |> 
  pivot_wider(values_from='lamina_sum',names_from='tratamiento') |> 
  mutate(T1 = 100*T1/T0,T2 = 100*T2/T0,T3 = 100*T3/T0,T4 = 100*T4/T0) |> 
  select(-T0,-et0) |> 
  pivot_longer(cols=c('T1','T2','T3','T4'),names_to='tratamiento',values_to='porcentaje') |> 
  mutate(temporada = '2023-2024',.before=tratamiento)

per <- bind_rows(per_2022,per_2023) |> 
  mutate(sitio=factor(sitio,levels=c('rio_claro','la_esperanza')))

# volumen aplicado

volumen <- riego |> 
  mutate(volumen_m3 = lamina_mm*0.48) |> 
  group_by(sitio,temporada,tratamiento) |> 
  summarise(volumen_total = sum(volumen_m3,na.rm=T)) |> 
  ungroup() |> 
  mutate(tratamiento = toupper(tratamiento),
         sitio = factor(sitio,levels=c('rio_claro','la_esperanza')))

per <- per |> 
  left_join(volumen,by=c('sitio','temporada','tratamiento'))

volumen |> 
  left_join(per,by=c('sitio','temporada','tratamiento')) |> 
  mutate(porcentaje = ifelse(is.na(porcentaje),"",paste(as.character(round(porcentaje,0)),'%'))) |> 
  filter(tratamiento != 'ET0') |> 
  ggplot(aes(tratamiento,volumen_total,fill = tratamiento)) +
  geom_bar(stat = 'identity') +
  facet_grid(temporada~sitio, scale = 'fixed',labeller = as_labeller(names)) +
  geom_text(aes(tratamiento,volumen_total+50,label = porcentaje),size=3) +
  labs(y = 'total volume aplicated (m3)',
       x = 'month',
       fill = 'treatment') +
  scale_fill_manual(values=pal) +
  theme_light()
