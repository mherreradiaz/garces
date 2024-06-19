source('script/00_setup.R')
library(wesanderson)

# -- setup

pal <- c(wes_palette(n=5,name = 'Darjeeling2')[4],
         wes_palette(n=5,name = 'Darjeeling2')[2],
         wes_palette(n=5,name = 'Rushmore1')[3],
         wes_palette(n=4,name = 'Chevalier1')[2],
         wes_palette(n=5,name = 'Darjeeling1')[4],
         wes_palette(n=5,name = 'Rushmore1')[5])

names(pal) <- c('ET0','T0','T1','T2','T3','T4')

names <- c(la_esperanza = 'La Esperanza',rio_claro = 'Rio Claro',
           T1 = 'T1',T2 = 'T2',T3 = 'T3',T4 = 'T4',T0 = 'T0',
           '1' = '1','2' = '2','3' = '3', 
           '2022-2023' = '2022-2023', '2023-2024' = '2023-2024')

riego <- read_rds('data/processed/riego.rds')

# -- lamina acumm

riego |> 
  mutate(fecha = as.Date(ifelse(temporada == '2022-2023',fecha+years(1),fecha)),
         sitio = factor(sitio,levels=c('rio_claro','la_esperanza'))) |> 
  ggplot(aes(fecha,lamina_acum,fill=tratamiento)) +
  geom_area(alpha = .7,position = "identity") +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(temporada~sitio, scale = 'fixed',labeller = as_labeller(names)) +
  labs(y = 'daily cumulative water depth (mm)',
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
  select(-T0,-ET0) |> 
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
  select(-T0,-ET0) |> 
  pivot_longer(cols=c('T1','T2','T3','T4'),names_to='tratamiento',values_to='porcentaje') |> 
  mutate(temporada = '2023-2024',.before=tratamiento)

per <- bind_rows(per_2022,per_2023) |> 
  mutate(sitio=factor(sitio,levels=c('rio_claro','la_esperanza')))

# volumen aplicado

volumen <- riego |> 
  group_by(sitio,temporada,tratamiento) |> 
  summarise(volumen_total = sum(volumen_m3,na.rm=T)) |> 
  ungroup() |> 
  mutate(tratamiento = toupper(tratamiento),
         sitio = factor(sitio,levels=c('rio_claro','la_esperanza'))) |> 
  left_join(per,by=c('sitio','temporada','tratamiento'))

volumen |> 
  mutate(porcentaje = ifelse(is.na(porcentaje),"",paste(as.character(round(porcentaje,0)),'%'))) |> 
  filter(tratamiento != 'ET0') |> 
  ggplot(aes(tratamiento,volumen_total,fill = tratamiento)) +
  geom_bar(stat = 'identity') +
  facet_grid(temporada~sitio, scale = 'fixed',labeller = as_labeller(names)) +
  geom_text(aes(tratamiento,volumen_total+25,label = porcentaje),size=3) +
  labs(y = 'total volume applied (m3)',
       x = 'treatment',
       fill = 'treatment') +
  scale_fill_manual(values=pal) +
  theme_light() +
  theme(legend.position = "none")
ggsave(paste0('output/figs/volumen_total.png'), width = 7, height = 6)
