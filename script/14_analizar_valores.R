source('script/funciones/paquetes.R')

files <- list.files('data/processed/espacial/raster/potencial_mod/',full.names=T)

sitios <- str_extract(files,"(?<=mod_)[a-zA-Z_]+(?=_\\d{4}_\\d{2}_\\d{2})")
fechas <- gsub('_','-',str_extract(files,"\\d{4}_\\d{2}_\\d{2}"))

id_files <- tibble(sitios,fechas) |> 
  mutate(id = paste0(sitios,'_',gsub('-','_',fechas)),
         temporadas = ifelse(fechas < '2023-06-01','2022-2023','2023-2024'))

pol_list <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2022'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2022'),
                 vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2023'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2023'))

data_list <- list()

for (x in 1:length(pol_list)) {
  
  pol <- pol_list[[x]]
  
  name <- sources(pol)
  
  sitio <- str_extract(name, "(?<=sitios\\\\)[^\\.]+")
  año <- as.numeric(str_extract(name, "\\d{4}$"))
  temporada <- paste0(año,'-',año+1)
  
  id <- id_files |> 
    filter(sitios == sitio,
           temporadas == temporada) |> 
    pull(id)
  
  r <- rast(files[which(id_files$id %in% id)])
  
  names(r) <- id_files |> 
    filter(sitios == sitio,
           temporadas == temporada) |> 
    pull(fechas)
  
  extract <- extract(r,pol)
  
  data_list[[x]] <- extract |> 
    left_join(values(pol) |> 
                mutate(ID = 1:15,
                       unidad=factor(rep(1:3,5),levels = 1:3)) |> 
                select(ID,unidad,codigo),by='ID') |> 
    pivot_longer(cols=2:ncol(extract),names_to='fecha',values_to='potencial') |> 
    separate(col=codigo,into=c('tratamiento','codigo'),sep=2) |> 
    mutate(sitio = sitio,
           temporada = temporada,
           .before = tratamiento) |> 
    select(sitio,temporada,fecha,tratamiento,unidad,everything(),-ID)
 
}

data <- bind_rows(data_list)

# visualizacion clima

data_clima <- read_rds('data/processed/clima.rds') |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup() |> 
  distinct()

data_plot <- data |> 
  left_join(data_clima,by=c('sitio','temporada','fecha')) |> 
  pivot_longer(cols=c('potencial','t_media','rh_media','vpd_medio'),
               names_to='variable',
               values_to='valor') |> 
  group_by(sitio,temporada,variable) |> 
  mutate(valor = as.numeric(scale(valor))) |> 
  ungroup() |> 
  group_by(sitio,temporada,fecha,variable) |> 
  summarise(valor = mean(valor,na.rm=T)) |> 
  ungroup()

data_plot |> 
  mutate(fecha = as.Date(fecha)) |> 
  filter(variable %in% c('potencial','vpd_medio')) |> 
  ggplot(aes(fecha,valor,color=variable)) +
  geom_line() +
  labs(y = 'variables estandarizadas',
       x = 'mes') +
  facet_grid(sitio~temporada,scales='free')

# visualización índices

data_index <- read_rds('data/processed/sentinel2_index_smooth.rds')

data_plot <- data |> 
  left_join(data_index,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) |> 
  pivot_longer(cols=c('potencial','ndwi','ndmi','msi','gci','ndvi','nbr','b_i'),
               names_to='variable',
               values_to='valor') |> 
  # group_by(sitio,temporada,variable) |> 
  # mutate(valor = as.numeric(scale(valor))) |> 
  # ungroup() |> 
  group_by(sitio,temporada,fecha,variable) |> 
  summarise(valor = mean(valor,na.rm=T)) |> 
  ungroup()

data_plot |> 
  mutate(fecha = as.Date(fecha)) |> 
  filter(variable %in% c('potencial','ndwi')) |> 
  ggplot(aes(fecha,valor,color=variable)) +
  geom_line() +
  labs(y = 'variables estandarizadas',
       x = 'mes') +
  facet_grid(sitio~temporada,scales='free')
