source('script/00_setup.R')

files <- grep('NDVI',list.files('data/processed/espacial/raster/vi_smooth',full.names=T),value=T)

kc <- lapply(files,function (x) {
  1.44*rast(x)+.1
})

eto <- read_rds('data/processed/clima_dia.rds') |> 
  select(sitio:fecha,eto) |> 
  mutate(eto = ifelse(eto==0,NA,eto))

sitio_grupo <- c(rep('la_esperanza',2),rep('rio_claro',2))
temporada_grupo <- c(rep(c('2022-2023','2023-2024'),2))

for (i in 1:4) {
  
  eto_grupo <- eto |> 
    filter(sitio == sitio_grupo[i],
           temporada == temporada_grupo[i])
  
  r <- kc[[i]]
  fechas_comunes <- intersect(names(r),eto_grupo$fecha)
  
  riego <- r[[which(names(r) %in% fechas_comunes)]]*
    pull(eto_grupo,eto)[which(eto_grupo$fecha %in% fechas_comunes)]
  
  writeRaster(riego,glue('data/processed/espacial/raster/ETc/ETc_{sitio_grupo[[i]]}_{substr(temporada_grupo[[i]],1,4)}.tif'),
              overwrite=T)
  writeRaster(r,glue('data/processed/espacial/raster/ETc/Kc_{sitio_grupo[[i]]}_{substr(temporada_grupo[[i]],1,4)}.tif'),
              overwrite=T)
  
}

# extraer en sectores

etc_r <- grep('ETc_',list.files('data/processed/espacial/raster/ETc',full.names=T),value=T)
kc_r <- grep('Kc_',list.files('data/processed/espacial/raster/ETc',full.names=T),value=T)

sector <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg',layer = 'sectores_riego'),
               vect('data/processed/espacial/sitios/la_esperanza.gpkg',layer = 'sectores_riego'),
               vect('data/processed/espacial/sitios/rio_claro.gpkg',layer = 'sectores_riego'),
               vect('data/processed/espacial/sitios/rio_claro.gpkg',layer = 'sectores_riego'))

sitio_grupo <- c(rep('la_esperanza',2),rep('rio_claro',2))
temporada_grupo <- c(rep(c('2022-2023','2023-2024'),2))

etc_riego_grupo <- list()
kc_riego_grupo <- list()

for (i in 1:4) {
  
  etc_riego_grupo[[i]] <- extract(rast(etc_r[i]),sector[[i]],fun = mean) |> 
    pivot_longer(cols=-1,names_to = 'fecha', values_to = 'ETc') |> 
    rename(sector = ID) |>
    mutate(sitio = sitio_grupo[[i]],
           temporada = temporada_grupo[i]) |> 
    select(sitio,temporada,fecha,sector,ETc)
  
  kc_riego_grupo[[i]] <- extract(rast(kc_r[i]),sector[[i]],fun = mean) |> 
    pivot_longer(cols=-1,names_to = 'fecha', values_to = 'Kc') |> 
    rename(sector = ID) |>
    mutate(sitio = sitio_grupo[[i]],
           temporada = temporada_grupo[i]) |> 
    select(sitio,temporada,fecha,sector,Kc)
    
}

riego <- bind_rows(etc_riego_grupo) |> 
  left_join(bind_rows(kc_riego_grupo)) |> 
  arrange(sitio,temporada,fecha,sector) |> 
  rename(sector_id = sector) |> 
  mutate(fecha = as.Date(fecha))

write_rds(riego,'data/processed/ETc.rds')


# graficar

etc <- read_rds('data/processed/ETc.rds')

etc |> 
  mutate(sector_id = as.factor(sector_id)) |> 
  ggplot(aes(fecha,ETc,color = sector_id)) +
  geom_point(size=.7,alpha=.7) +
  geom_line(linewidth = .5, alpha = .7) +
  facet_wrap(temporada~sitio, scales = 'free_x') +
  labs(x = NULL,
       y = 'ETc (mm)') +
  theme_bw() +
  theme(legend.position = 'none')

ggsave('output/figs/ETc.png', height = 8, width = 13)
