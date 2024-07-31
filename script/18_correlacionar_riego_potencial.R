source('script/00_setup.R')

minmax <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(c(min = NA, max = NA))
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  min_value <- min(x[x >= lower_bound], na.rm = TRUE)
  max_value <- max(x[x <= upper_bound], na.rm = TRUE)
  
  return(c(min = min_value, max = max_value))
}

# dias soleados

clima <- read_rds('data/processed/clima_hora.rds') |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup()

umbrales <- read_rds('data/processed/potencial.rds') |> 
  distinct(sitio,temporada,fecha) |> 
  left_join(clima) |> 
  group_by(sitio,temporada,month(fecha)) |> 
  summarise(t = floor(minmax(t_media)[1]),
            rh = ceiling(minmax(rh_media)[2]),
            vpd = floor(minmax(vpd_medio)[1]))

fechas <- clima |> 
  mutate(month(fecha)) |> 
  left_join(umbrales) |> 
  select(-`month(fecha)`) |> 
  filter(!(t_media < t & rh_media > rh & vpd_medio < vpd)) |> 
  mutate(diff_bef = c(NA, diff(as.Date(fecha))),
         diff_aft = lead(diff_bef, default = NA),
         diff = ifelse(diff_bef == 1 | diff_aft == 1,1,NA)) |> 
  filter(diff == 1) |> 
  select(sitio:fecha) |> 
  mutate(filter = 1,
         fecha = as.Date(fecha))

# potencial

files <- list.files('data/processed/espacial/raster/potencial_predict/',full.names=T) 
files_le <- grep('la_esperanza',files,value=T)
files_rc <- grep('rio_claro',files,value=T)

sitio <- c('la_esperanza','rio_claro')
temporada <- c('2022-2023','2023-2024')

pol_2022 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2022'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2022'))
pol_2023 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2023'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2023'))
pol <- list(pol_2022,pol_2023)

data_list_le <- lapply(files_le,function(x) {
  r <- rast(x)
  r_fecha <- names(r)
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  names(r) <- 'potencial'
  df <- data.frame(sitio = 'la_esperanza',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[1]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[1]], ID =F))
  
  return(df)
})
data_list_rc <- lapply(files_rc,function(x) {
  r <- rast(x)
  r_fecha <- names(r)
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  names(r) <- 'potencial'
  df <- data.frame(sitio = 'rio_claro',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[2]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[2]], ID =F))
  
  return(df)
})

data_le <- bind_rows(data_list_le)
data_rc <- bind_rows(data_list_rc)

potencial <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |> 
  mutate(fecha = gsub('_','-',fecha)) |> 
  arrange(sitio,fecha,tratamiento,unidad)

riego <- read_rds('data/processed/riego.rds')

data <- potencial |> 
  mutate(fecha=as.Date(fecha)) |> 
  left_join(riego,by=c('sitio','temporada','fecha','tratamiento'))

# delta potencial

plot = list()

for (i in 1:5) { # sacar delta en i días a partir del riego
  
  delta_data <- data |> 
    left_join(fechas) |> 
    filter(filter==1) |> 
    select(-filter) |> 
    arrange(sitio,temporada,tratamiento,unidad,fecha) |> 
    mutate(diferencia_dias = ifelse(as.numeric(lead(fecha,i)-fecha) == i,1,NA),
           indicador_riego = ifelse(!is.na(lamina_mm) & diferencia_dias == 1, 1, NA),
           indicador_riego = ifelse(indicador_riego == 1 | lag(indicador_riego,i) == 1,1,NA)) |> 
    filter(indicador_riego == 1) |> 
    select(-diferencia_dias,-indicador_riego) |> 
    mutate(delta_potencial = lead(potencial)-potencial) |> 
    na.omit() |> 
    select(sitio:codigo,lamina_mm,delta_potencial)
  
  n <- delta_data |> 
    group_by(sitio,temporada) |> 
    summarise(q3 = quantile(delta_potencial,.75)*2,
              n = n())
  
  plot[[i]] <- delta_data |> 
    ggplot(aes(sitio,delta_potencial)) +
    geom_boxplot() +
    geom_text(data=n,aes(sitio,q3,label = paste0('n = ',n)),hjust = 1.3, size = 3) +
    facet_grid(~temporada,labeller = as_labeller(names)) +
    labs(title = glue('{i} día/s después del riego')) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank())

}

wrap_plots(plot, ncol = 3, nrow = 2)
ggsave(glue('output/figs/delta_potencial.png'), width = 15, height = 10)
