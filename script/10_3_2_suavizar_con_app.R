source('script/funciones/paquetes.R')
library(mgcv)

files <- list.files('data/processed/espacial/raster/vi_raw/',full.names=T)
name <- str_match(files, ".*/RAW_(.*?)\\.tif$")[,2]

vi_r <- lapply(files, function(x) rast(x))

suavizado <- list()
dir.out <- 'data/processed/espacial/raster/vi_smooth/'

for (i in seq_along(vi_r)) {
  print(name[i])
  vi <- vi_r[[i]]
  names(vi)
  fechas <- as.Date(names(vi))
  
  vi_suavizado <- app(vi,\(y){
    y[which(y > 10)] <- NA
    y[is.infinite(y)] <- NA
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- loess(y ~ x, data = data, span = .3)
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  names(vi_suavizado) <- seq(min(fechas),max(fechas),by='1 day')
  writeRaster(vi_suavizado,glue('{dir.out}SMOOTH_{name[i]}.tif'))
}

# visualizar

files_raw <- list.files('data/processed/espacial/raster/vi_raw/',full.names=T)
files_smooth <- list.files('data/processed/espacial/raster/vi_smooth/',full.names=T)

name <- unique(str_match(files_raw,".*/(.*?)_(.*?)_(\\d{4})\\.tif$")[,3])
vi_name <- str_split(name, "_", n = 2, simplify = TRUE)[,1]
sitio <- str_split(name, "_", n = 2, simplify = TRUE)[,2]

plot_list <- list()

for (i in seq_along(name)) {
  
  px <- ifelse(sitio[i] == 'la_esperanza',5998,7982)
  
  plot_list[[i]] <- lapply(grep(name[i],files_smooth,value=T), function(x) {
    r <- rast(x)[px]
    tibble(fecha = names(r), smooth = as.numeric(r))}) |> 
    bind_rows() |> 
    left_join(lapply(grep(name[i],files_raw,value=T), function(x) {
      r <- rast(x)[px]
      tibble(fecha = names(r), raw = as.numeric(r))}) |> 
        bind_rows(),by= 'fecha') |> 
    mutate(temporada=ifelse(fecha<'2023-06-01','2022-2023','2023-2024')) |> 
    pivot_longer(cols=c('smooth','raw'),values_to='value',names_to='origin') |>
    mutate(fecha = as.Date(fecha)) |> 
    ggplot(aes(fecha,value,color=origin)) +
    geom_point(size = .7) +
    facet_grid(~temporada,scales='free_x') +
    labs(title = sitio[i], y = vi_name[i]) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b") +
    theme(legend.position = "none",
          plot.title = element_blank(),
          axis.title.x = element_blank())

}

plot_le <- plot_list[1:length(plot_list) %% 2 != 0]
plot_rc <- plot_list[1:length(plot_list) %% 2 == 0]

wrap_plots(plot_le, ncol = 4, nrow = 4)
ggsave(paste0('output/figs/vi_suavizado_la_esperanza.png'), width = 15, height = 10)
wrap_plots(plot_rc, ncol = 4, nrow = 4)
ggsave(paste0('output/figs/vi_suavizado_rio_claro.png'), width = 15, height = 10)


# parametros biofísicos

fapar_files <- list.files('data/processed/espacial/raster/biophysical/fapar/',full.names=T)
fcover_files <- list.files('data/processed/espacial/raster/biophysical/fcover/',full.names=T)
lai_files <- list.files('data/processed/espacial/raster/biophysical/lai/',full.names=T)
cab_files <- list.files('data/processed/espacial/raster/biophysical/lai_cab/',full.names=T)
cw_files <- list.files('data/processed/espacial/raster/biophysical/lai_cw/',full.names=T)

fecha <- as.Date(str_extract(fapar_files,"\\d{8}"),format="%Y%m%d")
temporada <- ifelse(fecha<'2023-06-01','2022','2023')
sitio <- str_extract(fapar_files,"(rio_claro|la_esperanza)")

name <- c('la_esperanza_2022','rio_claro_2022',
          'la_esperanza_2023','rio_claro_2023')

dir.out <- 'data/processed/espacial/raster/biopar_raw/'

id <- list(which(temporada == '2022' & sitio == 'la_esperanza'),
           which(temporada == '2022' & sitio == 'rio_claro'),
           which(temporada == '2023' & sitio == 'la_esperanza'),
           which(temporada == '2023' & sitio == 'rio_claro'))

for (i in seq_along(id)) {
  
  fapar <- rast(fapar_files[id[[i]]])
  fcover <- rast(fcover_files[id[[i]]])
  lai <- rast(lai_files[id[[i]]])
  cab <- rast(cab_files[id[[i]]])
  cw <- rast(cw_files[id[[i]]])
  
  names(fapar) <- fecha[id[[i]]]
  names(fcover) <- fecha[id[[i]]]
  names(lai) <- fecha[id[[i]]]
  names(cab) <- fecha[id[[i]]]
  names(cw) <- fecha[id[[i]]]
  
  writeRaster(fapar,glue('{dir.out}RAW_FAPAR_{name[i]}.tif'))
  writeRaster(fcover,glue('{dir.out}RAW_FCOVER_{name[i]}.tif'))
  writeRaster(lai,glue('{dir.out}RAW_LAI_{name[i]}.tif'))
  writeRaster(cab,glue('{dir.out}RAW_CAB_{name[i]}.tif'))
  writeRaster(cw,glue('{dir.out}RAW_CW_{name[i]}.tif'))
  
}

#

all_files <- sort(list.files('data/processed/espacial/raster/biopar_raw/',full.names=T))
all_fechas <- substr(all_files,nchar(all_files)-11,nchar(all_files)-4)
all_fechas <- gsub('_','-',paste0(substr(all_fechas, 1, 4), "_", substr(all_fechas, 5, 6), "_", substr(all_fechas, 7, 8)))

index_name <- c('fapar','fcover','lai','lai_cab','lai_cw')

files <- list.files('data/processed/espacial/raster/biopar_raw/',full.names=T)
name <- str_match(files, ".*/RAW_(.*?)\\.tif$")[,2]

biopar_r <- lapply(files, function(x) rast(x))

suavizado <- list()
dir.out <- 'data/processed/espacial/raster/biopar_smooth/'

for (i in seq_along(biopar_r)) {
  print(name[i])
  biopar <- biopar_r[[i]]
  fechas <- as.Date(names(biopar))
  
  biopar_suavizado <- app(biopar,\(y){
    y[is.infinite(y)] <- NA
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- loess(y ~ x, data = data, span = .3)
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  names(biopar_suavizado) <- seq(min(fechas),max(fechas),by='1 day')
  writeRaster(biopar_suavizado,glue('{dir.out}SMOOTH_{name[i]}.tif'))
}

# visualizar

files_raw <- list.files('data/processed/espacial/raster/biopar_raw/',full.names=T)
files_smooth <- list.files('data/processed/espacial/raster/biopar_smooth/',full.names=T)

name <- unique(str_match(files_raw,".*/(.*?)_(.*?)_(\\d{4})\\.tif$")[,3])
vi_name <- str_split(name, "_", n = 2, simplify = TRUE)[,1]
sitio <- str_split(name, "_", n = 2, simplify = TRUE)[,2]

plot_list <- list()

for (i in seq_along(name)) {
  
  px <- ifelse(sitio[i] == 'la_esperanza',1541,1910)
  
  plot_list[[i]] <- lapply(grep(name[i],files_smooth,value=T), function(x) {
    r <- rast(x)[px]
    tibble(fecha = names(r), smooth = as.numeric(r))}) |> 
    bind_rows() |> 
    left_join(lapply(grep(name[i],files_raw,value=T), function(x) {
      r <- rast(x)[px]
      tibble(fecha = names(r), raw = as.numeric(r))}) |> 
        bind_rows(),by= 'fecha') |> 
    mutate(temporada=ifelse(fecha<'2023-06-01','2022-2023','2023-2024')) |> 
    pivot_longer(cols=c('smooth','raw'),values_to='value',names_to='origin') |>
    mutate(fecha = as.Date(fecha)) |> 
    ggplot(aes(fecha,value,color=origin)) +
    geom_point(size = .3) +
    facet_grid(~temporada,scales='free_x') +
    labs(title = sitio[i], y = vi_name[i]) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b") +
    theme(legend.position = "none",
          plot.title = element_blank(),
          axis.title.x = element_blank())
  
}

plot_le <- plot_list[1:length(plot_list) %% 2 != 0]
plot_rc <- plot_list[1:length(plot_list) %% 2 == 0]

wrap_plots(plot_le, ncol = 2, nrow = 3)
ggsave(paste0('output/figs/biopar_suavizado_la_esperanza.png'), width = 10, height = 5)
wrap_plots(plot_rc, ncol = 2, nrow = 3)
ggsave(paste0('output/figs/biopar_suavizado_rio_claro.png'), width = 10, height = 5)
