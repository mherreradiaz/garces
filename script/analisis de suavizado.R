source('script/funciones/setup')


# all_files_smooth <- list.files('data/processed/espacial/raster/vi_smooth/',full.names=T)
# all_fechas_smooth <- gsub('_','-',substr(all_files_smooth,nchar(all_files_smooth)-13,nchar(all_files_smooth)-4))
# 
# all_files_raw <- list.files('data/processed/espacial/raster/vi_raw/',full.names=T)
# all_fechas_raw <- gsub('_','-',substr(all_files_raw,nchar(all_files_raw)-13,nchar(all_files_raw)-4))
# 
# index_name <- c("ndwi", "ndmi", "msi", "gci", "ndvi", "nbr", "nmdi", "dwsi",'ndvi_705', "b_i")

all_files_smooth <- list.files('data/processed/espacial/raster/biopar_smooth',full.names=T)
all_fechas_smooth <- gsub('_','-',substr(all_files_smooth,nchar(all_files_smooth)-13,nchar(all_files_smooth)-4))

all_files_raw <- sort(list.files('data/processed/espacial/raster/biopar_raw/',full.names=T))
all_fechas_raw <- substr(all_files_raw,nchar(all_files_raw)-11,nchar(all_files_raw)-4)
all_fechas_raw <- gsub('_','-',paste0(substr(all_fechas_raw, 1, 4), "_",
                                      substr(all_fechas_raw, 5, 6), "_",
                                      substr(all_fechas_raw, 7, 8)))

index_name <- c('fapar','fcover','lai','lai_cab','lai_cw')

# setup

sitio <- 'rio_claro'

files_smooth <- grep(sitio,all_files_smooth,value=T)
fechas_smooth <- as.Date(gsub('_','-',substr(files_smooth,nchar(files_smooth)-13,nchar(files_smooth)-4)))
r <- rast(files_smooth)

index_list_smooth <- lapply(index_name, function(x) {
  index <- subset(r,which(names(r)==x))
  names(index) <- fechas_smooth
  index
})
names(index_list_smooth) <- index_name

files_raw <- grep(sitio,all_files_raw,value=T)
fechas_raw <- substr(files_raw,nchar(files_raw)-11,nchar(files_raw)-4)
fechas_raw <- as.Date(gsub('_','-',paste0(substr(fechas_raw, 1, 4), "_",
                                  substr(fechas_raw, 5, 6), "_",
                                  substr(fechas_raw, 7, 8))))
# fechas_raw <- as.Date(gsub('_','-',substr(files_raw,nchar(files_raw)-13,nchar(files_raw)-4)))

r <- rast(files_raw)

index_list_raw <- lapply(index_name, function(x) {
  index <- subset(r,which(names(r)==x))
  names(index) <- fechas_raw
  index
})
names(index_list_raw) <- index_name

# escoger vi y pixel

index_smooth <- index_list_smooth[[x]]
index_raw <- index_list_raw[[x]]

px <- sample(nrow(values(index_smooth[[1]])),1)

data <- tibble(fecha = as.Date(names(index_smooth)),
               smooth = as.numeric(index_smooth[px])) |> 
  left_join(tibble(fecha = as.Date(names(index_raw)),
                   raw = as.numeric(index_raw[px])),
            by='fecha') |> 
  mutate(temporada = ifelse(fecha < '2023-06-01','2022-2023','2023-2024')) |> 
  pivot_longer(cols=c('smooth','raw'),names_to='origin',values_to='value')

data |> 
  ggplot(aes(fecha,value,color=origin)) +
  geom_point() +
  facet_grid(~temporada,scales='free_x') +
  labs(x = 'month',
       y=index_name[x])





