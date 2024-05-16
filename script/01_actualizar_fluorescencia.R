source('script/funciones/paquetes.R')

data_fluo <- read_rds('data/processed/fluorescencia.rds')
codigos <- read_csv2('data/metadata/codigos_arboles.csv')

dates_fluo <- data_fluo |> 
  group_by(sitio) |> 
  distinct(fecha) 

files <- dir_ls('data/raw/fluorescencia',regexp = 'fluor_')
dates_new <- str_extract(files,'[0-9]{8}')

ind <- which(!(ymd(dates_new) %in% ymd(dates_fluo$fecha)))

sit <- str_remove_all(sapply(str_split(files[ind],'/'),function(x) x[4]),'fluor_|_[0-9]{8}.txt')

names_cols <- as.data.frame(read_csv('data/metadata/codigos_fluor.csv'))

for (x in 1:length(ind)) {
  
  lineas <- readLines(files[ind][x])
  lineas <- lineas[6:length(lineas)]
  campos <- strsplit(lineas, "\t")
  datos <- list()
  names <- c()
  
  for (i in 2:length(lineas)) {
    datos[[i-1]] <- as.numeric(campos[[i]][2:length(campos[[i]])])
    names[i-1] <- campos[[i]][1]
  }
  
  data_new <- as.data.frame(setNames(datos, names))
  data_new[which(data_new$Mo %in% boxplot.stats(data_new$Mo)$out),1:ncol(data_new)] <- NA
  
  data_new <- data_new |> 
    mutate(sitio = sit[x],
           fecha = ymd(dates_new[ind])[x],
           codigo = names_cols[which(names_cols$sitio == sit[x]),2], 
           .before = Bckg) |>
    group_by(sitio, fecha, codigo) |> 
    summarise(across(Bckg:`DIo.RC`, \(x) mean(x, na.rm = T))) |>
    left_join(codigos, by = c('sitio', 'codigo')) |>
    mutate(temporada = '2023-2024',
           tratamiento = substr(codigo,1,2),
           codigo = substr(codigo,3,nchar(codigo)),
           unidad = factor(unidad, levels = 1:3)) |>
    select(sitio, temporada, fecha, tratamiento, unidad, codigo, everything())
  
  names(data_new) <- names(data_fluo)
  data_fluo <- rbind(data_fluo, data_new)
  
}

data_fluo <- data_fluo |>
  arrange(sitio,fecha, tratamiento, unidad)

write_rds(data_fluo,'data/processed/fluorescencia.rds')

