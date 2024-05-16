source('script/funciones/paquetes.R')

data_potencial <- read_rds('data/processed/potencial.rds')

dates_potencial <- data_potencial |> 
  group_by(sitio) |> 
  distinct(fecha) |> 
  ungroup()

files <- dir_ls('data/raw/potencial',regexp = 'potencial_')
dates_new <- str_extract(files,'[0-9]{8}')

ind <- which(!(ymd(dates_new) %in% ymd(dates_potencial$fecha)))

sit <- str_remove_all(sapply(str_split(files[ind],'/'),function(x) x[4]),'potencial_|_[0-9]{8}.xlsx')

for (x in 1:length(ind)) {
  
  data_new <- read_xlsx(files[ind][x], sheet = 1)
  
  data_new <- data_new |> 
    rename(potencial_bar = bar) |>
    mutate(sitio = sit[x],
           temporada = '2023-2024',
           fecha = as.character(ymd(dates_new[ind][x])),
           tratamiento = substr(codigo,1,2),
           unidad = factor(rep(1:3,5), levels = 1:3),
           .before = codigo) |>
    mutate(codigo = substr(codigo,3,nchar(codigo))) |>
    arrange(across(sitio:unidad))
  
  data_potencial <- rbind(data_potencial, data_new)
  
}

data_potencial <- data_potencial |>
  arrange(fecha, tratamiento, unidad)

write_rds(data_potencial,'data/processed/potencial.rds')
