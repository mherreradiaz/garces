source('script/00_setup.R')

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

# agregar del drive

drive <- read_csv('data/raw/potencial/potencial_drive.csv') |> 
  rename(tratamiento = treatment,
         sitio = field,
         fecha = data,
         potencial_bar = `water_potential (bar)`) |>
  separate(col='tratamiento',into = c('tratamiento','codigo'),sep=2) |> 
  mutate(temporada = '2022-2023',
         sitio = case_when(sitio == 'La Esperanza' ~ 'la_esperanza',
                           sitio == 'Rio Claro' ~ 'rio_claro',
                           .default = sitio),
         fecha = as.character(fecha),
         potencial_bar = as.numeric(potencial_bar),
         codigo = gsub('H0','H',codigo),
         codigo = gsub('H104A','H103A',codigo),
         codigo = gsub('H106A','H104A',codigo)) |> 
  select(sitio,temporada,fecha,tratamiento,codigo,potencial_bar) |> 
  left_join(pote |> 
              mutate(unidad = as.numeric(as.character(unidad))) |> 
              distinct(sitio,temporada,tratamiento,unidad,codigo)) |> 
  mutate(unidad = factor(unidad,levels=1:3))

pote |> 
  bind_rows(drive) |> 
  distinct(sitio,temporada,fecha,tratamiento,codigo,unidad,
           .keep_all = T) |> 
  arrange(sitio,fecha,tratamiento,unidad)
  distinct(sitio,temporada,tratamiento,unidad,codigo) |> 
  arrange(sitio,temporada,tratamiento,unidad) |> View()

