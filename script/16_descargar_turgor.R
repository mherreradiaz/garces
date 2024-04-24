library(agvAPI)
options(timeout = max(3600, getOption("timeout")))
source('script/funciones/read_yara.R')
source('reporte/book/paquetes.R')
dia <- function(año, mes) {
  
  dia <- as.Date(paste(año, mes, "01", sep = "-"))
  
  primer_dia <- dia - days(1)
  ultimo_dia <- dia + months(1)
  
  return(c(primer_dia, ultimo_dia))
}
si <- function(condition, true_value, false_value) {
  if (condition) {
    return(true_value)
  } else {
    return(false_value)
  }
}

# 2022-2023

# download yara data

metadata <- read_csv2('data/metadata/metadata_yara.csv')
codigo_tur_2022 <- read_csv('data/metadata/codigos_zim_turgor_2022.csv')
codigo_tur_2023 <- read_csv2('data/metadata/codigos_zim_turgor.csv')

# DATOS TURGOR ####

data_codigo_tur_2022 <- left_join(codigo_tur_2022,metadata,by=c('sensor' = 'identificador'))
data_codigo_tur_2023 <- left_join(codigo_tur_2023,metadata,by=c('sensor' = 'identificador'))

device_id_tur_2022 <- data_codigo_tur_2022 |> 
  filter(grepl('Yara Water-Sensor',tipo)) |> 
  pull(device_id)

device_id_tur_2023 <- data_codigo_tur_2023 |> 
  filter(grepl('Yara Water-Sensor',tipo)) |> 
  pull(device_id)

for (año in 2022:2023) {
  
  for (mes in c(9:12,1:4)) {
    
    if (mes == 1) {año <- año + 1}
    if (año == 2022 & mes < 12) {next}
    
    fecha_actual <- as.Date(paste(año,mes,'01',sep = '-'))
    
    fechas <- dia(año,mes) 
    
    dest <- paste0('data/data_raw/zim/mes/zim_turgor_update',
                   año,'_',mes,'.csv')
    
    read_yara(si(fecha_actual<'2023-06-01',device_id_tur_2022,device_id_tur_2023),
              from_date = fechas[1],
              until_date = fechas[2],
              dest_file = dest)
  }
}

files <- list.files('data/data_raw/zim/mes/',full.names = T)

data_new <- list()

for (a in 1:length(files)) {
  
  año <- substring(gsub(".*update(\\d{4}).*", "\\1", files[a]), 1, 4)
  mes <- substring(gsub(".*_([0-9]{2}).*", "\\1", files[a]), 1, 2)
  
  fecha_actual <- as.Date(paste(año,mes,'01',sep = '-'))
  
  data_new[[a]] <- read_delim(files[a],delim = '\t') |> 
    select(1,contains('Yara')) |> 
    mutate(across(where(is.character),as.numeric)) |> 
    pivot_longer(-1,names_to = 'sensor') |> 
    rename_with(.fn = \(x) 'datetime',.cols = starts_with('Times')) |>
    mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |> 
    mutate(sensor =str_extract(str_extract(sensor,'[0-9]{4}\\['),'[0-9]{4}')) |> 
    group_by(sensor,fecha = floor_date(datetime,'30 minutes')) |> 
    summarize(value = mean(value,na.rm = T)) |> 
    mutate(sensor = as.numeric(sensor)) |> 
    left_join(si(fecha_actual < '2023-06-01',codigo_tur_2022,codigo_tur_2023),by = 'sensor') |> 
    separate(codigo,2,into =c('tratamiento','codigo')) |> 
    rename(turgor = value) |>
    mutate(temporada = si(fecha_actual < '2023-06-01','2022-2023','2023-2024'),
           unidad = factor(unidad, levels = 1:3),
           hora = format(as.POSIXct(fecha), format = "%H:%M"),
           zim = substr(codigo,nchar(codigo)-1,nchar(codigo)),
           codigo = substr(codigo,1,nchar(codigo)-2),
           fecha = format(as.POSIXct(fecha), format = "%Y-%m-%d")) |>
    select(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,zim,sensor,turgor) |>
    arrange(fecha,sitio,hora,tratamiento,unidad,zim) |>
    ungroup()
  
}

data_turgor <- bind_rows(data_new) |>
  distinct(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,zim,sensor,.keep_all = T) |>
  arrange(fecha,sitio,hora,tratamiento,unidad,zim)

write_rds(data_turgor,'data/data_processed/zim_turgor.rds')
