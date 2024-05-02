source('script/funciones/read_yara.R')
source('script/funciones/paquetes.R')
options(timeout = max(3600, getOption("timeout")))

# descargar datos yara y clima del último mes

metadata <- read_csv2('data/metadata/metadata_yara.csv')
codigo_tur_2022 <- read_csv('data/metadata/codigos_zim_turgor_2022.csv')
codigo_tur_2023 <- read_csv2('data/metadata/codigos_zim_turgor.csv')

data_codigo_tur_2022 <- left_join(codigo_tur_2022,metadata,by=c('sensor' = 'identificador'))
data_codigo_tur_2023 <- left_join(codigo_tur_2023,metadata,by=c('sensor' = 'identificador'))

device_id_tur_2022 <- data_codigo_tur_2022 |> 
  filter(grepl('Yara Water-Sensor',tipo)) |> 
  pull(device_id)

device_id_tur_2023 <- data_codigo_tur_2023 |> 
  filter(grepl('Yara Water-Sensor',tipo)) |> 
  pull(device_id)

año <- year(now())
mes <- month(now())

fechas <- dia(año,mes) 

dest <- paste0('data/data_raw/turgor/turgor_',
               año,'_',sprintf('%02d',mes),'.csv')

read_yara(si(fechas[1] < '2023-06-01',device_id_tur_2022,device_id_tur_2023),
          from_date = fechas[1],
          until_date = fechas[2],
          dest_file = dest)

# descargar datos clima

año <- year(now())
mes <- month(now())

fechas <- dia(año,mes) 

data_clima_le <- clima('00205018',c('Temperature','VPD','Eto','Precipitation','Humidity'),fechas) |> 
  mutate(sitio = 'la_esperanza',
         .before = datetime)
data_clima_rc <- clima('00203E6E',c('Temperature','VPD','Eto','Precipitation','Humidity'),fechas) |> 
  mutate(sitio = 'rio_claro',
         .before = datetime)

data_clima <- bind_rows(data_clima_le,data_clima_rc)

write_rds(data_clima,paste0('data/data_raw/clima/clima_',año,'_',sprintf('%02d',mes),'.rds'))