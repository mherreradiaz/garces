
library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
options(timeout = max(3600, getOption("timeout")))

codigo_sm <- read.csv('data/metadata/codigos_zim_sm.csv', sep = ';')
codigo_tur <- read.csv('data/metadata/codigos_zim_turgor.csv', sep = ';')

data_old <- dir_ls('data/data_raw/zim',regexp = 'zim_update')
old_date <- substr(max(strptime(str_extract(data_old,'[0-9]{12}'), '%Y%m%d%H%M')),1,16)

from_date <- unlist(strsplit(old_date,'[ :]'))
until_date <- unlist(strsplit(substr(lubridate::now(),1,16),'[ :]'))

url <- paste0("https://yara.zim-probe.com/datas/user_data/param%3A%7B%22username%22%3A%22CL-Universidad%20Mayor%20FZ%22%2C%22password%22%3A%2212hepok34%22%2C%22user_id%22%3A%5B%22911%22%5D%2C%22from_date%22%3A%22",
              from_date[1],'%20',from_date[2],'%3A',from_date[3],"%22%2C%22until_date%22%3A%22",
              until_date[1],'%20',until_date[2],'%3A',until_date[3],"%22%7D")

download.file(url,paste0('data/data_raw/zim/zim_update',
                         gsub('-','',until_date[1]),until_date[2],until_date[3],'.csv'), mode = 'wb')
                      
data_new <- read_delim(paste0('data/data_raw/zim/zim_update',
                             gsub('-','',until_date[1]),until_date[2],until_date[3],'.csv'),
                      delim = '\t')

data_sm <- data_new |> 
  select(1,contains('Soil Moisture')) |> 
  pivot_longer(-1,names_to = 'sensor') |> 
  rename_with(.fn = \(x) 'datetime',.cols = starts_with('Times')) |> 
  mutate(sensor =str_extract(str_extract(sensor,'[0-9]{4}\\['),'[0-9]{4}'),
         datetime = as_datetime(datetime,tz = 'America/Santiago')) |> 
  group_by(sensor,hora = floor_date(datetime,'1 hour')) |> 
  summarize(value = mean(value,na.rm = TRUE)) |> 
  mutate(sensor = as.numeric(sensor)) |> 
  left_join(codigo_sm,by = 'sensor') |> 
  separate(codigo,2,into =c('tratamiento','codigo'))

data_tur <- data_new |> 
  select(1,contains('Yara')) |> 
  mutate(across(where(is.character),as.numeric)) |> 
  pivot_longer(-1,names_to = 'sensor') |> 
  rename_with(.fn = \(x) 'datetime',.cols = starts_with('Times')) |>
  mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |> 
  mutate(sensor =str_extract(str_extract(sensor,'[0-9]{4}\\['),'[0-9]{4}')) |> 
  group_by(sensor,hora = floor_date(datetime,'1 hour')) |> 
  summarize(value = mean(value,na.rm = TRUE)) |> 
  mutate(sensor = as.numeric(sensor)) |> 
  left_join(codigo_tur,by = 'sensor') |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |> 
  mutate(zim = substr(codigo,6,7)) 

data_tur <- data_tur |> 
  mutate(hora = hora |> with_tz(tzone = 'UTC') |> force_tz(tzone = 'America/Santiago')) 

data_sm <- rbind(read_rds('data/data_processed/rds/data_zim_sm.rds'),
                 data_sm)
data_tur <- rbind(read_rds('data/data_processed/rds/data_zim_turgor.rds'),
                 data_tur)

write_rds(data_sm,'data/data_processed/rds/data_zim_sm.rds')
write_rds(data_tur,'data/data_processed/rds/data_zim_turgor.rds')


