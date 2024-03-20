
library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(agvAPI)

data_old <- read_rds('data/data_processed/clima.rds')

periodo <- c(as.Date(max(data_old$fecha))-1,substr(now(),1,10))

le_temp <- getDataAGV_clima(station_id ='00205018', var = 'Temperature',
                            time_span = periodo) |>
  mutate(sitio = 'la_esperanza', .before = datetime)
le_vpd <- getDataAGV_clima(station_id ='00205018', var = 'VPD',
                           time_span = periodo) |>
  mutate(sitio = 'la_esperanza', .before = datetime)
le_eto <- getDataAGV_clima(station_id ='00205018', var = 'ETo',
                           time_span = periodo) |>
  mutate(sitio = 'la_esperanza', .before = datetime)
le_pp <- getDataAGV_clima(station_id ='00205018', var = 'Precipitation',
                          time_span = periodo) |>
  mutate(sitio = 'la_esperanza', .before = datetime)


rc_temp <- getDataAGV_clima(station_id ='00203E6E', var = 'Temperature',
                            time_span = periodo) |>
  mutate(sitio = 'rio_claro', .before = datetime)
rc_vpd <- getDataAGV_clima(station_id ='00203E6E', var = 'VPD',
                           time_span = periodo) |>
  mutate(sitio = 'rio_claro', .before = datetime)
rc_eto <- getDataAGV_clima(station_id ='00203E6E', var = 'ETo',
                           time_span = periodo) |>
  mutate(sitio = 'rio_claro', .before = datetime)
rc_pp <- getDataAGV_clima(station_id ='00203E6E', var = 'Precipitation',
                          time_span = periodo) |>
  mutate(sitio = 'rio_claro', .before = datetime)

temp <- rbind(le_temp,rc_temp) |>
  rename(t_media = `avg (°C)`,
         t_max = `max (°C)`,
         t_min = `min (°C)`)
vpd <- rbind(le_vpd,rc_vpd) |>
  rename(vpd_media = `avg (mbar)`,
         vpd_min = `min (mbar)`)
eto <- rbind(le_eto,rc_eto) |>
  rename(eto = `ETo[mm] (mm)`) |>
  mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |>
  mutate(fecha = format(datetime, "%Y-%m-%d")) |>
  select(sitio,fecha,eto)
pp <- rbind(le_pp,rc_pp) |>
  rename(pp = `sum (mm)`)

data_new <- temp |>
  left_join(vpd, by = c('sitio', 'datetime')) |>
  left_join(pp, by = c('sitio', 'datetime')) |>
  mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |>
  mutate(fecha = format(datetime, "%Y-%m-%d"),
         hora = hour(floor_date(datetime,'1 hour')),
         .before = datetime) |>
  select(-datetime) |>
  left_join(eto, by =c('sitio','fecha')) |>
  group_by(sitio,fecha,hora) |>
  summarise(t_media = mean(t_media,na.rm=T),
            t_max = max(t_max,na.rm=T),
            t_min = min(t_min,na.rm=T),
            vpd_media = mean(vpd_media,na.rm=T),
            vpd_min = min(vpd_min,na.rm=T),
            pp = sum(pp,na.rm=T),
            eto = mean(eto,ma.rm=T)) |>
  ungroup() |>
  mutate(temporada = ifelse(fecha > '2023-06-01','2023-2024','2022-2023'),
         .before = fecha)

data_clima <- data_old |>
  bind_rows(data_new) |>
  distinct(sitio,temporada,fecha,hora,.keep_all = T) |>
  arrange(temporada,sitio,fecha,hora) 

write_rds(data_clima, 'data/data_processed/clima.rds')

# data_clima |>
#   mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
#   select(sitio,fecha_hora,t_media) |>
#   ggplot(aes(fecha_hora,t_media)) +
#   geom_line() +
#   facet_wrap(~sitio, scale = 'free_y',ncol = 1) +
#   theme_bw()
# 
# data_clima |>
#   group_by(sitio,fecha) |>
#   summarise(eto = mean(eto,na.rm=T)) |>
#   mutate(fecha = as.Date(fecha)) |>
#   ggplot(aes(fecha,eto)) +
#   geom_line() +
#   facet_wrap(~sitio, scale = 'free_y',ncol = 1) +
#   theme_bw()

