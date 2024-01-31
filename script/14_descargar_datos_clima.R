
library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(agvAPI)

le_temp <- getDataAGV_clima(station_id ='00205018', var = 'Temperature',
                          time_span = c('2023-10-30',substr(now(),1,10))) |>
  mutate(sitio = 'la_esperanza', .before = datetime)
le_vpd <- getDataAGV_clima(station_id ='00205018', var = 'VPD',
                            time_span = c('2023-10-30',substr(now(),1,10))) |>
  mutate(sitio = 'la_esperanza', .before = datetime)
le_eto <- getDataAGV_clima(station_id ='00205018', var = 'ETo',
                            time_span = c('2023-10-30',substr(now(),1,10))) |>
  mutate(sitio = 'la_esperanza', .before = datetime)
le_pp <- getDataAGV_clima(station_id ='00205018', var = 'Precipitation',
                           time_span = c('2023-10-30',substr(now(),1,10))) |>
  mutate(sitio = 'la_esperanza', .before = datetime)


rc_temp <- getDataAGV_clima(station_id ='00203E6E', var = 'Temperature',
                            time_span = c('2023-10-30',substr(now(),1,10))) |>
  mutate(sitio = 'rio_claro', .before = datetime)
rc_vpd <- getDataAGV_clima(station_id ='00203E6E', var = 'VPD',
                           time_span = c('2023-10-30',substr(now(),1,10))) |>
  mutate(sitio = 'rio_claro', .before = datetime)
rc_eto <- getDataAGV_clima(station_id ='00203E6E', var = 'ETo',
                           time_span = c('2023-10-30',substr(now(),1,10))) |>
  mutate(sitio = 'rio_claro', .before = datetime)
rc_pp <- getDataAGV_clima(station_id ='00203E6E', var = 'Precipitation',
                           time_span = c('2023-10-30',substr(now(),1,10))) |>
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
  mutate(fecha = format(datetime, "%Y-%m-%d")) |>
  select(sitio,fecha,eto)
pp <- rbind(le_pp,rc_pp) |>
  rename(pp = `sum (mm)`)

clima <- temp |>
  left_join(vpd, by = c('sitio', 'datetime')) |>
  left_join(pp, by = c('sitio', 'datetime')) |>
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
              eto = mean(eto,ma.rm=T))

write_rds(clima, 'data/data_processed/clima.rds')
