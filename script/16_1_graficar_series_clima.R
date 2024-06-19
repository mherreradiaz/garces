source('script/00_setup.R')
library(wesanderson)
library(patchwork)

# -- setup 

Sys.setlocale("LC_TIME", "C")
convert_to_sentence_case <- function(string) {
  s <- strsplit(string, " ")[[1]]
  s <- tolower(s) # Primero convertimos toda la cadena a minúsculas
  s <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  return(s)
}

clima <- read_rds('data/processed/clima_dia.rds') |> 
  rowwise() |> 
  mutate(sitio = convert_to_sentence_case(gsub('_',' ',sitio))) |> 
  pivot_longer(cols=c('t_media','rh_media','vpd_medio','eto','pp'),
               names_to='variable',
               values_to='valor') |> 
  filter(between(fecha,'2022-10-10','2023-04-10') | 
           between(fecha,'2023-10-10','2024-04-10')) |> 
  mutate(fecha = as.Date(fecha),
         sitio = factor(sitio,levels=c('Rio Claro','La Esperanza')))

clima_uno <- clima |> 
  filter(variable %in% c('t_media','rh_media','vpd_medio'))
clima_dos <- clima |> 
  filter(variable %in% c('eto','pp'))

cosecha <- read_rds('data/processed/cosecha/produccion.rds') |> 
  distinct(sitio,temporada,fecha) |> 
  bind_rows(tibble(sitio = 'Rio Claro',
                   temporada = '2023-2024',
                   fecha = as.Date('2024-01-03'))) |> 
  mutate(sitio = factor(sitio,levels=c('Rio Claro','La Esperanza')))

sitios_pal <- wes_palette(n=5,name = 'Darjeeling1')[c(4,5)]
names(sitios_pal) <- c('Rio Claro','La Esperanza')

# -- graficar

p_t_media <- clima |> 
  filter(variable == 't_media') |> 
  ggplot(aes(fecha,valor,color=sitio)) +
  geom_point(size = .9) +
  geom_line(alpha=.8,linewidth=.4) +
  labs(y = 'T (°C)',
       x = "",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank())

p_rh_media <- clima |> 
  filter(variable == 'rh_media') |> 
  ggplot(aes(fecha,valor,color=sitio)) +
  geom_point(size = .9) +
  geom_line(alpha=.8,linewidth=.4) +
  labs(y = 'RH (%)',
       x = "",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank())

p_vpd_media <- clima |> 
  filter(variable == 'vpd_medio') |> 
  ggplot(aes(fecha,valor,color=sitio)) +
  geom_point(size = .9) +
  geom_line(alpha=.8,linewidth=.4) +
  labs(y = 'VPD (mbar)',
       x = "",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank())

p_eto <- clima |> 
  filter(variable == 'eto') |> 
  ggplot(aes(fecha,valor,color=sitio)) +
  geom_point(size = .9) +
  geom_line(alpha=.8,linewidth=.4) +
  labs(y = 'ET0 (mm)',
       x = "",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank())

p_pp <- clima |> 
  filter(variable == 'pp') |> 
  ggplot(aes(fecha,valor,fill=sitio)) +
  geom_bar(stat = "identity",position = 'dodge',width=3) +
  labs(y = 'PP (mm)',
       x = "",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank())

p_t_media/p_rh_media/p_vpd_media/p_eto/p_pp
ggsave(paste0('output/figs/weather_ts.png'), width = 12, height = 10)

clima |> 
  filter(variable == 'pp') |> 
  ggplot(aes(fecha,valor,fill=sitio)) +
  geom_bar(stat = "identity",position = 'dodge',width=3) +
  labs(y = 'PP (mm)',
       x = "month",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(panel.grid.minor.x = element_blank())
ggsave(paste0('output/figs/base_weather_ts.png'), width = 12, height = 10)

clima_uno |> 
  ggplot(aes(fecha,valor,color=variable)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_grid(sitio~temporada,scale = 'free_x',labeller = as_labeller(names)) +
  geom_vline(data = df_fechas, aes(xintercept = fecha_vertical), color = "red", linetype = "dashed")
  labs(y = 'value (T° for t_mean; mbar for vpd and % for rh)',
       x = 'month') +
  theme_light() +
  theme(legend.position = "bottom")
  


# valores mensuales

clima <- read_rds('data/processed/clima_dia.rds') |>
  mutate(fecha_mes = floor_date(as.Date(fecha), unit = "month"),
         mes = month(as.Date(fecha))) |> 
  filter(!mes %in% c(6,7)) |> 
  group_by(sitio,temporada,fecha_mes) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T),
            eto = mean(eto,na.rm=T),
            pp = mean(pp,na.rm=T)) |> 
  ungroup() |> 
  pivot_longer(cols=c('t_media','rh_media','vpd_medio','eto','pp'),
               names_to='variable',
               values_to='valor')
# data <- data |> 
#   group_by(sitio,temporada,variable) |> 
#   mutate(valor = as.numeric(scale(valor))) |> 
#   ungroup()

data |> 
  ggplot(aes(fecha_mes,valor,color=variable)) +
  geom_point() +
  geom_line() +
  facet_grid(sitio~temporada,scales='free', labeller = as_labeller(names))

# valores diarios

data_potencial <- read_rds('data/processed/potencial_xgb_predict.rds') |> 
  select(sitio,temporada,fecha) |>
  distinct() |> 
  mutate(id=1)

data <- read_rds('data/processed/clima_dia.rds') |> 
  pivot_longer(cols=c('t_media','rh_media','vpd_medio','eto','pp'),
               names_to='variable',
               values_to='valor') |>  
  left_join(data_potencial,by=c('sitio','temporada','fecha')) |>
  filter(!is.na(id)) |>
  select(-id)

data_eto <- data |> 
  filter(variable %in% c('pp','eto')) |> 
  rename(acum = variable)

data |> 
  mutate(fecha = as.Date(fecha)) |> 
  filter(!variable %in% c('pp','eto')) |> 
  ggplot(aes(x = fecha, y = valor, color = variable)) +
  geom_point() +
  geom_bar(data = data_eto,
           aes(as.Date(fecha),valor,fill = acum), stat = 'identity') +
  geom_point(alpha = 0.5, size = .5) +  
  geom_smooth(method = "loess", span = .1,se = FALSE) + 
  facet_grid(sitio~temporada, scales = "free_x",labeller=as_labeller(names)) +
  labs(x = "month",
       y = "value",
       color = "variable") +
  scale_x_date(labels = date_format("%b %Y")) +
  theme_light() +
  theme(strip.text = element_text(size = 10))
ggsave(paste0('output/figs/series_clima.png'),scale =3:5)

