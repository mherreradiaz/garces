source('script/00_setup.R')

sitio_reclass <- function(x) {
  factor(case_when(x == 'la_esperanza' ~ 'La Esperanza',
                   x == 'rio_claro' ~ 'Rio Claro'),
         levels = c('Rio Claro','La Esperanza'))
}
Sys.setlocale("LC_TIME", "C")

# fluorescencia

data <- read_rds('data/processed/fluorescencia.rds')

data |> distinct(sitio,temporada,fecha) |> 
  group_by(sitio,temporada) |> 
  reframe(n = n())

data |> 
  mutate(sitio = sitio_reclass(sitio),
         fecha = as.Date(fecha)) |> 
  group_by(sitio,temporada,fecha,tratamiento) |> 
  reframe(value = mean(Phi_Po,na.rm=T)) |> 
  ggplot(aes(fecha,value,color=tratamiento)) +
  geom_point(alpha=.5,size=.9) +
  geom_line(linewidth = .7,alpha = .6) +
  facet_grid(sitio~temporada,scales='free_x') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(.15,0)) +
  labs(x = NULL, y = expression(Phi[P0]), color = 'Tratamiento')

ggsave(paste0('output/reunion/fluorescencia.png'), width = 10, height = 6)

# potencial dia

data <- read_rds('data/processed/potencial.rds') |> 
  mutate(potencial = -potencial_bar/10)
tlp <- read_rds('data/processed/tlp.rds') |> 
  group_by(sitio,temporada) |> 
  reframe(tlp = mean(tlp,na.rm=T)) |> 
  mutate(sitio = sitio_reclass(sitio)) |> 
  dplyr::select(-temporada) |> 
  group_by(sitio,tlp) |> 
  reframe(temporada = c('2022-2023','2023-2024'))

data |> distinct(sitio,temporada,fecha) |> 
  group_by(sitio,temporada) |> 
  reframe(n = n())

data |> 
  mutate(sitio = sitio_reclass(sitio),
         fecha = as.Date(fecha)) |> 
  group_by(sitio,temporada,fecha,tratamiento) |> 
  reframe(value = mean(potencial,na.rm=T)) |> 
  ggplot(aes(fecha,value,color=tratamiento)) +
  geom_point(alpha=.5,size=.9) +
  geom_line(linewidth = .7,alpha = .6) +
  geom_hline(data = tlp, aes(yintercept = tlp, color = sitio), 
             linetype = "dashed", linewidth = 0.6) + 
  facet_grid(sitio~temporada,scales='free_x') +
  geom_text(data = tlp,aes(x=as.Date(case_when(
    temporada=='2022-2023'~'2022-11-01',
    temporada=='2023-2024'~'2023-11-01')),
    y=tlp,label='TLP'),
            hjust = 2, vjust = -1.3, size = 4, inherit.aes = FALSE) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(.15,0)) +
  labs(x = NULL, y = expression(Psi[s]), color = 'Tratamiento')

ggsave(paste0('output/reunion/potencial_dia.png'), width = 10, height = 6)

# potencial hora

data <- read_rds('data/processed/potencial_horario.rds') |> 
  mutate(potencial = -bar/10)
tlp <- read_rds('data/processed/tlp.rds') |> 
  group_by(sitio,temporada) |> 
  reframe(tlp = mean(tlp,na.rm=T)) |> 
  mutate(sitio = sitio_reclass(sitio)) |> 
  dplyr::select(-temporada)

dia_info <- data |> 
  mutate(sitio = sitio_reclass(sitio)) |> 
  distinct(sitio,temporada,fecha)

data |> 
  mutate(sitio = sitio_reclass(sitio),
         hora = as.numeric(substr(hora,1,2))) |> 
  group_by(sitio,temporada,hora,tratamiento) |> 
  reframe(value = mean(potencial,na.rm=T)) |> 
  ggplot(aes(hora,value,color=tratamiento)) +
  geom_point(alpha=.5,size=.9) +
  geom_line(linewidth = .7,alpha = .6) +
  geom_hline(data = tlp, aes(yintercept = tlp, color = sitio), 
             linetype = "dashed", linewidth = 0.6) + 
  facet_grid(sitio~temporada,scales='free_x') +
  geom_text(data = dia_info,aes(x=-Inf,y=-Inf,label=fecha),
            hjust = -.2, vjust = -1.3, size = 4, inherit.aes = FALSE) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white')) +
  scale_x_continuous(breaks = c(7:20), expand = c(.1,0)) +
  labs(x = NULL, y = expression(Psi[s]), color = 'Tratamiento')

ggsave(paste0('output/reunion/potencial_hora.png'), width = 10, height = 6)

# fluorescencia

data <- read_rds('data/processed/ceptometro.rds')

data |> distinct(sitio,temporada,fecha) |> 
  group_by(sitio,temporada) |> 
  reframe(n = n())

data |> 
  mutate(sitio = sitio_reclass(sitio)) |> 
  ggplot(aes(fecha,lai,color=tratamiento)) +
  geom_point(alpha=.5,size=.9) +
  geom_line(linewidth = .7,alpha = .6) +
  facet_grid(sitio~temporada,scales='free_x') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(.15,0)) +
  labs(x = NULL, y = 'LAI', color = 'Tratamiento')

ggsave(paste0('output/reunion/lai.png'), width = 10, height = 6)

# metabolomas

produccion <- read_rds('data/processed/cosecha/produccion.rds')




