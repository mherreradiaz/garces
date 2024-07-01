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
         sitio = factor(sitio,levels=c('Rio Claro','La Esperanza')),
         valor = ifelse(variable == 'eto' & valor == 0,NA,valor))

cosecha <- read_rds('data/processed/cosecha/produccion.rds') |> 
  distinct(sitio,temporada,fecha) |>
  rowwise() |> 
  mutate(sitio = convert_to_sentence_case(gsub('_',' ',sitio))) |> 
  bind_rows(tibble(sitio = 'Rio Claro',
                   temporada = '2023-2024',
                   fecha = as.Date('2024-01-03'))) |> 
  mutate(sitio = factor(sitio,levels=c('Rio Claro','La Esperanza'))) |> 
  mutate(label = case_when(sitio == "La Esperanza" ~ "pre-harvest",
                           sitio == "Rio Claro" ~ "post-harvest"),
         x_adjust = case_when(sitio == "La Esperanza" ~ -15,
                              sitio == "Rio Claro" ~ 50))

sitios_pal <- wes_palette(n=5,name = 'Darjeeling1')[c(4,5)]
names(sitios_pal) <- c('Rio Claro','La Esperanza')

# -- graficar

p_t_media <- clima |> 
  filter(variable == 't_media') |> 
  ggplot(aes(fecha,valor,color=sitio)) +
  geom_point(size = .8, alpha=.8) +
  geom_line(alpha=.8,linewidth=.4) +
  ylim(0,40) +
  geom_vline(data = cosecha, aes(xintercept = as.numeric(fecha), color = sitio),
             linetype = "dashed") +
  geom_text(data = cosecha, aes(x = fecha + x_adjust, y = Inf, label = label),
            color = "black", vjust = 2, 
            hjust = case_when(cosecha$sitio == "La Esperanza" ~ 1.1,
                              cosecha$sitio == "Rio Claro" ~ -0.1),
            size = 3)  +
  labs(y = 'T (°C)',
       x = "",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 13),
        text = element_text(size = 12)) +
  guides(color = "none")

p_rh_media <- clima |> 
  filter(variable == 'rh_media') |> 
  ggplot(aes(fecha,valor,color=sitio)) +
  geom_point(size = .8, alpha=.8) +
  geom_line(alpha=.8,linewidth=.4) +
  ylim(0,130) +
  geom_vline(data = cosecha, aes(xintercept = as.numeric(fecha), color = sitio),
             linetype = "dashed") +
  geom_text(data = cosecha, aes(x = fecha + x_adjust, y = Inf, label = label),
            color = "black", vjust = 2, 
            hjust = case_when(cosecha$sitio == "La Esperanza" ~ 1.1,
                              cosecha$sitio == "Rio Claro" ~ -0.1),
            size = 3) +
  labs(y = 'RH (%)',
       x = "",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 12)) +
  guides(color = "none")

p_vpd_media <- clima |> 
  filter(variable == 'vpd_medio') |> 
  ggplot(aes(fecha,valor,color=sitio)) +
  geom_point(size = .8, alpha=.8) +
  geom_line(alpha=.8,linewidth=.4) +
  ylim(0,50) +
  geom_vline(data = cosecha, aes(xintercept = as.numeric(fecha), color = sitio),
             linetype = "dashed") +
  geom_text(data = cosecha, aes(x = fecha + x_adjust, y = Inf, label = label),
            color = "black", vjust = 2, 
            hjust = case_when(cosecha$sitio == "La Esperanza" ~ 1.1,
                              cosecha$sitio == "Rio Claro" ~ -0.1),
            size = 3) +
  labs(y = 'VPD (mbar)',
       x = "",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 12)) +
  guides(color = "none")

p_eto <- clima |> 
  filter(variable == 'eto') |> 
  ggplot(aes(fecha,valor,color=sitio)) +
  geom_point(size = .8, alpha=.8) +
  geom_line(alpha=.8,linewidth=.4) +
  ylim(0,7.5) +
  geom_vline(data = cosecha, aes(xintercept = as.numeric(fecha), color = sitio),
             linetype = "dashed") +
  geom_text(data = cosecha, aes(x = fecha + x_adjust, y = Inf, label = label),
            color = "black", vjust = 2, 
            hjust = case_when(cosecha$sitio == "La Esperanza" ~ 1.1,
                              cosecha$sitio == "Rio Claro" ~ -0.1),
            size = 3) +
  labs(y = 'ET0 (mm)',
       x = "month",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(panel.grid.minor.x = element_blank(),
        strip.text = element_blank(),
        legend.position = 'bottom',
        text = element_text(size = 12))

p_pp <- clima |> 
  filter(variable == 'pp') |> 
  ggplot(aes(fecha,valor,fill=sitio)) +
  geom_bar(stat = "identity",position = 'dodge',width=3) +
  ylim(0, 75) +
  geom_vline(data = cosecha, aes(xintercept = as.numeric(fecha), color = sitio),
             linetype = "dashed") +
  geom_text(data = cosecha, aes(x = fecha + x_adjust, y = Inf, label = label),
            color = "black", vjust = 2,
            hjust = case_when(cosecha$sitio == "La Esperanza" ~ 1.1,
                              cosecha$sitio == "Rio Claro" ~ -0.1),
            size = 3) +
  labs(y = 'PP (mm)',
       x = "",
       color = 'site') +
  facet_grid(~temporada, scales = 'free') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 12)) +
  guides(color = "none",
         fill = 'none')

p_t_media/p_rh_media/p_vpd_media/p_pp/p_eto
ggsave(paste0('output/figs/series_clima.png'), width = 10, height = 8)

# valores mensuales (no actualizado)

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

