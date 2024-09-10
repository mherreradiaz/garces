source('script/00_setup.R')
library(viridis)

Sys.setlocale("LC_TIME", "C")
convert_to_sentence_case <- function(string) {
  s <- strsplit(string, " ")[[1]]
  s <- tolower(s) # Primero convertimos toda la cadena a minúsculas
  s <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  return(s)
}
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
reclasificar <- function(x, y) {
  y <- as.data.frame(y)
  x_reclassified <- x
  
  for (i in seq_len(nrow(y))) {
    x_reclassified[x == y[i, 1]] <- y[i, 2]
  }
  
  return(x_reclassified)
}

# datos

var_names <- c('eto','vpd_medio','t_media','rh_media','cab','msi','dwsi','msr705','ndmi','nmdi','pp')
var_names_plot <- c('ET0','VPD','T','RH','CCC','MSI','DWSI','mSR705','NDMI','NMDI','PP')

data <- read_rds('data/processed/modelo_potencial_smooth.rds') |> 
  select(sitio:potencial_bar,all_of(var_names)) |> 
  mutate(potencial_bar = -potencial_bar/10)

# variabiliadd temporal

data_tmap <- data |> 
  group_by(sitio,temporada,fecha) |> 
  reframe(across(c(eto:pp), mean, na.rm = T)) |> 
  pivot_longer(cols = eto:pp,names_to = "variable",values_to = "valor") |> 
  rowwise() |> 
  mutate(fecha = as.Date(fecha),
         sitio = factor(convert_to_sentence_case(gsub('_',' ',sitio)),
                        ,levels=c('Rio Claro','La Esperanza'))) |> 
  filter(!between(month(fecha),5,9)) |> 
  mutate(variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=rev(var_names_plot))) |> 
  group_by(sitio,temporada,variable) |> 
  mutate(valor = scale(valor)) |> 
  ungroup() |> 
  na.omit()

data_tmap |> 
  ggplot(aes(x = fecha, y = variable, fill = valor)) +
  geom_tile() +
  scale_fill_viridis(option = "viridis", name = "standarized values",
                     limits = c(-3, 3), 
                     oob = scales::squish) +
  labs(x = "month",
       y = "variables") +
  theme_light() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(sitio~temporada,scales='free_x')
ggsave('output/figs/series_vi_smooth_signific.png', width = 8, height = 5)  

# correlación

data_cor <- data |>
  group_by(sitio, temporada) |> 
  reframe(across(var_names, ~ cor(.x, potencial_bar, use = "complete.obs"), 
                   .names = "{.col}")) |> 
  pivot_longer(cols = eto:pp,names_to = "variable",values_to = "cor") |> 
  rowwise() |> 
  mutate(cor = ifelse(is.na(cor),0,cor),
         sitio = factor(convert_to_sentence_case(gsub('_',' ',sitio)),
                        ,levels=c('Rio Claro','La Esperanza')),
         variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=rev(var_names_plot)))
  
data_cor |> 
  ggplot(aes(temporada,variable,fill=cor)) +
  geom_tile() +
  geom_text(aes(label = round(cor, 2)), color = "white", size = 3) + # Añade los valores de cor
  
  scale_fill_viridis(option = "viridis", name = "r", 
                     limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  facet_grid(~sitio) +
  theme_light() +
  labs(y = 'variable',
       x = 'season')

ggsave(paste0('output/figs/cor_variables_signif.png'), width = 5, height = 5)

# correlación meteo

library(paletteer)

meteo_names <- c('eto','vpd_medio','t_media','rh_media','pp')

cor_meteo <- data |>
  group_by(sitio, temporada,fecha) |>
  reframe(across(c(potencial_bar,all_of(meteo_names)), ~ mean(.x,na.rm=T),
                 .names = '{.col}')) |> 
  group_by(sitio, temporada) |> 
  reframe(across(eto:pp, ~ cor(.x, potencial_bar, use = "complete.obs"), 
                 .names = '{.col}')) |> 
  pivot_longer(cols = eto:pp,names_to = "variable",values_to = "cor") |> 
  rowwise() |> 
  mutate(cor = ifelse(is.na(cor),0.00001,cor),
         sitio = factor(convert_to_sentence_case(gsub('_',' ',sitio)),
                        ,levels=c('Rio Claro','La Esperanza')),
         variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=rev(var_names_plot)))

cor_meteo |> 
  ggplot(aes(temporada,variable,fill=cor)) +
  geom_tile() +
  geom_text(aes(label = ifelse(cor==0.00001,"",round(cor, 2))), color = "black", size = 3) +
  scale_fill_gradientn(colors = paletteer_c("ggthemes::Red-Blue-White Diverging", 30),
                       limits = c(-1, 1), 
                       breaks = seq(-1, 1, by = 0.5), name = "r") +
  facet_grid(~sitio) +
  theme_light() +
  labs(y = '',
       x = 'season')

ggsave(paste0('output/figs/cor_meteo.png'), width = 5, height = 5)

# correlacion vi

vi_names <- c('cab','msi','dwsi','msr705','ndmi','nmdi')

cor_vi <- data |>
  na.omit() |> 
  group_by(sitio, temporada,fecha) |> 
  reframe(across(all_of(vi_names), ~ cor(.x, potencial_bar, use = "complete.obs"), 
                 .names = "{.col}")) |> 
  pivot_longer(cols = cab:nmdi,names_to = "variable",values_to = "cor") |> 
  rowwise() |> 
  mutate(cor = ifelse(is.na(cor),0,cor),
         sitio = factor(convert_to_sentence_case(gsub('_',' ',sitio)),
                        ,levels=c('Rio Claro','La Esperanza')),
         variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=rev(var_names_plot)),
         fecha = as.Date(fecha))

cor_vi |> 
  ggplot(aes(fecha,variable,fill=cor)) +
  geom_tile() +
  # geom_text(aes(label = round(cor, 2)), color = "white", size = 3) +
  scale_fill_viridis(option = "viridis", name = "r", 
                     limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  labs(x = "month",
       y = "variables") +
  theme_light() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(sitio~temporada,scales='free_x')

cor_vi |> 
  mutate(grupo = case_when(
    variable %in% c("MSI", "NMDI") ~ "Grupo 1",
    TRUE ~ "Grupo 2")) |> 
  ggplot(aes(fecha,cor,color=variable)) +
  geom_line(alpha=.6) +
  # geom_smooth(aes(group = grupo, color = grupo), 
              # method = "loess", span =.3,se = F, alpha=.4) +
  geom_point(alpha=.7) +
  labs(x = "month",
       y = "r") +
  theme_light() +
  ylim(-2,2) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(sitio~temporada,scales='free_x')

ggsave(paste0('output/figs/cor_variables_signif.png'), width = 5, height = 5)

# correlación vi significancia

vi_names <- c('cab','msi','dwsi','msr705','ndmi','nmdi')

data_long <- left_join(data, data |> 
            na.omit() |> 
            group_by(sitio,temporada,fecha,tratamiento) |> 
            reframe(n = n()) |> 
            group_by(sitio,temporada,fecha) |> 
            reframe(n = n()) |> 
            filter(n==5)) |> 
  filter(n==5) |> 
  select(-n) |>
  mutate(potencial_bar = -potencial_bar/10)

cor_vi <- data_long |> 
  group_by(sitio,temporada,fecha) |> 
  reframe(across(all_of(vi_names), ~ cor.test(.x, potencial_bar,method='pearson')$estimate, 
                 .names = "{.col}")) |> 
  pivot_longer(cols = cab:nmdi,names_to = "variable",values_to = "pearson_r") |> 
  left_join(data_long |> 
              group_by(sitio,temporada,fecha) |> 
              reframe(across(all_of(vi_names), ~ cor.test(.x, potencial_bar,method='pearson')$p.value, 
                             .names = "{.col}")) |> 
              pivot_longer(cols = cab:nmdi,names_to = "variable",values_to = "pearson_p")) |> 
  left_join(data_long |> 
              group_by(sitio,temporada,fecha) |> 
              reframe(across(all_of(vi_names), ~ cor.test(.x, potencial_bar,method='spearman')$estimate, 
                             .names = "{.col}")) |> 
              pivot_longer(cols = cab:nmdi,names_to = "variable",values_to = "spearman_r")) |> 
  left_join(data_long |> 
              group_by(sitio,temporada,fecha) |> 
              reframe(across(all_of(vi_names), ~ cor.test(.x, potencial_bar,method='spearman')$p.value, 
                             .names = "{.col}")) |> 
              pivot_longer(cols = cab:nmdi,names_to = "variable",values_to = "spearman_p")) |> 
rowwise() |> 
  mutate(sitio = factor(convert_to_sentence_case(gsub('_',' ',sitio)),
                        ,levels=c('Rio Claro','La Esperanza')),
         variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=var_names_plot),
         fecha = as.Date(fecha),
         pearson_sig = ifelse(pearson_p<=.05,'*',""),
         spearman_sig = ifelse(spearman_p<=.05,'*',""))

cor_vi |> 
  filter(pearson_p<=.05) |> 
  group_by(sitio,temporada,variable) |> 
  reframe(r_mean = round(mean(pearson_r,na.rm=T),2),
          sd = round(sd(pearson_r,na.rm=T),2),
          n = n()) |> 
  mutate(comentario = ifelse(sitio == 'La Esperanza' & 
                               temporada == '2023-2024' & 
                               variable == 'CCC',
                             'positivo a negativo',""))

cor_vi |> 
  filter(pearson_p<=.05) |>
  ggplot(aes(fecha,pearson_r,color=variable)) +
  geom_line(alpha=.6) +
  # geom_smooth(aes(group = grupo, color = grupo), 
  # method = "loess", span =.3,se = F, alpha=.4) +
  geom_point(alpha=.7) +
  labs(x = "month",
       y = "r") +
  theme_light() +
  ylim(-1,1) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(sitio~temporada,scales='free_x')

ggsave(paste0('output/figs/cor_variables_signif.png'), width = 5, height = 5)

cor_vi |> 
  filter(pearson_p<=.05,
         variable %in% c('CCC','NMDI'),
         sitio == 'La Esperanza',
         temporada == '2023-2024') |> 
  ggplot(aes(fecha,pearson_r,color=variable,fill=variable)) +
  geom_line(linewidth = .7,alpha = .6,linetype = 'dashed') +
  geom_point(shape = 21,alpha=.5,size=2) +
  labs(y = 'Pearson correlation coefficient', x = NULL,color=NULL,fill=NULL) +
  scale_x_date(date_labels = "%b",
               limits = as.Date(c('2023-10-01', '2024-04-30')),
               minor_breaks = seq(as.Date('2023-09-01'),as.Date('2024-05-01'),by='month')) +
  ylim(-1,1) +
  geom_vline(aes(xintercept = as.Date('2023-12-12')), 
             linetype = "dashed", color = "black") +
  geom_text(aes(as.Date('2024-01-10'),-0.75,label='2024-01-10'), 
            size = 3, color = 'grey30') +
  geom_text(aes(as.Date('2024-04-20'),0.8,label='2024-04-12'), 
            size = 3, color = 'grey30') +
  geom_text(aes(as.Date('2023-12-12'),-1,label='pre-harvest'), 
            size = 3, color = 'grey30',hjust=1.2) +
  geom_text(aes(as.Date('2023-12-12'),-1,label='post-harvest'), 
            size = 3, color = 'grey30',hjust = -0.2) +
  theme_bw()
  
ggsave('output/figs/cor_ccc_raro.png', width = 7, height = 3)

data_long |> 
  filter(fecha == '2024-04-12', sitio == 'la_esperanza') |> 
  ggplot(aes(tratamiento,cab)) +
  geom_boxplot()

# primer gráfico vi

vi_names <- c('cab','msi','dwsi','msr705','ndmi','nmdi')

data_raw <- bind_rows(read_rds('data/processed/sentinel_vi_raw.rds'),
                      read_rds('data/processed/sentinel_biopar_raw.rds')) |> 
  distinct(across(sitio:codigo)) |> 
  left_join(read_rds('data/processed/sentinel_vi_raw.rds')) |> 
  left_join(read_rds('data/processed/sentinel_biopar_raw.rds')) |> 
  mutate(data = 'raw') |> 
  # filter(tratamiento=='T1',
  #        unidad == 2) |> 
  select(sitio:unidad,data,vi_names) |> 
  pivot_longer(cols=cab:nmdi,names_to='variable',values_to='value')

data_smooth <- bind_rows(read_rds('data/processed/sentinel_vi_smooth.rds'),
                      read_rds('data/processed/sentinel_biopar_smooth.rds')) |> 
  distinct(across(sitio:codigo)) |> 
  left_join(read_rds('data/processed/sentinel_vi_smooth.rds')) |> 
  left_join(read_rds('data/processed/sentinel_biopar_smooth.rds')) |> 
  mutate(data = 'smooth') |> 
  # filter(tratamiento=='T1',
  #        unidad == 2) |> 
  select(sitio:unidad,data,vi_names) |> 
  pivot_longer(cols=cab:nmdi,names_to='variable',values_to='value')

data <- bind_rows(data_raw,data_smooth) |> 
  mutate(fecha = as.Date(fecha)) |> 
  pivot_wider(names_from=data,values_from=value) |>
  mutate(variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=var_names_plot),
         sitio = factor(case_when(sitio == 'rio_claro' ~ 'Rio Claro',
                           sitio == 'la_esperanza' ~ 'La Esperanza'),
                        levels = c('Rio Claro','La Esperanza'))) 

metrics <- data |> 
  na.omit() |> 
  group_by(variable) |> 
  summarise(r_squared = cor(raw, smooth)^2,
            mae = mean(abs(raw - smooth)),
            rmse = sqrt(mean((raw - smooth)^2)),
            max_raw = max(raw, na.rm = TRUE),
            max_smooth = max(smooth, na.rm = TRUE))

data |> 
  na.omit() |> 
  ggplot(aes(raw,smooth)) +
  geom_point(shape = 21,fill = 'grey40',alpha=.5,size=.9) + 
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linetype = "dashed",
              linewidth = .7) +
  geom_text(data = metrics,
            aes(x = Inf,
                y = -Inf,
                label = sprintf("R²: %.2f\nMAE: %.2f\nRMSE: %.2f", r_squared, mae, rmse)),
            hjust = 1.1, vjust = -0.5,size = 2.5) +
  facet_wrap(~variable, ncol = 3, scales = 'free') +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.background = element_rect(fill = 'white'))

ggsave(paste0('output/figs/lm_vi.png'), width = 10, height = 7, bg = "white")

breaks <- as.Date(c("2022-10-01", "2023-01-01", "2023-04-01",
                  "2023-10-01", "2024-01-01", "2024-04-01"))
monthly_breaks <- c(seq(as.Date("2022-09-01"), 
                      as.Date("2023-05-01"), 
                      by = "month"),
                    seq(as.Date("2023-09-01"), 
                        as.Date("2024-05-01"), 
                        by = "month"))

data_label <- data |> 
  distinct(sitio,temporada,variable) |> 
  mutate(fecha = as.Date(ifelse(temporada == '2022-2023',
                        '2023-01-01','2024-01-01')))

data |> 
  filter(tratamiento == 'T1',
         unidad == 2) |> 
  ggplot(aes(fecha,raw,color = sitio, fill = sitio)) + 
  geom_vline(data = cosecha, aes(xintercept = fecha,color=sitio), 
             linetype = "dashed") +
  geom_point(shape = 21,alpha=.5,size=.9) +
  geom_line(aes(fecha,smooth),linewidth = .7,alpha = .6) +
  facet_wrap(~variable, ncol = 3, scales = 'free_y') +
  labs(y = NULL, x = NULL,color = NULL,fill = NULL) +
  scale_x_date(date_labels = "%b",
               breaks = breaks,
               minor_breaks = monthly_breaks) +
  scale_y_continuous(expand=c(.2,0)) +
  geom_vline(data = cosecha, aes(xintercept = fecha,color=sitio), 
             linetype = "dashed") +
  geom_label(data = data_label,
             aes(fecha,y = Inf, label = temporada),
             alpha = 1, color = 'grey40', fill = 'white',
             hjust = .5, vjust = 1.3, size = 2.5,label.size = NA) +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.background = element_rect(fill = 'white'),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.margin = margin(t = -3))

ggsave(paste0('output/figs/ts_vi.png'), width = 10, height = 7, bg = "white")

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

data |> 
  select(-raw) |> 
  group_by(sitio,temporada,fecha,variable) |> 
  reframe(value = mean(smooth,na.rm=T)) |> 
  group_by(sitio,temporada,variable) |> 
  mutate(value = as.numeric(scale(value))) |> 
  ungroup() |> 
  na.omit() |> 
  mutate(variable = factor(variable,levels=rev(var_names_plot))) |>
  filter((temporada=='2022-2023' & between(fecha,
                                           as.Date('2022-09-20'),
                                           as.Date('2023-04-20')))|
           (temporada=='2023-2024' & between(fecha,
                                             as.Date('2023-09-20'),
                                             as.Date('2024-04-20')))) |> 
  ggplot(aes(x = fecha, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_viridis(option = "viridis", name = "std values",
                     limits = c(-2.5, 2.5), 
                     oob = scales::squish) +
  labs(x = NULL,
       y = NULL) +
  theme_bw() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  facet_grid(sitio~temporada,scales='free_x') +
  theme(strip.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_vline(data = cosecha, aes(xintercept = fecha), 
             linetype = "dashed", color = "black") +
  geom_text(aes(case_when(temporada == '2022-2023' &
                            sitio == 'Rio Claro' ~ as.Date('2022-12-23'),
                          temporada == '2022-2023' &
                            sitio == 'La Esperanza' ~ as.Date('2022-12-12')),
                y='CCC',label='pre-harvest'),size = 3,color = 'white',hjust=1.2) +
  geom_text(aes(case_when(temporada == '2022-2023' &
                            sitio == 'Rio Claro' ~ as.Date('2022-12-23'),
                          temporada == '2022-2023' &
                            sitio == 'La Esperanza' ~ as.Date('2022-12-12')),
                y='CCC',label='post-harvest'),size = 3,color = 'white',hjust=-0.2) +
  geom_text(aes(case_when(temporada == '2023-2024' &
                            sitio == 'Rio Claro' ~ as.Date('2024-01-03'),
                          temporada == '2023-2024' &
                            sitio == 'La Esperanza' ~ as.Date('2023-12-12')),
                y='CCC',label='pre-harvest'),size = 3,color = 'white',hjust=1.2) +
  geom_text(aes(case_when(temporada == '2023-2024' &
                            sitio == 'Rio Claro' ~ as.Date('2024-01-03'),
                          temporada == '2023-2024' &
                            sitio == 'La Esperanza' ~ as.Date('2023-12-12')),
                y='CCC',label='post-harvest'),size = 3,color = 'white',hjust=-0.2)

ggsave(paste0('output/figs/tmap_vi.png'), width = 10, height = 5, bg = "white") 
