source('script/00_setup.R')
library(viridis)
library(wesanderson)
Sys.setlocale("LC_TIME", "C")

# -- setup

pal <- c('dodgerblue2',
         'green3',
         wes_palette(n=4,name = 'Chevalier1')[2],
         wes_palette(n=5,name = 'Darjeeling1')[4],
         'red3')
names(pal) <- c('T0','T1','T2','T3','T4')

# graficar

data_smooth  <- read_rds('data/processed/sentinel_vi_smooth.rds') |> 
  mutate(fecha = as.Date(fecha))

data_raw  <- read_rds('data/processed/sentinel_vi_raw.rds') |>
  na.omit() |> 
  mutate(fecha = as.Date(fecha))

var <- c('msi','dwsi','msr705','ndmi','nmdi')

for (i in seq_along(var)) {

  data_smooth |> 
    select(sitio:codigo,all_of(var[i])) |> 
    # distinct(sitio, temporada, fecha, tratamiento, !!sym(var[i]), .keep_all = T) |>
    ggplot(aes(fecha,!!sym(var[i]), color = tratamiento)) +
    # geom_point(size = .7) +
    geom_line(linewidth = .5) +
    geom_point(data = data_raw, aes(fecha,!!sym(var[i]),color = tratamiento), size = .5) +
    facet_grid(unidad~sitio+temporada, scales = 'free') +
    scale_x_date(date_breaks = "2 month", date_labels = "%b", expand = c(.15,0)) +
    scale_color_manual(values=pal) +
    labs(x = NULL, y = toupper(var[i]), color = 'treatment',
         title = glue::glue('{toupper(var[i])} - Smooth (line) & RAW (points)')) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'),
          text = element_text(size = 13))
  
  ggsave(glue::glue('output/figs/suavizado/{toupper(var[i])}.png'),
         width = 14, height = 9)

}
  