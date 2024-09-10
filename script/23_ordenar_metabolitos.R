source('script/00_setup.R')

int <- function(...) {
  Reduce(intersect, list(...))
}

rc_2022 <- read_xlsx('data/raw/metaboloma/metabolitos.xlsx',sheet = 3)
le_2022 <- read_xlsx('data/raw/metaboloma/metabolitos.xlsx',sheet = 2)
meta_2023 <- read_xlsx('data/raw/metaboloma/metabolitos.xlsx',sheet = 1)

var <- Reduce(intersect,list(names(rc_2022),names(le_2022),names(meta_2023)))

metabolitos <- meta_2023 |> select(all_of(var)) |> 
  bind_rows(rc_2022 |> select(all_of(var))) |> 
  bind_rows(le_2022 |> select(all_of(var))) |> 
  arrange(sitio,temporada,tratamiento,unidad) |> 
  mutate(unidad = as.factor(unidad))

write_rds(metabolitos,'data/processed/metaboloma.rds')