library(tidyverse)
library(driversbr)
library(fleetbr)
library(roadtrafficdeaths)
library(arrow)


# condutores
condutores <- 
  drivers |> 
  distinct() |> 
  pivot_wider(names_from = categoria_cnh, values_from = condutores) |> 
  summarise(
    .by = ano,
    across(starts_with("categoria"), ~ sum(., na.rm = T))
  ) |> 
  mutate(ciclo = rowSums(na.rm = T, across(contains("x")))) |> 
  select(-starts_with("categoria_x")) |> 
  rename(categoria_x = ciclo) |> 
  mutate(ano = as.integer(ano))

# frota
frota <- 
  fleetbr |> 
  pivot_wider(names_from = modal, values_from = frota) |>
  select(-c(uf, mes)) |> 
  summarise(.by = ano, across(everything(), sum)) |> 
  rename_with(~ str_to_lower(.)) |> 
  rename(total_veiculos = total) |> 
  mutate(ano = as.integer(ano))

# sinistros prf
url <- "https://github.com/ONSV/prfdata/releases/download/v0.2.0/prf_sinistros.zip"

temp_file <- tempfile()
temp_dir <- tempdir()

download.file(url, temp_file, quiet = T)
unzip(temp_file, exdir = temp_dir)

sinistros_prf <- open_dataset(file.path(temp_dir, "prf_sinistros")) |> 
  mutate(
    acidentes_fatais = if_else(
      classificacao_acidente == "Com Vítimas Fatais",
      1, 0, missing = 0
    )
  ) |> 
  summarise(
    .by = ano,
    qnt_acidentes = n(),
    qnt_acidentes_fatais = sum(acidentes_fatais),
    qnt_feridos = sum(feridos),
    qnt_mortos = sum(mortos)
  ) |> 
  arrange(ano) |>  
  mutate(ano = as.integer(ano)) |> 
  collect()

# obitos datasus
mortes <-
  rtdeaths |> 
  count(ano_ocorrencia, name = "mortes") |> 
  drop_na() |> 
  rename(ano = ano_ocorrencia) |> 
  mutate(ano = as.integer(ano))

data_list <- list(frota, mortes, condutores, sinistros_prf)

data <- 
  data_list |> 
  reduce(full_join, by = "ano") |> 
  arrange(ano)

save(data, file = "data/data.rda")
