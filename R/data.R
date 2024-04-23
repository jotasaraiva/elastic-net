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
  rename(categoria_x = ciclo)

frota <- fleetbr |> 
  filter(mes == 7) |> 
  pivot_wider(names_from = modal, values_from = frota) |> 
  summarise(.by = ano, across(3:24, ~ sum(., na.rm = T))) 

rtdeaths |> 
  view()
