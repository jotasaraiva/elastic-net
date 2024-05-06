library(dplyr)
library(tidyr)
library(tidymodels)
tidymodels_prefer()

load("data/data.rda")

data <- 
  data |> 
  drop_na() |> 
  mutate(across(where(is.numeric), as.numeric))

cv <-
  vfold_cv(data, v = 5)

rec <-
  recipe(mortes ~ ., data = data) |>
  remove_role(ano, old_role = "predictor") |>
  step_scale(all_numeric_predictors())

elastic_net <-
  linear_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet")

grid <-
  expand_grid(penalty = seq(0, 100, 10),
              mixture = seq(0, 1, 0.2))

result <-
  tune_grid(
    elastic_net,
    preprocessor = rec,
    grid = grid,
    resamples = cv
  )

best_fit <-
  result |> 
  collect_metrics() |> 
  filter(.metric == "rmse") |> 
  arrange(mean)

penalty <- slice(best_fit, 1)$penalty
mixture <- slice(best_fit, 1)$mixture

best_spec <- 
  linear_reg(penalty = penalty, mixture = mixture) |> 
  set_engine("glmnet")

fit <-
  workflow() |> 
  add_model(best_spec) |> 
  add_recipe(rec) |> 
  fit(data)


bind_cols(
  data,
  predict(fit, data)
) |> 
  select(ano, mortes, .pred) |> 
  pivot_longer(-ano) |> 
  ggplot(aes(ano, value, color = name)) +
  geom_line()
