library(dplyr)
library(tidyr)
library(tidymodels)
tidymodels_prefer()

load("data/data.rda")

data <- drop_na(data, total_veiculos)