library(tidyverse)

shr_raw <- readRDS("./data/raw/shr_1976_2022.rds")

shr_props_year <- shr_raw |>
  filter(year %in% c(1995,2021) & offender_1_weapon == "handgun" & !is.na(offender_1_age)) |>
  group_by(year) |>
  count(offender_1_age) |>
  mutate(age = as.numeric(str_extract(offender_1_age, "\\d+"))) |>
  filter(!is.na(age)) |>
  arrange(age) |>
  mutate(pr = n / sum(n),
         cum_pr = cumsum(n) / sum(n)) |>
  ungroup()

save(shr_props_year, file = "./data/shr_props_year.RData")
