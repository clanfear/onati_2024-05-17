library(tidyverse)


load("./data/raw/hom_rate_df.RData")



shr_raw <- readRDS("./data/raw/shr_1976_2022.rds")

fred_pop_year <- read_csv("./data/raw/fred_pop_year.csv") |>
  transmute(pop = (pop*1000)/100000,
            year = as.numeric(str_extract(date, "\\d+$")))
firearms <- c("rifle", "shotgun", "other gun", "handgun", "firearm, type not stated")

shr_rates_year <- shr_raw |>
  filter(between(year, 1970, 2021) & homicide_type != "manslaughter by negligence") |>
  mutate(gun = offender_1_weapon %in% firearms) |>
  summarize(All = n(),
            Firearm = sum(gun),
            .by = year) |>
  left_join(fred_pop_year) |>
  pivot_longer(c(All, Firearm), names_to = "type", values_to = "n") |>
  mutate(rate = n/pop)

save(shr_rates_year, file = "./data/shr_rates_year.RData")

chicago_rates_year <- hom_rate_df |>
  select(year, All = hom_rate, Firearm = gun_hom_rate) |>
  pivot_longer(c(All, Firearm), names_to = "type", values_to = "rate")

save(chicago_rates_year, file = "./data/chicago_rates_year.RData")


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
