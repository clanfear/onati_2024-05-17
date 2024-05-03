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


# shr_props_year <- shr_raw |>
#   filter(year %in% c(1995,2021) & offender_1_weapon == "handgun" & !is.na(offender_1_age)) |>
#   group_by(year) |>
#   count(offender_1_age) |>
#   mutate(age = as.numeric(str_extract(offender_1_age, "\\d+"))) |>
#   filter(!is.na(age)) |>
#   arrange(age) |>
#   mutate(pr = n / sum(n),
#          cum_pr = cumsum(n) / sum(n)) |>
#   ungroup()

library(gganimate)

anim_years <- 1990:2021

shr_props_year <- shr_raw |>
  filter(year %in% anim_years & offender_1_weapon == "handgun" & !is.na(offender_1_age)) |>
  group_by(year) |>
  count(offender_1_age) |>
  mutate(age = as.numeric(str_extract(offender_1_age, "\\d+"))) |>
  filter(!is.na(age)) |>
  arrange(age) |>
  mutate(pr = n / sum(n),
         cum_pr = cumsum(n) / sum(n)) |>
  ungroup() |>
  filter(between(age, 10, 60)) |>
  mutate(stage = case_when(
    age <= 20 ~ "Adolescence",
    age > 20 ~ "Adulthood",
    TRUE ~ ""
  ))

library(showtext)
showtext_auto()
plot_font <- "Open Sans"
font_add_google(name = plot_font)
shr_age_distribution <- ggplot(shr_props_year, aes(age, pr, fill = stage)) +
  geom_col() +
  # Here comes the gganimate specific bits
  labs(title = 'Offender age distribution: {frame_time}', x = 'Age', y = NULL) +
  transition_time(year) +
  scale_x_continuous(breaks = seq(10, 60, by = 10)) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal(base_size = 30) +
  coord_cartesian(expand = FALSE) +
  theme(panel.grid       = element_blank(),
        axis.ticks.x     = element_line(color = "white"),
        axis.text.x      = element_text(color = "white", margin = margin(10,0,0,0)),
        axis.text.y      = element_text(color = "white", margin = margin(0,10,0,0)),
        axis.title.y     = element_text(color = "white", margin = margin(0,10,0,0)),
        text             = element_text(color = "white"),
        panel.background = element_rect(fill = "#111111",colour = "#111111"),
        plot.background  = element_rect(fill = "#111111",colour = "#111111"),
        plot.margin = margin(20,12,5,5),
        legend.position  = "none")

animate(shr_age_distribution, 
        renderer = gifski_renderer(file = "./img/animations/shr_age_distribution.gif", loop = TRUE),
        width = 800, height = 800,
        fps = 2, 
        nframes = 12 + length(anim_years),
        device = "ragg_png", 
        end_pause = 6, 
        start_pause = 6,
        bg = 'transparent')

shr_rates_anim <- shr_rates_year |>
  filter(year %in% anim_years) |>
  select(year, rate, type) |>
  mutate(dot_year = min(anim_years)) |>
  complete(nesting(year, rate, type), dot_year = anim_years) |>
  mutate(draw = dot_year == year) |>
  ggplot(aes(x = year, y = rate, group = type, color = type, linetype = type)) + 
  geom_line(linewidth = 1.5) +
  geom_text(data = tibble(year = c(2005, 2005), rate = c(2.25,5.75), type = c("Firearm", "All")), 
            aes(label = type), size = 10, family = plot_font) +
  geom_point(data = ~ . |> filter(draw), aes(x = dot_year, color = type), size = 6) +
  transition_time(dot_year) +
  scale_linetype_manual(values = c("All" = "dashed", "Firearm" = "solid")) +
  geom_hline(yintercept = 0, color = "white", linewidth = 0.25) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 10)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(title = 'US homicide rate', x = "Year", y = "Per 100,000") +
  theme_minimal(base_size = 30) +
  theme(panel.grid       = element_blank(),
        axis.ticks.x     = element_line(color = "white"),
        axis.text.x      = element_text(color = "white", margin = margin(10,0,0,0)),
        axis.text.y      = element_text(color = "white", margin = margin(0,10,0,0)),
        axis.title.y     = element_text(color = "white", margin = margin(0,15,0,0)),
        text             = element_text(color = "white"),
        panel.background = element_rect(fill = "#111111",colour = "#111111"),
        plot.background  = element_rect(fill = "#111111",colour = "#111111"),
        plot.margin = margin(20,12,5,5),
        legend.position  = "none")

animate(shr_rates_anim, 
        renderer = gifski_renderer(file = "./img/animations/shr_rates_anim.gif", loop = TRUE), 
        fps = 2, 
        width = 800, height = 800,
        nframes = 12 + length(anim_years),
        device = "ragg_png", 
        end_pause = 6, 
        start_pause = 6,
        bg = 'transparent')
