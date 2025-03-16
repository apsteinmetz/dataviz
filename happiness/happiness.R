library(tidyverse)
library(gganimate)
library(janitor)
library(countrycode)

happiness <- read_csv("~/R Projects/dataviz_misc/happiness/data/happiness.csv") |>
  clean_names() |>
  # change country name to UTF-8
  mutate(country_name = iconv(country_name, from = "latin2", to = "UTF-8")) |>
  # add a continent column
  mutate(continent = countrycode(country_name,
                                 origin = "country.name",
                                 destination = "continent",
                                 custom_match = c("Kosovo" = "Europe",
                                                  "TĂźrkiye" = "Europe"))) |>
  select(continent, everything())

# summarize the sd of all numeric columns
temp <- happiness |>
  select_if(is.numeric) |>
  summarise_all(sd, na.rm = TRUE)

#plot changes in gdp per capita over time
happiness %>%
  # filter(country_name %in% sample(unique(happiness$country_name),size=10)) |>
  ggplot(aes(x = year, y = healthy_life_expectancy_at_birth, color = country_name)) +
  geom_line() +
  scale_colour_manual(values = gapminder::country_colors) +
  labs(title = "GDP per capita over time",
       x = "Year",
       y = "Log GDP per capita") +
  # remove legend
  theme(legend.position = "none")


happiness |>
  ggplot(aes(log_gdp_per_capita,positive_affect ,
             size = healthy_life_expectancy_at_birth^10,
             colour = country_name)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = gapminder::country_colors) +
  scale_size(range = c(2, 12)) +
  # scale_x_log10() +
  facet_wrap(~continent) +
  theme(legend.position = 'none') +
  labs(title = 'Year: {as.integer(frame_time)}', x = 'GDP Per Capita', y = 'Happiness') +
  transition_time(year)
