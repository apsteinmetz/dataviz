# analyse wq data
library(tidyverse)
library(duckplyr)
library(skimr)

# read in the data
wq <- df_from_parquet("data/wq_model_data.parquet")

glimpse(wq)
skim(wq)
# show pairwise correlation plots
wq |> select(-site) |> cor() |> corrplot::corrplot()

wq_lm <- lm(bacteria ~ ., data = select(wq,-site))
broom::tidy(wq_lm)
summary(wq_lm)



# plot model output
wq |> ggplot(aes(x = precip_wk, y = bacteria)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# normalize bacteria
wq_adj <- wq %>%
  mutate(log_bacteria = log(if_else(bacteria==0, 1,bacteria))) |>
  select(-bacteria)

skim(wq_adj)
# show pairwise correlation plots
wq_adj |> select(-site) |> cor() |> corrplot::corrplot()

wq_adj_2 <- wq_adj |> select(-site) |>
  filter(temperature_noaa > 55)
wq_adj_2_lm <- lm(log_bacteria ~ ., data = wq_adj_2)
broom::tidy(wq_adj_2_lm)
summary(wq_adj_2_lm)

# plot model output
wq_adj_2 |> ggplot(aes(x = precip_wk, y = log_bacteria)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# do a model with only the significant variables
wq_adj_3 <- wq_adj |> select(-site) |>
  filter(temperature_noaa > 55) |>
  select(bacteria,temperature_noaa,precip_noaa)
wq_adj_3_lm <- lm(bacteria ~ ., data = wq_adj_3)
broom::tidy(wq_adj_3_lm)
summary(wq_adj_3_lm)

# model each station separately
wq_adj_4 <- wq_adj |>
  filter(temperature_noaa > 55)
wq_adj_4_lm <- wq_adj_4 |>
  group_by(site) |>
  nest() |>
  mutate(model = map(data, ~lm(log_bacteria ~ ., data = .))) |>
  mutate(tidy = map(model, broom::glance)) |>
  unnest(tidy) |>
  # remove rows with NaN
  filter(!r.squared == 1.0) |>
  filter(nobs > 100)


best_model_data <- wq_adj_4_lm |>
  filter(r.squared  > .24) |>
  select(data) |>
  unnest(data)


# plot model output
best_model_data |> ggplot(aes(x = hours_since_last, y = log_bacteria)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)
