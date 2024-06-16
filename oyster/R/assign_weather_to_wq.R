# assign weather to WQ data
library(tidyverse)
library(lubridate)
#library(rnoaa)
library(rvest)
library(duckdb)
library(duckplyr)
library(arrow)

# read in the data
wq_data_3 <- df_from_parquet("data/wq_data_3.parquet")
# read weather data
weather <- df_from_parquet("data/weather.parquet")

# merge the data
wq_data_4 <- wq_data_3 %>% 
  left_join(weather, by = c("date")) |> 
  rename(weather_station = station,temperature = TEMP)

# features used for modeling
wq_model_data <- wq_data_4 |> 
  select(bacteria,precip_wk,tide_level,hours_since_last,flood_or_ebb,temperature)


# write to parquet
arrow::write_parquet(wq_data_4,"data/wq_data_4.parquet")
# write to csv
write_csv(wq_data_4,"data/wq_data_4.csv")
