# get tide data from noaa ----------------------------------
library(tidyverse)
library(lubridate)
#library(rnoaa)
library(rvest)
library(duckdb)
library(duckplyr)
library(arrow)

# use duckdb for every tidyverse function
methods_overwrite()

#  default station is the battery NYC
battery <- "8518750"
# functions to assign tide data water quality spreadsheet ----------------------

# get tide position for a specific time
get_tide_position <- function(obs_time = as.POSIXct("2011-10-20 18:43:00"), station = "8530645") {
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
  try_tide <- function(station){
    tide_pos <- tides_noaa |>
      filter(station_id == station) |>
      mutate(hours_since_last = difftime(obs_time, datetime, units = "hours")) |>
      filter(hours_since_last > 0) |>
      slice_min(order_by = hours_since_last, n = 1) |>
      #  take estimated_tide == FALSE when a proxy is also returned
      arrange(hours_since_last) |>
      tail(1) |>
      rename(tide_time = datetime) |>
      mutate(flood_or_ebb = if_else(hi_lo == "H", -1, 1)) |>
      select(-date, -hi_lo)
  }
  tide_pos <- try_tide(station)
  if (tide_pos$hours_since_last[1] > 8){
    tide_pos <- try_tide(battery)
    tide_pos$good_tide_station <- FALSE
  }
  return(tide_pos)
}

get_tide_time <- function(obs_time, station = "8530645") {
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
  tide_time <- tides_noaa |>
    filter(station_id == station) |>
    mutate(hours_since_last = difftime(obs_time, datetime, units = "hours")) |>
    filter(hours_since_last > 0) |>
    slice_min(order_by = hours_since_last, n = 1) |>
    #  take estimated_tide == FALSE when a proxy is also returned
    arrange(estimated_tide) |>
    head(1) |>
    pull(datetime)
  return(tide_time)
}

get_tide_time_2 <- Vectorize(function(obs_time, station = "8530645") {
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
  tide_time <- tides_noaa |>
    filter(station_id == station) |>
    mutate(hours_since_last = difftime(obs_time, datetime, units = "hours")) |>
    filter(hours_since_last > 0) |>
    slice_min(order_by = hours_since_last, n = 1) |>
    #  take estimated_tide == FALSE when a proxy is also returned
    arrange(estimated_tide) |>
    head(1) |>
    pull(datetime)
  return(tide_time)
})


# assign best available tide data to each sample -------------------------------
tides_noaa <- duckplyr_df_from_parquet("data/tides_noaa_sm.parquet")

# test
get_tide_position(ymd_hms("2011-10-20 12:00:00",tz= "America/New_York"))
get_tide_time    (ymd_hms("2011-10-20 12:00:00",tz= "America/New_York"),"8530645")

# get tide position for all sample times in wq_data
# mutate to return a list column
wq_data_2 <- df_from_parquet("data/wq_data_2.parquet")

#test
methods_restore()
tides_noaa <- as_tibble(tides_noaa)
tictoc::tic()
wq_data_3 <- wq_data_2 |>
  as_tibble() |>
  # there should be no NA in closest_tide_Id with clean data
  filter(!is.na(closest_tide_Id)) |>
  rowwise() %>%
  mutate(tide_time = map2_dfr(sample_time,closest_tide_Id,get_tide_position)) |>
  unnest(tide_time) |>
  rename(tide_station_id = station_id) |>
  mutate(hours_since_last = as.numeric(hours_since_last))
tictoc::toc()

# write to parquet
arrow::write_parquet(wq_data_3,"data/wq_data_3.parquet")
# write to csv
write_csv(wq_data_3,"data/wq_data_3.csv")

