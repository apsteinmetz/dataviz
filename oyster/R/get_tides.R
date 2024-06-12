# get tide data from noaa ----------------------------------
library(tidyverse)
library(lubridate)
library(rnoaa)
library(rvest)
library(duckdb)
library(duckplyr)
library(arrow)

# use duckdb for every tidyverse function
methods_overwrite()

#  default station is the battery NYC
battery <- "8518750"

# functions to get tide data from NOAA -----------------------------------------
get_json_response <- function(url) {
  response <- httr::GET(url) |>
    httr::content("text") |>
    jsonlite::fromJSON(simplifyDataFrame = TRUE)
  # one level of error checking
  # default to battery if no data
  estimated_tide <- !is.null(response$error)
  if (estimated_tide) {
    print("No data. Defaulting to The Battery")
    station = battery
    url <- str_replace(url, "station=[0-9]+", paste0("station=", station))
    response <- httr::GET(url) |>
      httr::content("text") |>
      jsonlite::fromJSON(simplifyDataFrame = TRUE)
  }
  response$good_tide_station <- !estimated_tide
  return(response)
}

# get tide data for a specific period
get_tide_data_noaa <- function(station = "8518750", # the battery
                               begin_date = "2021-01-01",
                               end_date = "2021-01-01") {
  # for error handling
  max_attempts <- 3
  attempt <- 1

  begin_date <- gsub("-","",as.character(begin_date))
  end_date <- gsub("-","",as.character(end_date))
  # debug
  print(paste(begin_date,end_date,station))

  url <- paste0("https://tidesandcurrents.noaa.gov/api/prod/datagetter?",
                "begin_date=",begin_date,
                "&end_date=",end_date,
                "&station=",station,
                "&product=predictions",
                "&datum=MLLW",
                "&time_zone=lst_ldt&units=english&interval=hilo&format=json")

  while(attempt <= max_attempts) {
    tryCatch({
      response <- get_json_response(url)
      break
    }, error = function(e) {
      if (attempt == max_attempts) {
        stop("JSON parse failure", max_attempts, " attempts.")
      }
      print(paste("JSON parse Attempt", attempt, "failed. Retrying..."))
      attempt <- attempt + 1
    })
  }

    tides_noaa <- response$predictions |>
    set_names(c("datetime","tide_level","hi_lo")) |>
    mutate(date = as.Date(datetime),.before = datetime) |>
    mutate(datetime = ymd_hms(paste0(datetime,":00"),tz= "America/New_York")) |>
    mutate(tide_level = as.numeric(tide_level)) |>
    mutate(station_id = station,.before = tide_level)
    tides_noaa$good_tide_station <- response$good_tide_station

  attr(tides_noaa,"station_id") <- station
  return(tides_noaa)

}

get_tide_data_noaa_year <- function(year=2011, station = "8518750"){
  print(paste(year, station))
  begin_date <- paste0(year,"-01-01")
  end_date <- paste0(year,"-12-31")
  return(get_tide_data_noaa(station = station,
                            begin_date = begin_date,
                            end_date = end_date))
}

# retrieve tide data for each combination of  observation date and station -----
# get tide data for each date and station in wq_data_2
# map() returns nothing if there is an error at any point.  Instead, this builds
# data frame one iteration at a time, allowing a manual restart from
# the last successful iteration. Change start_index to current value of n

# get tide stations needed
wq_data_2 <- duckplyr_df_from_file("data/wq_data_2.parquet","read_parquet")
wq_data <- duckplyr_df_from_file("data/wq_data.parquet","read_parquet")

needed_data <- wq_data_2 |>
  select(date,closest_tide_Id,date) |>
  filter(!is.na(closest_tide_Id)) |>
  distinct() |>
  arrange(date)

#test
tides_noaa <- get_tide_data_noaa(wq_data_2$closest_tide_Id[1],begin_date = wq_data_2$date[1]-1,end_date = wq_data_2$date[1])

start_index = 1
tides_noaa <- tibble()
for (n  in start_index:nrow(needed_data)) {
    # for() converts date to numeric
    print(paste(needed_data$closest_tide_Id[n],needed_data$date[n]))
    tides_noaa <- bind_rows(
      tides_noaa,
      get_tide_data_noaa(
        station = needed_data$closest_tide_Id[n],
        end_date = needed_data$date[n],
        begin_date = needed_data$date[n] - 1
      )
    )
}

tides_noaa <- tides_noaa |> distinct()


# save as parquet file
# df_to_parquet(tides_noaa,"data/tides_noaa.parquet")
arrow::write_parquet(tides_noaa,"data/tides_noaa.parquet")



# functions to assign tide data water quality spreadsheet ----------------------

# get tide position for a specific time
get_tide_position <- function(obs_time = as.POSIXct("2011-10-20 18:43:00"), station = "8530645") {
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
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
    select(-date,-hi_lo)
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
tides_noaa <- duckplyr_df_from_parquet("data/tides_noaa.parquet")

# test
get_tide_position(ymd_hms("2011-10-20 12:00:00",tz= "America/New_York"))
get_tide_time    (ymd_hms("2011-10-20 12:00:00",tz= "America/New_York"),"8530645")

# get tide position for all sample times in wq_data
# mutate to return a list column
wq_data_2 <- duckplyr_df_from_parquet("data/wq_data_2.parquet")

#test


methods_restore()
tides_noaa <- as_tibble(tides_noaa)
tictoc::tic()
wq_data_3 <- wq_data_2[1:20,] |>
  as_tibble() |>
  mutate(tide_time = map2(sample_time,closest_tide_Id,get_tide_position)) |>
  unnest(tide_time)

  #unnest(tide_time) |>
  #distinct()
# vectorize loses the class attribute. put it back
attributes(wq_data_3$tide_time) <- list(class=c("POSIXct","POSIXt"))
tictoc::toc()

tides_noaa_2 <- tides_noaa |>
  rename(closest_tide_Id = station_id,
         tide_time = datetime) |>
  mutate(tide_direction = ifelse(hi_lo == "H",-1,1)) |>
  select(-date,-hi_lo,-estimated_tide) |>
  distinct()

wq_data_3 <- wq_data_3 |>
  left_join(tides_noaa_2,by=c("closest_tide_Id","tide_time")) |>
  mutate(since_tide = difftime(sample_time,tide_time,units="hours"))


