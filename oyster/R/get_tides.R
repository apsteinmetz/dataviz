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
    url <- paste0(
      "https://tidesandcurrents.noaa.gov/api/prod/datagetter?",
      "begin_date=", begin_date,
      "&end_date=", end_date,
      "&station=", station,
      "&product=predictions",
      "&datum=MLLW",
      "&time_zone=lst_ldt&units=english&interval=hilo&format=json"
    )
    response <- httr::GET(url) |>
      httr::content("text") |>
      jsonlite::fromJSON(simplifyDataFrame = TRUE)
  }
  response$estimated_tide <- estimated_tide
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
      get_json_response(url)
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
    mutate(station_id = station,.before = tide_level) |>
    mutate(estimated_tide = estimated_tide)

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

# get tide position for a specific time ----------------------------------
get_tide_position <- function(obs_time){
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
  tide_time <- tides_noaa |>
    filter(abs(datetime-obs_time) <6) |>
    slice_min(order_by=datetime,n=1)
  return(tibble(since_tide = obs_time-tide_time$datetime,
                last_tide_level = tide_time$tide_level,
                flood_or_ebb = if_else(tide_time$hi_lo == "H",-1,1)))
}

get_tide_time <- function(obs_time,station){
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
  tide_time <- tides_noaa |>
    filter(station_id == station) |>
    filter(difftime(obs_time,datetime,units="hours") < 6) |>
    slice_min(order_by=datetime,n=1)
  return(tide_time$datetime)
}

# get tide stations needed
wq_data_2 <- duckplyr_df_from_file("data/wq_data_2.parquet","read_parquet")

needed_data <- wq_data_2 |>
  select(date,closest_tide_Id,date) |>
  distinct()

# get tide data for each date and station in wq_data_2
# map() returns nothing if there is an error at any point.  Instead, this builds
# data frame one iteration at a time, allowing a manual restart from
# the last successful iteration. Change start_index to current value of n
start_index = 1
# tides_noaa <- tibble()
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


tides_noaa <- duckplyr_df_from_parquet("data/tides_noaa.parquet")

get_tide_position(ymd_hms("2021-01-01 12:00:00",tz= "America/New_York"))
get_tide_time(ymd_hms("2021-01-01 12:00:00",tz= "America/New_York"),"8518750")

# get tide position for all sample times in wq_data
# mutate to return a list column
wq_data_2 <- duckplyr_df_from_parquet("data/wq_data_2.parquet")

tides <- tides_noaa |>
  rename(closest_tide_Id = station_id)

wq_data_3 <- wq_data_2 |>
  left_join(tides,by = c("date","closest_tide_Id"))

# # get tide data from usharbors.com----------------------------------
#   make_tide_year_mo <- function(year,month){
#     return(paste0(year,"-",str_pad(month,2,pad="0")))
#   }
#
#
#   make_datetime <- function(date,time){
#     return(as_datetime(paste(date,time),format = "%Y-%m-%d %I:%M %p",tz="America/New_York"))
#   }
#
#   make_date <- function(year_mo,day){
#     return(as.Date(paste(year_mo,day,sep = "-"),format = "%Y-%m-%d"))
#   }
#
#   tide_year_mos <- NULL
#   for (i in 2020:2024){
#     for(j in 1:12){
#       tide_year_mos <- c(tide_year_mos,make_tide_year_mo(i,j))
#     }
#   }
#   tide_year_mos
#
#
#
#   get_tide_data <- function(tide_year_mo = "2021-04") {
#     # tide_year_mo <- make_tide_year_mo(tide_year, tide_mo)
#     cat(tide_year_mo)
#     tides_html <- rvest::read_html(
#       paste0(
#         "https://www.usharbors.com/harbor/new-york/new-york-the-battery-ny/tides/?tide=",
#         tide_year_mo,
#         "#monthly-tide-chart"
#       )
#     )
#
#     #extract monthly tide chart
#     tides <- tides_html %>%
#       rvest::html_nodes(xpath = '//*[@id="monthly-tide-chart"]') %>%
#       rvest::html_table() |>
#       pluck(1)
#     if (length(tides) == 0) {
#       cat(" No Data\n")
#       return(NULL)
#     }
#
#     cat("\n")
#     tides <- tides  |>
#       janitor::clean_names() |>
#       select(1:10) |>
#       # remove first row with units
#       slice(-1) |>
#       set_names(
#         c(
#           "daynum",
#           "day_of_week",
#           "high_tide_am_time",
#           "high_tide_am_ft",
#           "high_tide_pm_time",
#           "high_tide_pm_ft",
#           "low_tide_am_time",
#           "low_tide_am_ft",
#           "low_tide_pm_time",
#           "low_tide_pm_ft"
#         )
#       ) |>
#       mutate(date = make_date(tide_year_mo, daynum),
#              .before = "daynum") |>
#       filter(!is.na(date)) |>
#       mutate(across(contains("ft"), as.numeric)) |>
#       mutate(across(contains("am_time"),  ~ paste(.x, "AM"))) |>
#       mutate(across(contains("pm_time"),  ~ paste(.x, "PM"))) |>
#       mutate(across(contains("time"),  ~ make_datetime(date, .x))) |>
#       select(-daynum, -day_of_week)
#     return(tides)
#   }
#
#
#
#   tides <- map(tide_year_mos,get_tide_data) |> bind_rows()
#
#   save(tides,file="data/tides.rdata")
