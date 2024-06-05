# get tide data from noaa ----------------------------------
library(tidyverse)
library(lubridate)
library(rnoaa)
library(rvest)
library(duckdb)
library(duckplyr)

# use duckdb for every tidyverse function
methods_overwrite()

#  default station is the battery NYC
battery <- "8518750"

# get tide data for a specific period
get_tide_data_noaa <- function(station = "8518750", # the battery
                               begin_date = "2021-01-01",
                               end_date = "2021-01-01") {

  begin_date <- gsub("-","",begin_date)
  end_date <- gsub("-","",end_date)

  url <- paste0("https://tidesandcurrents.noaa.gov/api/prod/datagetter?",
                "begin_date=",begin_date,
                "&end_date=",end_date,
                "&station=",station,
                "&product=predictions",
                "&datum=MLLW",
                "&time_zone=lst_ldt&units=english&interval=hilo&format=json")

  response <- httr::GET(url) |>
    httr::content("text") |>
    jsonlite::fromJSON(simplifyDataFrame = TRUE)
  tides_noaa <- response$predictions |>
    set_names(c("datetime","tide_level","hi_lo")) |>
    mutate(date = as.Date(datetime),.before = datetime) |>
    mutate(datetime = ymd_hms(paste0(datetime,":00"),tz= "America/New_York")) |>
    mutate(tide_level = as.numeric(tide_level)) |>
    mutate(station_id = station,.before = tide_level) |>
    as_tibble()

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
get_tide_position <- function(obs_time,tide_data){
  # find the closest tide time to the observation time
  tide_time <- tide_data |>
    filter(abs(datetime-obs_time) <6) |>
    slice_min(order_by=datetime,n=1)
  return(tibble(since_tide = obs_time-tide_time$datetime,
                last_tide_level = tide_time$tide_level,
                flood_or_ebb = if_else(tide_time$hi_lo == "H",-1,1)))
}

# get tide stations needed
wq_meta_2 <- duckplyr_df_from_file("data/wq_meta_2.csv","read_csv_auto")
needed_stations <- wq_meta_2 |>
  pull(closest_tide_Id) |>
  unique()

# test
tides_data <- get_tide_data_noaa_year(year= 2021)

year_range = 2011:2024
#needed_stations <- needed_stations[1:2]

# construct a tibble with all combinations of year_range and needed_stations
# and then use map to get the tide data for each combination
# this will take a while
tides_needed <- tibble(year = rep(year_range,each = length(needed_stations)),
                     station = rep(needed_stations,length(year_range)))

# get full history.  This will take a while
tides_noaa <-  map2(tides_needed$year,
                    tides_needed$station,
                    get_tide_data_noaa_year) |>
  bind_rows()


save(tides_noaa,file="data/tides_noaa.rdata")
# save as parquet file
write_parquet(tides_noaa,"data/tides_noaa.parquet")
# save with duckdb


get_tide_position(ymd_hms("2021-01-01 12:00:00",tz= "America/New_York"),tides_data)

# get tide position for all sample times in wq_data
# mutate to return a list column
wq_data <- wq_data |>
  mutate(tide = map(sample_time,~get_tide_position(.x,tides_noaa)))


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
