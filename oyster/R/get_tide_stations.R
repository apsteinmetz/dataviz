# get closest tide station
library(tidyverse)
library(nplyr)
library(rnoaa)
library(rvest)
library(V8)

# uses javascript on the fly so save this page as html and read that
# https://tidesandcurrents.noaa.gov/tide_predictions.html?gid=1407#listing

# maybe try rselenium or v8

tide_station_url <- "https://tidesandcurrents.noaa.gov/tide_predictions.html?gid=1407#listing"
# tide_stations_xml <- rvest::read_html(tide_station_url)
#
# js <- tide_stations_xml |>
#   html_elements("script") |>
#   html_text() |>
#   pluck(9)
#
# sb <- V8::new_context()
#
# sb$eval(js)

tide_station_path <- "data/Tide_Stations_NY_Harbor.html"
#scrape table of tide stations


tide_stations_raw <- rvest::read_html(tide_station_path)

# extract table from tide station page
tide_stations <- tide_stations_raw %>%
  rvest::html_elements("table") %>%
  rvest::html_table() |>
  pluck(1) |>
  # make a separate column for names where Id is an empty string
  mutate(area = if_else(Id == "", Name, NA),.before = Name) |>
  # if area is empty, fill with previous area
  fill(area) |>
  # remove rows where Id is empty
  filter(Id != "")


write_csv(tide_stations, "data/tide_stations_nyc.csv")


# function to get distance between two points of lat/lon -----------------------
# https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula

# pi constant
pi <- 3.14159265358979323846

deg2rad <- function(deg) {
  return deg * (pi/180)
}

get_distance <- function(lat1,lon1,lat2,lon2) {
  rad <- 6371 #Radius of the earth in km
  dLat <- deg2rad(lat2-lat1) # deg2rad below
  dLon <-  deg2rad(lon2-lon1);
  a =
    sin(dLat/2) * sin(dLat/2) +
    cos(deg2rad(lat1)) * cos(deg2rad(lat2)) *
    sin(dLon/2) * sin(dLon/2)

  c <- 2 * atan2(a**0.5, (1-a)**0.5)
  d <- rad * c # Distance in km
  return (d)
}

# example: get distance in km between two points
get_distance(40.7,-74,40.8,-74)

load("data/wq_meta.rdata")

get_closest_tide_station <- function(lat_q,lon_q) {
  # get distance between each station and the lat/lon
  closest <- tide_stations %>%
    mutate(dist = get_distance(Lat,Lon,lat_q,lon_q)) |>
    # get the closest station
    filter(dist == min(dist)) |>
    # prepend "closest_" to column names
    rename_with(~paste0("closest_tide_",.x))
  return(as_tibble(as.list(closest)))
}

wq_meta_2 <- wq_meta |>
  mutate(closest_tide_station = map2_dfr(latitude,longitude,get_closest_tide_station)) |>
  unnest(cols = c(closest_tide_station))


