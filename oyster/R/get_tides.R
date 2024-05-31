library(tidyverse)
library(RcppRoll)
library(googlesheets4)
library(lubridate)
library(rnoaa)
library(rvest)

# get tide data from usharbors.com----------------------------------
  make_tide_year_mo <- function(year,month){
    return(paste0(year,"-",str_pad(month,2,pad="0")))
  }


  make_datetime <- function(date,time){
    return(as_datetime(paste(date,time),format = "%Y-%m-%d %I:%M %p",tz="America/New_York"))
  }

  make_date <- function(year_mo,day){
    return(as.Date(paste(year_mo,day,sep = "-"),format = "%Y-%m-%d"))
  }

  tide_year_mos <- NULL
  for (i in 2020:2024){
    for(j in 1:12){
      tide_year_mos <- c(tide_year_mos,make_tide_year_mo(i,j))
    }
  }
  tide_year_mos



  get_tide_data <- function(tide_year_mo = "2021-04") {
    # tide_year_mo <- make_tide_year_mo(tide_year, tide_mo)
    cat(tide_year_mo)
    tides_html <- rvest::read_html(
      paste0(
        "https://www.usharbors.com/harbor/new-york/new-york-the-battery-ny/tides/?tide=",
        tide_year_mo,
        "#monthly-tide-chart"
      )
    )

    #extract monthly tide chart
    tides <- tides_html %>%
      rvest::html_nodes(xpath = '//*[@id="monthly-tide-chart"]') %>%
      rvest::html_table() |>
      pluck(1)
    if (length(tides) == 0) {
      cat(" No Data\n")
      return(NULL)
    }

    cat("\n")
    tides <- tides  |>
      janitor::clean_names() |>
      select(1:10) |>
      # remove first row with units
      slice(-1) |>
      set_names(
        c(
          "daynum",
          "day_of_week",
          "high_tide_am_time",
          "high_tide_am_ft",
          "high_tide_pm_time",
          "high_tide_pm_ft",
          "low_tide_am_time",
          "low_tide_am_ft",
          "low_tide_pm_time",
          "low_tide_pm_ft"
        )
      ) |>
      mutate(date = make_date(tide_year_mo, daynum),
             .before = "daynum") |>
      filter(!is.na(date)) |>
      mutate(across(contains("ft"), as.numeric)) |>
      mutate(across(contains("am_time"),  ~ paste(.x, "AM"))) |>
      mutate(across(contains("pm_time"),  ~ paste(.x, "PM"))) |>
      mutate(across(contains("time"),  ~ make_datetime(date, .x))) |>
      select(-daynum, -day_of_week)
    return(tides)
  }



  tides <- map(tide_year_mos,get_tide_data) |> bind_rows()

  save(tides,file="data/tides.rdata")

  # get tide data from noaa ----------------------------------
battery <- "8518750"

get_tide_data_noaa <- function(station = "8518750", # the battery
                               begin_date = "2021-01-01",
                               end_date = "2021-02-20") {

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
    set_names(c("datetime","tide_level","hi_lo"))
  attr(tides_noaa,"station_id") <- station
  attr(tides_noaa,"station_name") <- "The Battery"
  return(tides_noaa)

}

get_tide_data_noaa_year <- function(year, station = "8518750"){
  print(year)
  begin_date <- paste0(year,"-01-01")
  end_date <- paste0(year,"-12-31")
  return(get_tide_data_noaa(station = station, begin_date = begin_date,end_date = end_date))
}

get_tide_data_noaa_year(year= 2011)

tides_noaa <- map(2011:2024,get_tide_data_noaa_year) |> bind_rows()
save(tides_noaa,file="data/tides.rdata")
