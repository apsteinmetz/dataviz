# convert country codes in kaggle files to iso3 codes
library(tidyverse)
library(rvest)
library(gt)

if (!file.exists(here::here("medals/data/country_codes.csv"))){
# make master list of country codes, iso2, iso3, and ioc/fifa codes
wiki_iso_ioc_fifa_url <- "https://en.wikipedia.org/wiki/Comparison_of_alphabetic_country_codes"
wiki_iso_url <- "https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes"

wiki_iso <- read_html(wiki_iso_url) |> 
  html_elements("table") |> 
  pluck(1) |> 
  html_table() |> 
  janitor::clean_names() |>
  select(1,4,5) |> 
  set_names("country_name","iso2","iso3") |> 
  # remove first row
  slice(-1)

wiki_iso[1,2] <- "AF" #strange  

wiki_iso_ioc_fifa <- read_html(wiki_iso_ioc_fifa_url) |> 
  html_elements("table") |> 
  pluck(1) |> 
  html_table() |> 
  janitor::clean_names() |> 
  select(-x,-flag) |> 
  rename(iso3 = iso)

country_codes <- left_join(wiki_iso,wiki_iso_ioc_fifa) |> 
  select(-country) |> 
  filter(str_length(iso2) ==2) |> 
  mutate(country_name = str_remove(country_name,"\\[.+\\]"))

# save country codes
write_csv(country_codes,here::here("medals/data/country_codes.csv"))
} else {
  # read country codes
  country_codes <- read_csv(here::here("medals/data/country_codes.csv"))
}

medal_counts <- read_csv(here::here("medals/data/medals_total.csv"))
  
medal_counts_iso <- medal_counts |> 
  rename(ioc = country_code) |>
  left_join(country_codes,by="ioc") |> 
  rename(country_code = iso3) |>
  select(-iso2,-ioc,-fifa)

write_csv(medal_counts_iso,here::here("medals/data/medal_counts_iso.csv"))

# data in gt package
pop  <- countrypops |> 
  filter(year == 2022) |> 
  rename(country_code = country_code_3) |>
  select(country_code,country_name,population)
pop

gdp <- read_csv(here::here("medals/data/gdp.csv")) |> 
  mutate(GDP = as.numeric(str_remove_all(GDP,","))) |> 
  select(country_code,GDP)
gdp                

flag_urls <- country_codes |> 
  mutate(flag_url = str_c("https://flagcdn.com/w40/",tolower(iso2),".png")) |> 
  select(iso3,flag_url) |> 
  rename(country_code = iso3)

flag_urls

# use only countries with medals
macro_data <- medal_counts_iso |> 
  select(country_code) |>
  distinct() |>
  # alphabetic factor
  left_join(gdp, by = "country_code") |> 
  left_join(pop, by = "country_code") |> 
  left_join(flag_urls, by = "country_code") |> 
  select(country_code, country_name, GDP, population,flag_url) |> 
  mutate(country_code = as.factor(country_code)) |> 
  mutate(country_name = if_else(country_code == "TWN","Taiwan",country_name)) |> 
  mutate(population = if_else(country_code == "TWN",23570000,population)) |> 
  mutate(GDP = if_else(country_code == "TWN",791.61e9,GDP)) |> 
  # in billions
  mutate(GDP = GDP/1e9) |>
  # in millions
  mutate(population = population/1e6) |>
  filter(!is.na(country_name)) |> 
  arrange(country_name)

# save macro data
write_csv(macro_data,here::here("medals/data/macro_data.csv"))
