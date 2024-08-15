# convert country codes in kaggle files to iso3 codes

# read teams csv
teams<-read_csv("medals/data/teams.csv") |> 
  select(country,country_code) |> 
  distinct()
iso_reconcile <- read_csv("medals/data/iso_reconcile.csv") |> 
  rename(country_code = country_code_kaggle)

medal_counts <- read_csv(here::here("medals/data/medals_total.csv"))
  
fixed_codes <- left_join(medal_counts,iso_reconcile,by="country_code") |> 
  select(country_code,country_code_iso) |> 
  distinct()


write_csv(fixed_codes,here::here("medals/data/fixed_codes.csv"))


medal_counts_iso <- left_join(medal_counts,fixed_codes,by="country_code") |> 
  mutate(country_code = if_else(!is.na(country_code_iso),country_code_iso,country_code)) |> 
  select(-country_code_iso)

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

macro_data <- medal_counts_iso |> 
  select(country_code) |>
  distinct() |>
  left_join(gdp, by = "country_code") |> 
  left_join(pop, by = "country_code") |> 
  select(country_code, country_name, GDP, population) |> 
  mutate(country_code = as_factor(country_code)) |> 
  mutate(country_name = if_else(country_code == "TWN","Taiwan",country_name)) |> 
  mutate(population = if_else(country_code == "TWN",23570000,population)) |> 
  mutate(GDP = if_else(country_code == "TWN",791.61e9,GDP)) |> 
  # in billions
  mutate(GDP = GDP/1e9) |>
  # in millions
  mutate(population = population/1e6) |>
  filter(!is.na(country_name))

# save macro data
write_csv(macro_data,here::here("medals/data/macro_data.csv"))
