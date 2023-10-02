# Did "stop and frisk" reduce gun crime?
# https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8

library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(pdftools)
library(leaflet)
library(mapview)
library(leafpop)
library(ggthemr)
ggthemr::ggthemr("solarized")
rev_swatch <- ggthemr:::load_palette.character("solarized")$swatch %>% rev()
#ggthemr::ggthemr_palette(rev_swatch)

shooting_raw <- read_csv("data/NYPD_Shooting_Incident_Data__Historic_.csv")
View(shooting_raw)

# -----------------------------------
# clean up data
names(shooting_raw) <- tolower(names(shooting_raw))
shooting <- shooting_raw %>%
   rename(time = occur_time,
          date = occur_date,
          is_murder = statistical_murder_flag) %>%
   mutate(date = as.Date(date,format = "%m/%d/%Y")) %>%
   mutate_if(is_character,str_to_title) %>%
   mutate_if(is_character,as_factor) %>%
   mutate(precinct=as.factor(precinct)) %>%
   {.}

# -----------------------------------
# make factor levels the same across perp and victim
race_levels <-
   c(
      "Black",
      "Black Hispanic",
      "White Hispanic",
      "White" ,
      "Asian / Pacific Islander",
      "Unknown"
   )
age_levels <- c("<18","18-24","25-44","45-64" ,"65+","Unknown")
shooting <- shooting %>% mutate_at(vars(contains("race")),factor,levels=race_levels) %>%
   mutate_at(vars(contains("age")),factor,levels=age_levels)
# -----------------------------------
# wish I could use tidy select features here
shooting <- shooting %>%
   mutate(perp_age_group=fct_explicit_na(perp_age_group,na_level="Unknown")) %>%
   mutate(perp_race=fct_explicit_na(perp_race,na_level="Unknown")) %>%
   mutate(vic_race=fct_explicit_na(vic_race,na_level="Unknown")) %>%
   mutate(location_desc=fct_explicit_na(location_desc,na_level="Unspecified")) %>%
   mutate(perp_sex=fct_explicit_na(perp_sex,na_level="Unknown"))

# -----------------------------------
# Shooting Trends
shooting %>%
   mutate(year=year(date)) %>%
   group_by(year,is_murder) %>% tally %>%
   ggplot(aes(year,n,fill=is_murder)) + geom_col()

# -------------------------------------------------------
# Wrong place, wrong time
shooting %>% mutate(hour = hour(time)) %>%
   group_by(hour) %>%
   ggplot(aes(hour)) + geom_bar()  +
   labs(title = "The Midnight Hour",
        subtitle = "NYC Shootings 2003 to 2019-09",
        caption = "source:NYPD Compustat",
        x = "Hour of the Day",
        y = "Count")

# rank most dangerous spots and turn to factor level
loc_levels <- shooting %>%
   group_by(location_desc) %>%
   count() %>% arrange(desc(n)) %>%
   pull(location_desc) %>%
   as.character()

shooting <- shooting %>% mutate(location_desc=factor(location_desc,levels = loc_levels))


shooting %>% mutate(hour = hour(time)) %>%
   group_by(hour,location_desc) %>%
   tally() %>%
   ggplot(aes(hour,location_desc,fill=n)) + geom_tile() +
   scale_fill_gradient(trans = "log",breaks = c(1,10,100,500),name="Count",) +
   labs(title = "Wrong Place, Wrong Time",
        subtitle = "NYC Shootings 2003 to 2019-09",
        caption = "source:NYPD Compustat")


# -------------------------------------------------------
# -------------------------------------------------------

precincts <- st_read(here::here("/data/police precincts.geojson"))
precinct_summary <- left_join(precincts,shooting)

# -------------------------------------------------------

shot_tally <- shooting %>% group_by(precinct) %>%
   tally() %>%
   arrange(desc(n)) %>%
   {.}

shot_tally %>% ggplot(aes(precinct,n)) + geom_col()
