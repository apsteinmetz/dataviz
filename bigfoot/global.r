library(tidyverse)
library(broom)
library(lubridate)
library(ggplot2)
library(choroplethr)
library(maps)



# you must log in to Kaggle download this data set
# https://www.kaggle.com/chemcnabb/bfro-bigfoot-sighting-report/downloads/bfro-report-locations.csv
bfro_data<-read_csv("bfro-report-locations.csv")
bfro_data<-bfro_data %>% 
  mutate(region=as.character(NA)) %>% 
  rename(lat=latitude) %>% 
  rename(long=longitude) %>% 
  mutate(timestamp=as.Date(timestamp)) %>% 
  select(matches("[^(title)]"),title) %>% 
  filter(timestamp<Sys.Date()) %>%
  filter(abs(lat<90),abs(long)<180)

load("states_map.rdata")


bfro_data<-bfro_data %>% 
  mutate(region=map.where("state",bfro_data$long,bfro_data$lat)) %>% 
  mutate(region=str_replace(region,":[a-z]+","")) %>% 
  mutate(region=str_replace(region,"washington island","washington")) %>% 
  filter(region != "puerto rico") %>% 
  na.omit()

#Finally, to clean up the data set we get the year from the time stamp, make sure no impossible dates exist and summarise the sighting counts by state, then by state and year.
bfro_data<-bfro_data %>% 
  mutate(Year=year(timestamp)) %>% 
  filter(Year<=year(Sys.Date())) %>%
  select(Year,region,everything()) %>% 
  arrange(Year,region,number)
state_sum<-bfro_data %>%  
  mutate(region=str_to_lower(region)) %>% 
  group_by(region) %>% 
  summarise(value=as.integer(n()))
state_year_sum<-bfro_data  %>% 
  mutate(region=str_to_lower(region)) %>% 
  group_by(region,Year) %>% 
  summarise(value=as.integer(n()))

#fill in missing years and missing states with zero 
state_list<-states_map %>% 
  ungroup() %>% 
  select(region) %>% 
  unique() %>% 
  mutate(region=tolower(region))

state_year_sum<-state_year_sum %>%
  full_join(state_list) %>% 
  complete(Year=full_seq(state_year_sum$Year,1),fill=list(value=0)) %>% 
  mutate(value=as.integer(value)) %>% 
  filter(region != "puerto rico") 


state_sum<-state_sum %>%
  full_join(state_list) %>% 
  complete(region,fill=list(value=0)) %>% 
  mutate(value=as.integer(value)) %>% 
  filter(region != "puerto rico") 

maxYear<-max(state_year_sum$Year)
maxSights<-max(state_year_sum$value)
startYear=1960




