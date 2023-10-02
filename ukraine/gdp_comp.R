# plot gdps of ukraine belligerants
# sources: UN, Kiel Institute
library(tidyverse)
library(RColorBrewer)
library(ggimage)

gdp_raw <- read_csv("data/world_gdp_un.csv")
load("data/short_country_names.rdata")
ru_names <- read.csv("data/russian_country_names.txt") %>% 
  as_tibble()

gdp <- gdp_raw %>% 
  as_tibble %>% 
  filter(Year == 2020) |> 
  filter(Item == "Gross Domestic Product (GDP)") %>% 
  left_join(short_country_names) %>% 
  mutate(GDP_TRN_USD = Value/1000000000000) |> 
  select(Country,arming,GDP_TRN_USD)  %>% 
  mutate(arming = factor(arming,
                         levels = c("Ukraine",
                                    "Arming Ukraine",
                                    "Russia",
                                    "Arming Russia"))) |> 
  filter(!is.na(arming)) %>% 
  bind_cols(ru_names) %>% 
  mutate(Помощь = arming)
  
  
levels(gdp$Помощь) <- c("Украина", "Оружие на Украину", "Россия", "Оружие на России")

gdp %>% 
  group_by(arming) %>% 
  tally(GDP_TRN_USD,name = "GDP_TRN_USD") %>% 
  ggplot(aes(arming,GDP_TRN_USD)) + geom_col() + 
  labs(title = "Who Wins A War of Attrition?",
       subtitle = "GDP of Belligerents and their Helpers in the Russo-Ukraine War",
       y = "GDP in 2020 (Trillion USD)",
       x = "Countries",
       caption = "Sources: UN, Kiel Institute")

# stacked bar
gg <- gdp %>% 
  ggplot(aes(arming,GDP_TRN_USD,fill=Country)) + 
           geom_col(color = "black") +
  scale_fill_manual(values = rep(brewer.pal(6,"Dark2"),8))  +
  annotate("text",x = 4,y=5,label = "Iran,\nN.Korea,\nBelarus",size = 3) +
  labs(title = "Who Wins A War of Attrition?",
       subtitle = "GDP of Belligerents and their Helpers in the Russo-Ukraine War",
       y = "GDP in 2020 (Trillion USD)",
       x = "Countries",
       caption = "Sources: UN, Kiel Institute") + 
  theme_minimal() + 
  theme(panel.grid = element_blank())

gg %>% ggbackground(background = "data/plot_background.jpg",alpha = .1)


gg <- gdp %>% 
  ggplot(aes(Помощь,GDP_TRN_USD,fill=страна)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = rep(brewer.pal(6,"Dark2"),8))  +
  annotate("text",x = 4,y=5,label = "Иран,\nЮжная Корея,\nБеларусь",size = 3) +
  labs(title = "Кто выиграет войну на истощение?",
       subtitle = "ВВП воюющих сторон и их пособников в русско-украинской войне",
       y = "ВВП в 2020 г. (триллион долларов США)",
       x = "Страны",
       caption = "Источники: ООН, Кильский институт.") + 
  theme_minimal() + 
  theme(panel.grid = element_blank())

  gg %>% 
    ggbackground(background = "data/plot_background.jpg",alpha = .1)

source("bar_plot_unicode.r")
temp <- gdp |> 
  group_by(arming) |> 
  tally(GDP_TRN_USD,name="GDP_TRN_USD") |> 
  select(arming,GDP_TRN_USD) |>  
  mutate(bar = build_bar(GDP_TRN_USD))
