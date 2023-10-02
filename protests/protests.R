# protests from acledata.com
# for methodology see
# https://acleddata.com/acleddatanew/wp-content/uploads/dlm_uploads/2020/10/ACLED_USAMethodology_2020.pdf


attrib <- "Source: Armed Conflict Location & Event Data Project (ACLED) www.acleddata.com"


library(tidyverse)
library(readxl)
library(wesanderson)
library(lubridate)

# "https://acleddata.com/download/22846/"

protests_raw <- read_excel("data/USA_2020_2021_Jan22.xlsx")

# make long data pivoted on actors
# clean up and organize the data
protests <- protests_raw %>%
   separate(ASSOC_ACTOR_1,into = c("A_ACTOR_1a","A_ACTOR_1b","A_ACTOR_1c","A_ACTOR_1d","A_ACTOR_1e","A_ACTOR_1f"),sep = ";",extra="merge") %>%
   separate(ASSOC_ACTOR_2,into = c("A_ACTOR_2a","A_ACTOR_2b","A_ACTOR_2c","A_ACTOR_2d","A_ACTOR_2e","A_ACTOR_2f"),sep = ";",extra="merge") %>%
   mutate(across(contains("ACTOR"),trimws)) %>%
   pivot_longer(contains("ACTOR"),names_to = NULL,values_to = "ACTOR") %>%
   filter(!is.na(ACTOR)) %>%
   mutate(ACTOR = ifelse(str_detect(ACTOR,"Police Forces"),"Police Forces",ACTOR)) %>%
   mutate(ACTOR = ifelse(str_detect(ACTOR,"Military Forces"),"Military Forces",ACTOR)) %>%
   mutate(ACTOR = ifelse(str_detect(ACTOR,"Government of the United"),"Government Forces",ACTOR)) %>%
   mutate(ACTOR = str_remove(ACTOR,"\\(United States\\)")) %>%
   mutate(ACTOR = ifelse(str_detect(ACTOR,"NAACP"),"NAACP",ACTOR)) %>%
   mutate(across(contains("ACTOR"),trimws)) %>%
   mutate(across(ACTOR = as.factor(ACTOR))) %>%
   mutate(across(contains("EVENT_TYPE"),as.factor)) %>%
   mutate(alignment = "unspecified") %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Forces"),"government",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Patriot"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Liberty"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"GOP"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Republican"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Regiment"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Blue"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"BLM"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Black"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Communist"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"People"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"LGBT"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Workers"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Fasci"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Democrat"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Antifa"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Aryan"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Labor|Labour"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"NFAC"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Latin"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Afri"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"III"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Boys"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Bois"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"QAnon"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Christian"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"White"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Oath"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Free"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"KKK"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Watchmen"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Pro-Police"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Militia"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Social"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"ACLU"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Justice"),"left",alignment)) %>%
   mutate(alignment = factor(alignment,levels = c("left","unspecified","government","right"))) %>%
   unique()



# count(ACTOR,name="event_count",sort=TRUE) %>%

# remove generic (non-political group) actors
# where the labels apply to both left and right protests
generics <- c("Protesters","Students", "Rioters","Teachers","Women",
               "Civilians","Sole Perpetrator","Journalists")
protests <- protests %>%
   filter(!(ACTOR %in% generics))

# EXPLORE DATA -----------------------------------------------------

protests$EVENT_TYPE %>% levels()
protests$SUB_EVENT_TYPE %>% levels()

protests %>% filter(EVENT_TYPE=="Protests") %>%
   select(SUB_EVENT_TYPE) %>%
   unique()
# find locations with most action
top_cities <- protests %>%
   group_by(LOCATION) %>%
   tally(sort=TRUE) %>%
   head(20) %>%
   select(LOCATION) %>%
   rowid_to_column()


#overall fraction of riots vs. protests
pie_data <- protests %>%
   filter(EVENT_TYPE %in% c("Protests","Riots")) %>%
   count(EVENT_TYPE,sort = TRUE) %>%
   mutate(label = paste0(EVENT_TYPE," ",round(n/sum(n)*100),"%")) %>%
   {.}

pie(pie_data$n,
    labels = pie_data$label,
    main="Unrest in America 2020-Jan 2021")


# show worst cities
pal <- c( "#273046","#FAD510", "#CB2314", "#354823")
protests %>%
   filter(EVENT_TYPE != "Strategic developments") %>%
   group_by(LOCATION,EVENT_TYPE) %>%
   count(EVENT_TYPE,sort = TRUE) %>%
   right_join(top_cities) %>%
   ggplot(aes(reorder(LOCATION,-rowid),n,fill=EVENT_TYPE)) + geom_col() +
   coord_flip() +
   labs(y="Event Count",
        x="",
        title="Unrest in America 2020-Jan 2021",
        caption = attrib)  +
   scale_y_continuous(labels = scales::comma) +
   scale_fill_manual(values = pal)

protests %>%
   filter(EVENT_TYPE %in% c("Protests","Riots")) %>%
   ggplot(aes(alignment,fill=EVENT_TYPE)) + geom_bar() +
   labs(y="Event Count",
        title="Unrest in America 2020-Jan 2021",
        caption = attrib,
        x = "Alighment of Participants") +
   scale_y_continuous(labels = scales::comma) +
   scale_fill_manual(values = wesanderson::wes_palette("BottleRocket2",n=2))


# fraction of violence by alignment
protests %>%
   filter(EVENT_TYPE %in% c("Protests","Riots")) %>%
   filter(alignment %in% c("left","right")) %>%
   group_by(alignment,EVENT_TYPE) %>%
   count(EVENT_TYPE) %>%
   group_by(alignment) %>%
   mutate(per = prop.table(n)) %>%
   ggplot(aes(alignment,per,fill=EVENT_TYPE)) + geom_col() +
   labs(y="Percentage",
        title="Unrest in America 2020-Jan 2021",
        caption = attrib)  +
   scale_y_continuous(labels = scales::percent) +
   scale_fill_manual(values = wesanderson::wes_palette("BottleRocket2",n=2))



#most active groups, participated in more than 100 events
active_groups <- protests %>%
   filter(EVENT_TYPE %in% c("Protests","Riots")) %>%
   filter(alignment %in% c("left","right")) %>%
   count(ACTOR,sort=TRUE) %>%
   filter(n > 50) %>%
   select(ACTOR)


# who are the most violent groups
protests %>%
   filter(EVENT_TYPE %in% c("Protests","Riots")) %>%
   filter(alignment %in% c("left","right")) %>%
   # group_by(alignment,ACTOR,EVENT_TYPE) %>%
   # count(EVENT_TYPE) %>%
   right_join(active_groups) %>%
   # Combine all groups labeled "militia" and retally
   mutate(ACTOR = if_else(str_detect(ACTOR,"Militia"),"Various Militia",ACTOR)) %>%
   group_by(alignment,ACTOR,EVENT_TYPE) %>%
   count(EVENT_TYPE) %>%
   group_by(ACTOR) %>%
   mutate(per = prop.table(n)) %>%
   filter(EVENT_TYPE=="Riots") %>%
   arrange(desc(per)) %>%
   ggplot(aes(reorder(ACTOR,per),per,fill=alignment)) + geom_col() +
   labs(y="Percent of Riot Events",
        title="Unrest in America 2020-Jan 2021",
        caption = attrib,
        x = "Actor",
        subtitle = "Most violent actors of those present at more than 50 events.")  +
   scale_y_continuous(labels = scales::percent) +
   scale_fill_manual(values = wesanderson::wes_palette("BottleRocket2",n=2)) +
   coord_flip()

# FATALTIES

deaths_by_city <-
   protests %>%
   filter(EVENT_TYPE %in% c("Protests","Riots")) %>%
   select(EVENT_ID_CNTY,EVENT_DATE,LOCATION,FATALITIES) %>%
   filter(FATALITIES > 0) %>%
   unique() %>%
   arrange(EVENT_DATE) %>%
   group_by(LOCATION) %>%
   tally(FATALITIES) %>%
   arrange(desc(n))


deaths_by_city  %>%
   ggplot(aes(reorder(LOCATION,n),n)) + geom_col() +
   coord_flip() +
   labs(y="Cumulative Fatalities",
        title="Unrest in America 2020-Jan 2021",
        subtitle = "Deaths during Riots or Protests",
        caption = attrib,
        x = "Location")

deaths <- protests %>%
   filter(EVENT_TYPE %in% c("Protests","Riots")) %>%
   select(EVENT_ID_CNTY,EVENT_DATE,LOCATION,FATALITIES,alignment) %>%
   filter(FATALITIES > 0) %>%
   nest(data=c(alignment)) %>%
   unique() %>%
   arrange(EVENT_DATE) %>%
   mutate(event_week = ceiling_date(EVENT_DATE,unit = "week"))

deaths$data

# primary_actor <- tibble("alignment"="right") %>% fix()
#primary_actor$alignment <-  factor(primary_actor$alignment,levels=c("right","both","left"))
#deaths <- bind_cols(deaths,primary_actor) %>%
#   mutate(alignment = as_factor(alignment))



# deaths %>%
# #    group_by(event_week,alignment) %>%
#    group_by(event_week,alignment) %>%
# #   group_by(EVENT_DATE,LOCATION) %>%
#    tally(FATALITIES) %>%
# #    ggplot(aes(date(event_week),n,fill=alignment)) + geom_col() +
#    ggplot(aes(event_week,n,fill=alignment)) + geom_col() +
#     scale_y_continuous(labels=scales::label_comma(accuracy = 1)) +
#  #   scale_x_date(date_breaks = "4 weeks") +
#     labs(y="Weekly Fatalities",
#          title="Unrest in America 2020-Jan 2021",
#          subtitle = "Weekly Fatality Trends during Riots and Demonstrations",
#          caption = attrib,
#          x = "Date")f
