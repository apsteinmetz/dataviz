# gender guesser
library(dplyr)
library(readr)
library(tidyr)

names <- read_csv("data/names/yob1905.txt",
                     col_names=c("first","gender","count"),
                     col_types = list(col_character(),col_character(),col_number()))
names_prob <- names %>% 
  select(first,gender,count) %>% 
  pivot_wider(names_from = gender,values_from=count,values_fill = 0) %>% 
  mutate(prob_female=F/(F+M)) %>% 
  {.}

guess_gender <- function(name,confidence=0.6){
  gender = "U"
  guess <- names_prob %>% 
    filter(tolower(first) == tolower(name))
  if(nrow(guess) ==1 ) {
    if (guess$prob_female > confidence) gender <- "F"
    if (1-guess$prob_female > confidence) gender <- "M"
  }
  return(gender)
}

guess_gender_v <- Vectorize(guess_gender)

genders_1905 <- names_prob %>% transmute(first,
                         gender=guess_gender_v(first)) %>% 
  mutate(gender = as.factor(gender))

save(genders_1905,file="data/genders_1905")
