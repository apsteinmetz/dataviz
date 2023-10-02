# get lyrics for an artist
library(genius)
library(tidyverse)
library(tidytext)

#make sure you have this
geniusr::genius_token()

# get all lyrics from an album
#lyrics <- genius::genius_album("Ramones","¡Adios Amigos!")

ramones_albums <- tribble(
  ~album, ~year,
  "Ramones", 1976,
  "Leave Home", 1977,
  "Rocket To Russia", 1977,
  "Road To Ruin", 1978,
  "End Of The Century", 1980,
  "Pleasant Dreams", 1981,
  "Subterranean Jungle", 1983,
  "Too Tough To Die", 1984,
  "Animal Boy", 1986,
  "Halfway To Sanity",1987,
  "Brain Drain",1989,
  "Mondo Bizarro",1992,
  "Acid Eaters",1993,
  "¡Adios Amigos!",1995
)
artist_albums <- ramones_albums %>% 
  mutate(artist="Ramones") %>% 
  select(artist,album) %>%
  {.}

if (file.exists("data/ramones_lyrics_genius.rdata")){
  load("data/ramones_lyrics_genius.rdata")
} else {
  ramones_lyrics_genius <- genius::add_genius(artist_albums,artist,album)
  save(ramones_lyrics_genius,file="data/ramones_lyrics_genius.rdata")
}


lyric_words <- ramones_lyrics_genius %>%  unnest_tokens(word,lyric)

