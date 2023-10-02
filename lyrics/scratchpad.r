# scratchpad
ramones_lyrics %>% 
  select(-band,-lyric_url) %>% 
  unnest_tokens(ngram,lyrics,token = "ngrams",n=10) %>% 
#  group_by(ngram) %>% 
  filter(str_detect(ngram,"i don't want to be buried in a pet sematary"))
  {.}

get_lyric <- function(lyric_url){
  lyric <- bow(lyric_url) %>% 
    scrape() %>% 
    #    /html/body/div[4]/div/div[2]/div[5]
    # body > div.container.main-page > div > div.col-xs-12.col-lg-8.text-center > div:nth-child(8)
    html_nodes("div") %>%
    #not happy with this.  relies on the lyrics always being the 22nd `<div>` tag on the page.
    .[20] %>% 
    html_text() %>%
    str_remove_all("\r") %>% 
    str_replace_all("\n"," ") %>% 
    str_trim() %>% 
    {.}
  return(lyric)
}

lyric <- ramones_lyrics %>% 
  filter(song_name=="Pet Sematary") %>% 
  pull(lyric_url) %>% 
  get_lyric()
