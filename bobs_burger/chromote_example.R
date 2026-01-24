# Source - https://stackoverflow.com/a/76307302
# Posted by margusl, modified by community. See post 'Timeline' for change history
# Retrieved 2026-01-23, License - CC BY-SA 4.0

library(tidyverse)
library(rvest)

### update /
library(chromote)
# new sessio, set userAgent, value grabbed from current Chrome wof Windows
b <- ChromoteSession$new()
b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")
#> named list()
### / update 

# Functions
get_urls_in_table <- function(master_link) {
  ### update /
  {
    b$Page$navigate(master_link)
    b$Page$loadEventFired()
  } 
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  ### / update
  links <- read_html(with_user_agent) %>% html_nodes("td a") %>% html_attr('href') %>% as.data.frame()
  return(links)
}

get_content <- function(x) {
  ### update /  
  {
    b$Page$navigate(x)
    b$Page$loadEventFired()
  } 
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  ### / update
  content <- read_html(with_user_agent) %>% html_nodes(".styled") %>% html_text()
  print("getting content")
  # Sys.sleep(30) # Here add 30 sec between each scraping attempt to prevent the queries from becoming too frequent
  return(content)
}

# Layer 1
senate_pages <- get_urls_in_table("https://www.congress.gov/congressional-record/108th-congress/browse-by-date")

senate_pages <- senate_pages %>%
  mutate(real_url = paste("https://www.congress.gov", ., sep = "")) %>%
  filter(grepl("senate",real_url))

# Layer 2
senate_articles <- lapply(senate_pages$real_url[1:2],get_urls_in_table) 
# Here I only ran the first two issues, but this should work for all if you get rid of [1:2]

senate_articles <- as.data.frame(do.call(rbind, senate_articles)) %>% # Take data frames out of lists
  mutate(real_url = paste("https://www.congress.gov", ., sep = "")) %>%
  filter(grepl("article",real_url))

# Layer 3
senate_content <- lapply(senate_articles$real_url[1:3],get_content)
#> [1] "getting content"
#> [1] "getting content"
#> [1] "getting content"
# Again only running the first few, but should work on a larger scale
senate_content <- as.data.frame(do.call(rbind, senate_content)) # Data clean should follow this

### update /
# close Chromote session
b$close()
#> [1] TRUE
