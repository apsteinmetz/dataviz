library(chromote)
library(rvest)
library(xml2)
library(stringr)
library(dplyr)
library(readr)
library(purrr)

credit_url <- "https://www.imdb.com/title/tt1561755/fullcredits/?ref_=tt_cl_sm#cast"
credit_url <- "https://www.imdb.com/title/tt1561755/fullcredits/"
OUT_CSV <- "bobs_burgers_one_episode_guest_stars.csv"
# User agent to mimic a real browser

USER_AGENT <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36"

b <- ChromoteSession$new()
b$Browser$getVersion()

b$Network$setUserAgentOverride(userAgent = USER_AGENT, wait_ = FALSE)
p <- b$Page$loadEventFired(wait_ = FALSE)

b$view()
b$go_to(credit_url)
page <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
page_b <- read_html(page)

b$close()

#__next > main > div > section > div > section > div > div.sc-e1aae3e0-1.eEFIsG.ipc-page-grid__item.ipc-page-grid__item--span-2 > section:nth-child(9) > div.sc-314065ad-0.hZXevt.full-credits-page-container > ul > li:nth-child(48)
#__next > main > div > section > div > section > div > div.sc-e1aae3e0-1.eEFIsG.ipc-page-grid__item.ipc-page-grid__item--span-2 > section:nth-child(9) > div.sc-314065ad-0.hZXevt.full-credits-page-container > ul > li:nth-child(48) > div
# convert page to html
all_items <- html_nodes(page_b, ".ipc-page-section--bp-none") |>
    html_nodes("ul") |>
    html_nodes("li")

all_items[240] |> html_text2()
