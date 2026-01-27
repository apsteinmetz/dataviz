library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(readr)

clean_text <- function(x) x |> str_replace_all("\\s+", " ") |> str_trim()

html <- read_file("Imdb.html")
doc <- read_html(html)

all_items <- html_elements(doc, 'li[data-testid="name-credits-list-item"]')

cast_one_ep <- list()

for (item in all_items) {
  # Check if this is a CAST item by looking for /characters/ links
  char_links <- html_elements(item, 'a[href*="/characters/"]')
  if (length(char_links) == 0) {
    next
  }

  # Episode count
  ep_btn <- html_element(item, "button.ipc-link")
  ep_txt <- ep_btn |> html_text2() |> clean_text()

  if (is.na(ep_txt)) {
    next
  }
  if (!str_detect(ep_txt, regex("^1\\s+episode$", ignore_case = TRUE))) {
    next
  }

  # Actor name and link
  actor_node <- html_element(item, "a.name-credits--title-text")
  actor <- actor_node |> html_text2() |> clean_text()
  actor_href <- html_attr(actor_node, "href")

  # Character names and character link
  char_names <- char_links |>
    html_text2() |>
    map_chr(clean_text) |>
    keep(~ .x != "" && .x != "...")

  character <- paste(char_names, collapse = " / ")

  # Get the first character link (for visiting)
  char_href <- html_attr(char_links[1], "href")

  # Year
  item_text <- item |> html_text2()
  year_match <- str_match(item_text, "1 episode\\s*[•·]\\s*(20\\d{2})")
  year <- if (!is.na(year_match[1, 2])) year_match[1, 2] else NA_character_

  cast_one_ep[[length(cast_one_ep) + 1]] <- list(
    actor = actor,
    actor_url = paste0("https://www.imdb.com", actor_href),
    character = character,
    character_url = paste0("https://www.imdb.com", char_href),
    year = year
  )
}

cat("One-episode cast members with links:\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n\n")

for (i in seq_along(cast_one_ep)[1:min(5, length(cast_one_ep))]) {
  cat(sprintf(
    "%d. %s (%s)\n",
    i,
    cast_one_ep[[i]]$actor,
    cast_one_ep[[i]]$year
  ))
  cat(sprintf("   Character: %s\n", cast_one_ep[[i]]$character))
  cat(sprintf("   Character URL: %s\n\n", cast_one_ep[[i]]$character_url))
}
