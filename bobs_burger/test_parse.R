library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(readr)

clean_text <- function(x) x |> str_replace_all("\\s+", " ") |> str_trim()

html <- read_file("Imdb.html")
doc <- read_html(html)

all_items <- html_elements(doc, 'li[data-testid="name-credits-list-item"]')
cat("Total list items:", length(all_items), "\n")

cast_count <- 0
one_ep_cast <- list()

for (item in all_items) {
  # Check if this is a CAST item by looking for /characters/ links
  char_links <- html_elements(item, 'a[href*="/characters/"]')
  if (length(char_links) == 0) {
    next
  } # Skip non-cast items

  cast_count <- cast_count + 1

  # Episode count
  ep_btn <- html_element(item, "button.ipc-link")
  ep_txt <- ep_btn |> html_text2() |> clean_text()

  if (is.na(ep_txt)) {
    next
  }

  # Check if exactly "1 episode"
  if (!str_detect(ep_txt, regex("^1\\s+episode$", ignore_case = TRUE))) {
    next
  }

  # Actor name
  actor_node <- html_element(item, "a.name-credits--title-text")
  actor <- actor_node |> html_text2() |> clean_text()

  # Character names
  char_names <- char_links |>
    html_text2() |>
    map_chr(clean_text) |>
    keep(~ .x != "" && .x != "...")

  character <- paste(char_names, collapse = " / ")

  # Year
  item_text <- item |> html_text2()
  year_match <- str_match(item_text, "1 episode\\s*[•·]\\s*(20\\d{2})")
  year <- if (!is.na(year_match[1, 2])) year_match[1, 2] else NA_character_

  one_ep_cast[[length(one_ep_cast) + 1]] <- list(
    actor = actor,
    character = character,
    year = year
  )
}

# convert to data frame
one_ep_cast_df <- bind_rows(lapply(one_ep_cast, as.data.frame))


cat("Cast items (with /characters/ links):", cast_count, "\n")
cat("Cast with '1 episode':", length(one_ep_cast), "\n\n")

cat("First 10 one-episode cast members:\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
for (i in seq_along(one_ep_cast)[1:min(10, length(one_ep_cast))]) {
  cat(sprintf(
    "%d. %s - %s (%s)\n",
    i,
    one_ep_cast[[i]]$actor,
    one_ep_cast[[i]]$character,
    one_ep_cast[[i]]$year
  ))
}
