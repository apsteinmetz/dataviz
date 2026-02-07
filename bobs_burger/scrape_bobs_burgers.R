# Scrape Bob's Burgers Single-Episode Cast Members from IMDB
# Using chromote to interact with JavaScript-driven page
# The page is JavaScript-heavy so we simulate browser interaction

library(tidyverse)
library(rvest)
library(chromote)
library(xml2)

# Target URL
url <- "https://www.imdb.com/title/tt1561755/fullcredits/?ref_=tt_cst_sm"

# Initialize Chromote session with user agent
b <- ChromoteSession$new()
b$Network$setUserAgentOverride(
  userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)

# Navigate to the page
b$Page$navigate(url)
Sys.sleep(8) # Wait for JavaScript to render

# Get the rendered HTML
html_content <- b$Runtime$evaluate(
  "document.querySelector('html').outerHTML"
)$result$value
page <- read_html(html_content)

# Find the Cast section (3rd section on the page)
sections <- page |> html_nodes("[data-testid^='sub-section']")
cast_section <- sections[3]

# Get cast items from Cast section only
cast_items_only <- cast_section |>
  html_nodes("li[data-testid='name-credits-list-item']")

# Filter to single-episode cast members
single_episode_cast <- cast_items_only |>
  keep(
    ~ {
      button_text <- .x |> html_nodes("button.ipc-link") |> html_text()
      any(button_text == "1 episode")
    }
  )

# Find indices of single-episode cast in the section
single_ep_indices <- which(map_lgl(
  cast_items_only,
  ~ {
    button_text <- .x |> html_nodes("button.ipc-link") |> html_text()
    any(button_text == "1 episode")
  }
))

message(sprintf(
  "Found %d single-episode cast members",
  length(single_episode_cast)
))

# Function to extract basic cast info
extract_cast_info <- function(item) {
  actor <- item |>
    html_node("a.name-credits--title-text-big") |>
    html_text() |>
    str_trim()

  character_links <- item |>
    html_nodes("a.ipc-link--inherit-color") |>
    html_text()

  characters <- character_links |>
    discard(~ .x == "...") |>
    paste(collapse = " / ")

  episode_div <- item |>
    html_node("div.sc-9eb08875-1") |>
    html_text()

  year <- str_extract(episode_div, "\\d{4}")

  tibble(Actor = actor, Character = characters, Year = year)
}

# Extract basic info
basic_cast_data <- map_dfr(single_episode_cast, extract_cast_info)

# Function to process a cast member and get season/episode
process_cast_member <- function(b, section_idx, item_idx, actor_name) {
  # Close any existing modal
  b$Runtime$evaluate(
    "
    var closeBtn = document.querySelector('[aria-label=\"Close Prompt\"]');
    if (closeBtn) closeBtn.click();
  "
  )
  Sys.sleep(0.3)

  # Click the episode button
  js_click <- sprintf(
    "
    (function() {
      var sections = document.querySelectorAll('[data-testid^=\"sub-section\"]');
      var castSection = sections[%d];
      if (!castSection) return 'section not found';
      var items = castSection.querySelectorAll('li[data-testid=\"name-credits-list-item\"]');
      var item = items[%d];
      if (!item) return 'item not found';
      var button = item.querySelector('button.ipc-link');
      if (!button) return 'button not found';
      button.click();
      return 'clicked';
    })()
  ",
    section_idx - 1,
    item_idx - 1
  )

  click_result <- b$Runtime$evaluate(js_click)$result$value
  if (click_result != "clicked") {
    return(list(season = NA_integer_, episode = NA_integer_))
  }

  Sys.sleep(2) # Wait for modal

  # Get modal content
  modal_text <- b$Runtime$evaluate(
    "
    (function() {
      var modal = document.querySelector('[role=\"dialog\"]');
      return modal ? modal.textContent : '';
    })()
  "
  )$result$value

  # Extract S##.E## pattern
  season_ep_match <- str_extract(modal_text, "S(\\d+)\\.E(\\d+)")

  # Close modal
  b$Runtime$evaluate(
    "
    var closeBtn = document.querySelector('[aria-label=\"Close Prompt\"]');
    if (closeBtn) closeBtn.click();
    else {
      var backdrop = document.querySelector('.ipc-promptable-base__backdrop');
      if (backdrop) backdrop.click();
    }
  "
  )
  Sys.sleep(0.3)

  if (!is.na(season_ep_match)) {
    season <- as.integer(str_extract(season_ep_match, "(?<=S)\\d+"))
    episode <- as.integer(str_extract(season_ep_match, "(?<=E)\\d+"))
    return(list(season = season, episode = episode))
  }
  return(list(season = NA_integer_, episode = NA_integer_))
}

# Process all cast members
results <- basic_cast_data |>
  mutate(Season = NA_integer_, Episode = NA_integer_)

message("Processing cast members...")
for (i in seq_along(single_ep_indices)) {
  actor <- basic_cast_data$Actor[i]
  ep_info <- process_cast_member(b, 3, single_ep_indices[i], actor)
  results$Season[i] <- ep_info$season
  results$Episode[i] <- ep_info$episode

  if (i %% 20 == 0) {
    message(sprintf("Processed %d/%d...", i, length(single_ep_indices)))
  }
}

# Clean up Character column
results_clean <- results |>
  mutate(
    Character = str_replace_all(Character, "\\(voice\\)", "") |> str_trim(),
    Character = str_replace_all(Character, "\\(uncredited\\)", "") |> str_trim()
  ) |>
  select(Actor, `Character(s)` = Character, Year, Season, Episode)

# Save to CSV
write_csv(results_clean, "bobs_burgers_single_episode_cast.csv")
message(sprintf(
  "\nSaved %d records to bobs_burgers_single_episode_cast.csv",
  nrow(results_clean)
))

# Close browser
b$close()

# Preview
print(head(results_clean, 20))
