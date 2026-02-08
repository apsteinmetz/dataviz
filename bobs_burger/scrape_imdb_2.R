# Scrape Bob's Burgers episode data from IMDB
# Extracts: season, episode, title, aired_date, synopsis, imdb_rating, imdb_vote_count, thumbnail_url, imdb_episode_url

library(tidyverse)
library(rvest)
library(chromote)

# Function to extract episode data from a single season page
scrape_season <- function(b, season_num) {
  url <- sprintf(
    "https://www.imdb.com/title/tt1561755/episodes/?season=%d&ref_=ttep",
    season_num
  )

  message(sprintf("Scraping season %d...", season_num))

  # Navigate to the page
  b$Page$navigate(url)
  Sys.sleep(4) # Wait for page to load

  # Get page HTML
  html_content <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  page <- read_html(html_content)

  # Find all episode article elements
  episodes <- page |> html_elements("article.episode-item-wrapper")

  if (length(episodes) == 0) {
    message(sprintf("  No episodes found for season %d", season_num))
    return(tibble())
  }

  message(sprintf("  Found %d episodes", length(episodes)))

  # Extract data from each episode
  episode_data <- map_dfr(episodes, function(ep) {
    # Thumbnail URL - from img src
    thumbnail_url <- ep |>
      html_element("img.ipc-image") |>
      html_attr("src") |>
      (\(x) if (is.na(x)) "" else x)()

    # Episode URL - from lockup overlay link
    ep_href <- ep |>
      html_element("a.ipc-lockup-overlay") |>
      html_attr("href")
    imdb_episode_url <- if (is.na(ep_href)) {
      ""
    } else {
      paste0("https://www.imdb.com", ep_href)
    }

    # Title text contains "Sn.En ∙ Title"
    title_text <- ep |>
      html_element("div.ipc-title__text") |>
      html_text2() |>
      (\(x) if (is.na(x)) "" else x)()

    # Parse season/episode from title (format: "S1.E1 ∙ Title")
    se_match <- str_match(title_text, "S(\\d+)\\.E(\\d+)\\s*∙?\\s*(.+)")
    parsed_season <- if (!is.na(se_match[1, 2])) {
      as.integer(se_match[1, 2])
    } else {
      season_num
    }
    parsed_episode <- if (!is.na(se_match[1, 3])) {
      as.integer(se_match[1, 3])
    } else {
      NA_integer_
    }
    title <- if (!is.na(se_match[1, 4])) {
      str_trim(se_match[1, 4])
    } else {
      title_text
    }

    # Aired date
    aired_date <- ep |>
      html_element("span.sc-5372d523-10") |>
      html_text2() |>
      (\(x) if (is.na(x)) "" else x)()

    # Synopsis
    synopsis <- ep |>
      html_element("div.ipc-html-content-inner-div") |>
      html_text2() |>
      (\(x) if (is.na(x)) "" else x)()

    # IMDB rating
    imdb_rating <- ep |>
      html_element("span.ipc-rating-star--rating") |>
      html_text2() |>
      (\(x) if (is.na(x)) NA_real_ else as.numeric(x))()

    # Vote count - in parentheses like "( 2.5K )"
    vote_text <- ep |>
      html_element("span.ipc-rating-star--voteCount") |>
      html_text2() |>
      (\(x) if (is.na(x)) "" else x)()

    # Clean vote count - remove parentheses and convert K/M
    imdb_vote_count <- vote_text |>
      str_remove_all("[\\(\\)\\s]") |>
      (\(x) {
        if (x == "" || is.na(x)) {
          return(NA_real_)
        }
        multiplier <- case_when(
          str_detect(x, "K$") ~ 1000,
          str_detect(x, "M$") ~ 1000000,
          TRUE ~ 1
        )
        as.numeric(str_remove(x, "[KM]$")) * multiplier
      })()

    tibble(
      season = parsed_season,
      episode = parsed_episode,
      title = title,
      aired_date = aired_date,
      synopsis = synopsis,
      imdb_rating = imdb_rating,
      imdb_vote_count = imdb_vote_count,
      thumbnail_url = thumbnail_url,
      imdb_episode_url = imdb_episode_url
    )
  })

  episode_data
}

# Main execution
message("Starting IMDB Bob's Burgers scrape...")

# Start chromote browser with user agent to avoid 403 errors
b <- ChromoteSession$new()
b$Network$setUserAgentOverride(
  userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)

# Scrape all 16 seasons
all_episodes <- map_dfr(1:16, ~ scrape_season(b, .x))

# Close browser
b$close()

message(sprintf("\nScraped %d total episodes", nrow(all_episodes)))

# Preview data
message("\nData preview:")
print(all_episodes, n = 20)

# Save to CSV
output_file <- "c:/Users/Apste/Documents/R Projects/dataviz_misc/bobs_burger/imdb_bob.csv"
write_csv(all_episodes, output_file)

message(sprintf("\nSaved to %s", output_file))

# Summary stats
message("\nSummary by season:")
all_episodes |>
  summarise(
    n_episodes = n(),
    avg_rating = mean(imdb_rating, na.rm = TRUE) |> round(2),
    .by = season
  ) |>
  arrange(season) |>
  print(n = 16)
