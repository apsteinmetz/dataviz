library(chromote)
library(rvest)
library(xml2)
library(stringr)
library(dplyr)
library(readr)
library(purrr)

FULLCREDITS_URL <- "https://www.imdb.com/title/tt1561755/fullcredits/?ref_=tt_cl_sm#cast"
OUT_CSV <- "bobs_burgers_one_episode_guest_stars.csv"

clean_text <- function(x) {
  x %>% str_replace_all("\\s+", " ") %>% str_trim()
}

extract_se_ep <- function(text) {
  text <- clean_text(text)

  # Pattern 1: S3.E15
  m1 <- str_match(text, "(?i)S\\s*(\\d+)\\s*\\.\\s*E\\s*(\\d+)")
  if (!all(is.na(m1[1, ]))) {
    return(list(season = as.integer(m1[1, 2]), episode = as.integer(m1[1, 3])))
  }

  # Pattern 2: Season 3 Episode 15
  m2 <- str_match(text, "(?i)Season\\s*(\\d+).*?Episode\\s*(\\d+)")
  if (!all(is.na(m2[1, ]))) {
    return(list(season = as.integer(m2[1, 2]), episode = as.integer(m2[1, 3])))
  }

  list(season = NA_integer_, episode = NA_integer_)
}

# Fetch fully-rendered HTML via a real browser tab (Chromote)
get_rendered_html <- function(tab, url, wait_ms = 1500, timeout = 10000) {
  # Set a realistic user agent before navigating
  tryCatch(
    {
      tab$Network$setUserAgentOverride(
        userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
        
      )
    },
    error = function(e) {
      message("Could not set user agent: ", e$message)
    }
  )
  
  # Navigate with wait_ = FALSE to avoid blocking
  tab$Page$navigate(url = url, wait_ = FALSE)
  
  # Wait for load event with timeout
  tryCatch(
    {
      tab$Page$loadEventFired(timeout_ = timeout)
    },
    error = function(e) {
      message("Load event timeout, continuing anyway...")
    }
  )
  
  Sys.sleep(wait_ms / 1000)

  # Get the whole document HTML with timeout
  res <- tryCatch(
    {
      tab$Runtime$evaluate(
        expression = "document.documentElement.outerHTML",
        returnByValue = TRUE,
        timeout_ = timeout
      )
    },
    error = function(e) {
      message("Error getting HTML: ", e$message)
      return(list(result = list(value = "")))
    }
  )
  
  res$result$value
}

# Optional: if IMDb throws a bot-check, you can pause to manually solve it
maybe_pause_for_botcheck <- function(tab, html) {
  if (
    str_detect(
      html,
      regex(
        "verify you are a human|robot|captcha|JavaScript is disabled",
        ignore_case = TRUE
      )
    )
  ) {
    message("\nIMDb may be showing a bot-check/captcha in the Chrome window.")
    message("1) Look at the Chrome window that opened and complete the check.")
    message("2) Then press ENTER here to continue...\n")
    invisible(readline())
  }
}

# Parse the full credits cast table for rows with "1 episode"
parse_one_episode_cast_rows <- function(html) {
  doc <- read_html(html)

  # Classic IMDb cast list table is `table.cast_list`
  trs <- html_elements(doc, "table.cast_list tr")

  out <- list()

  for (tr in trs) {
    tds <- html_elements(tr, "td")
    if (length(tds) < 4) {
      next
    }

    actor <- tr %>%
      html_element("td:nth-child(2) a") %>%
      html_text2() %>%
      clean_text()
    character <- tr %>%
      html_element("td.character") %>%
      html_text2() %>%
      clean_text()

    # The episodes cell is usually the last td with something like "1 episode" as link text
    eps_td <- tr %>% html_element("td:last-child")
    if (is.na(html_name(eps_td))) {
      next
    }

    eps_text <- eps_td %>% html_text2() %>% clean_text()

    if (
      !str_detect(eps_text, regex("\\b1\\s+episode\\b", ignore_case = TRUE))
    ) {
      next
    }

    # Find the anchor whose text includes "1 episode"
    a_nodes <- html_elements(eps_td, "a")
    if (length(a_nodes) == 0) {
      next
    }

    one_link <- NA_character_
    for (a in a_nodes) {
      if (
        str_detect(
          clean_text(html_text2(a)),
          regex("\\b1\\s+episode\\b", ignore_case = TRUE)
        )
      ) {
        one_link <- html_attr(a, "href")
        break
      }
    }

    if (is.na(one_link) || is.na(actor) || actor == "") {
      next
    }

    out[[length(out) + 1]] <- tibble(
      Actor = actor,
      Character = character,
      OneEpisodeLink = paste0("https://www.imdb.com", one_link)
    )
  }

  if (length(out) == 0) {
    return(tibble(
      Actor = character(0),
      Character = character(0),
      OneEpisodeLink = character(0)
    ))
  }

  bind_rows(out) %>%
    distinct()
}

# From the "1 episode" link page, extract the specific episode title (and season/ep if found)
# This page format can vary; we use robust heuristics:
# - First non-series title link
# - Then look for Sx.Ey patterns in page text
parse_episode_from_one_ep_page <- function(html) {
  doc <- read_html(html)

  # Candidate title links
  title_links <- html_elements(doc, 'a[href^="/title/tt"]')
  titles <- title_links %>% html_text2() %>% map_chr(clean_text)

  # Pick first title that isn't just the series name
  ep_title <- NA_character_
  if (length(titles) > 0) {
    for (t in titles) {
      if (
        t != "" &&
          !str_detect(t, regex("^Bob'?s\\s+Burgers$", ignore_case = TRUE))
      ) {
        ep_title <- t
        break
      }
    }
  }
  if (is.na(ep_title) || ep_title == "") {
    ep_title <- "(episode title not found)"
  }

  body_text <- doc %>% html_text2() %>% clean_text()
  se <- extract_se_ep(body_text)

  list(EpisodeTitle = ep_title, Season = se$season, EpisodeNumber = se$episode)
}

# MAIN
bobs_one_episode_guests <- function() {
  b <- Chromote$new()
  tab <- b$new_session()

  message("Opening IMDb full credits in a real browser tab...")

  html <- get_rendered_html(tab, FULLCREDITS_URL, wait_ms = 2000)
  maybe_pause_for_botcheck(tab, html)

  # Re-fetch after manual botcheck if needed
  html <- get_rendered_html(tab, FULLCREDITS_URL, wait_ms = 2000)

  cast_one <- parse_one_episode_cast_rows(html)
  message(sprintf("Found %d cast lines with '1 episode'.", nrow(cast_one)))

  if (nrow(cast_one) == 0) {
    message("No rows found. IMDb may have changed markup or blocked access.")
    tab$close()
    b$close()
    return(invisible(NULL))
  }

  # Visit each "1 episode" link and extract the specific episode info
  results <- pmap_dfr(
    cast_one,
    function(Actor, Character, OneEpisodeLink) {
      Sys.sleep(0.6) # be polite / reduce risk of throttling

      page_html <- get_rendered_html(tab, OneEpisodeLink, wait_ms = 1500)
      maybe_pause_for_botcheck(tab, page_html)

      # Re-fetch after potential manual action
      page_html <- get_rendered_html(tab, OneEpisodeLink, wait_ms = 1200)

      ep <- parse_episode_from_one_ep_page(page_html)

      tibble(
        Actor = Actor,
        Character = Character,
        Season = ifelse(is.na(ep$Season), "", ep$Season),
        EpisodeNumber = ifelse(is.na(ep$EpisodeNumber), "", ep$EpisodeNumber),
        EpisodeTitle = ep$EpisodeTitle,
        IMDbOneEpisodeLink = OneEpisodeLink
      )
    }
  )

  # Write CSV
  write_csv(results, OUT_CSV)
  message(sprintf("Wrote %d rows to %s", nrow(results), OUT_CSV))

  tab$close()
  b$close()

  results
}
# Run it
df <- bobs_one_episode_guests()
print(head(df, 10))


