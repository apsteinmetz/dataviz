Sys.setenv(CHROMOTE_HEADLESS = "false")

library(chromote)
library(rvest)
library(xml2)
library(stringr)
library(dplyr)
library(readr)
library(purrr)

FULLCREDITS_URL <- "https://www.imdb.com/title/tt1561755/fullcredits/?ref_=tt_cl_sm#cast"
OUT_CSV <- "bobs_burgers_one_episode_guest_stars.csv"

# User agent to mimic a real browser
USER_AGENT <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

# Set up browser to look more like a real user
setup_browser_stealth <- function(tab) {
  # Set user agent
  tryCatch(
    {
      tab$Network$setUserAgentOverride(
        userAgent = USER_AGENT,
        acceptLanguage = "en-US,en;q=0.9",
        platform = "Win32"
      )
    },
    error = function(e) message("Could not set user agent: ", e$message)
  )

  # Disable webdriver detection flags
  stealth_js <- '
    // Remove webdriver property
    Object.defineProperty(navigator, "webdriver", {
      get: () => undefined
    });
    
    // Mock plugins
    Object.defineProperty(navigator, "plugins", {
      get: () => [1, 2, 3, 4, 5]
    });
    
    // Mock languages
    Object.defineProperty(navigator, "languages", {
      get: () => ["en-US", "en"]
    });
    
    // Mock permissions
    const originalQuery = window.navigator.permissions.query;
    window.navigator.permissions.query = (parameters) => (
      parameters.name === "notifications" ?
        Promise.resolve({ state: Notification.permission }) :
        originalQuery(parameters)
    );
  '

  tryCatch(
    {
      tab$Runtime$evaluate(expression = stealth_js, timeout_ = 5000)
    },
    error = function(e) message("Could not apply stealth: ", e$message)
  )
}

clean_text <- function(x) {
  x |> str_replace_all("\\s+", " ") |> str_trim()
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
get_rendered_html <- function(tab, url1, wait_ms = 2500, timeout = 15000) {
  # Apply stealth settings before navigating
  setup_browser_stealth(tab)

  # Navigate with wait_ = FALSE to avoid blocking
  tab$Page$navigate(url = url1, wait_ = FALSE)

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

# Get current page HTML without navigation
get_current_html <- function(tab, timeout = 15000) {
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

# Click the "1 episode" button for a specific cast member and extract episode info from the popup
# button_index: 0-based index of the "1 episode" button to click
click_episode_button_and_parse <- function(tab, button_index, timeout = 10000) {
  # JavaScript to click the button and wait for popup
  click_js <- sprintf(
    '
    (function() {
      // Find all "1 episode" buttons that are in cast items (have /characters/ link nearby)
      const allItems = document.querySelectorAll("li[data-testid=\'name-credits-list-item\']");
      let oneEpButtons = [];
      
      allItems.forEach(item => {
        // Check if this is a cast item (has /characters/ link)
        const charLink = item.querySelector("a[href*=\'/characters/\']");
        if (!charLink) return;
        
        // Find the episode button
        const btn = item.querySelector("button.ipc-link");
        if (btn && btn.textContent.trim().toLowerCase() === "1 episode") {
          oneEpButtons.push(btn);
        }
      });
      
      if (%d >= oneEpButtons.length) {
        return {error: "Button index out of range", total: oneEpButtons.length};
      }
      
      // Click the button
      oneEpButtons[%d].click();
      return {success: true, total: oneEpButtons.length};
    })()
  ',
    button_index,
    button_index
  )

  # Execute the click
  click_result <- tryCatch(
    {
      tab$Runtime$evaluate(
        expression = click_js,
        returnByValue = TRUE,
        timeout_ = timeout
      )
    },
    error = function(e) {
      message("Error clicking button: ", e$message)
      return(NULL)
    }
  )

  if (is.null(click_result)) {
    return(list(success = FALSE, error = "Click failed"))
  }

  # Wait for popup to appear
  Sys.sleep(1.0)

  # Extract episode info from the popup (bottomsheet)
  # Format is on second-to-last line: S<season>.E<episode>  <episode title> (<year>)
  parse_popup_js <- '
    (function() {
      // Look for the popup/bottomsheet that appears after clicking
      const popup = document.querySelector("[data-testid=\'promptable\']") ||
                    document.querySelector(".ipc-promptable-base") ||
                    document.querySelector("[role=\'dialog\']") ||
                    document.querySelector(".episodic-credits-bottomsheet");
      
      if (!popup) {
        return {error: "Popup not found"};
      }
      
      const popupText = popup.innerText;
      const lines = popupText.split("\\n").filter(line => line.trim() !== "");
      
      // Get second-to-last line which has the episode info
      // Format: S4.E7  Bob and Deliver (2013)
      let episodeLine = "";
      if (lines.length >= 2) {
        episodeLine = lines[lines.length - 2].trim();
      } else if (lines.length === 1) {
        episodeLine = lines[0].trim();
      }
      
      // Parse: S<season>.E<episode>  <title> (<year>)
      // Regex: S(\\d+)\\.E(\\d+)\\s+(.+?)\\s*\\((\\d{4})\\)
      const match = episodeLine.match(/S(\\d+)\\.E(\\d+)\\s+(.+?)\\s*\\((\\d{4})\\)/);
      
      if (match) {
        return {
          season: parseInt(match[1]),
          episode: parseInt(match[2]),
          title: match[3].trim(),
          year: match[4],
          popupFound: true,
          episodeLine: episodeLine
        };
      }
      
      // Fallback: try to find S#.E# pattern anywhere
      const seMatch = episodeLine.match(/S(\\d+)\\.E(\\d+)/);
      if (seMatch) {
        // Try to get title after S#.E#
        const afterSE = episodeLine.substring(episodeLine.indexOf(seMatch[0]) + seMatch[0].length).trim();
        const titleMatch = afterSE.match(/^(.+?)(?:\\s*\\((\\d{4})\\))?$/);
        
        return {
          season: parseInt(seMatch[1]),
          episode: parseInt(seMatch[2]),
          title: titleMatch ? titleMatch[1].trim() : null,
          year: titleMatch && titleMatch[2] ? titleMatch[2] : null,
          popupFound: true,
          episodeLine: episodeLine
        };
      }
      
      return {
        error: "Could not parse episode info",
        popupFound: true,
        popupText: popupText.substring(0, 500),
        episodeLine: episodeLine
      };
    })()
  '

  popup_result <- tryCatch(
    {
      tab$Runtime$evaluate(
        expression = parse_popup_js,
        returnByValue = TRUE,
        timeout_ = timeout
      )
    },
    error = function(e) {
      message("Error parsing popup: ", e$message)
      return(NULL)
    }
  )

  # Close the popup by pressing Escape or clicking outside
  close_popup_js <- '
    (function() {
      // Try pressing Escape
      document.dispatchEvent(new KeyboardEvent("keydown", {key: "Escape", keyCode: 27}));
      
      // Also try clicking any close button
      const closeBtn = document.querySelector("[aria-label=\'Close\']") ||
                       document.querySelector(".ipc-promptable-base__close") ||
                       document.querySelector("button[data-testid=\'promptable-close\']");
      if (closeBtn) closeBtn.click();
      
      return true;
    })()
  '

  tryCatch(
    {
      tab$Runtime$evaluate(expression = close_popup_js, timeout_ = 5000)
    },
    error = function(e) {}
  )

  Sys.sleep(0.5)

  if (is.null(popup_result) || !is.null(popup_result$result$value$error)) {
    return(list(
      success = FALSE,
      error = popup_result$result$value$error %||% "Unknown error"
    ))
  }

  val <- popup_result$result$value
  list(
    success = TRUE,
    season = val$season,
    episode = val$episode,
    title = val$title,
    popup_found = val$popupFound %||% FALSE,
    debug_text = val$popupText
  )
}

# Get total count of "1 episode" cast buttons
get_one_episode_button_count <- function(tab, timeout = 10000) {
  count_js <- '
    (function() {
      const allItems = document.querySelectorAll("li[data-testid=\'name-credits-list-item\']");
      let count = 0;
      
      allItems.forEach(item => {
        const charLink = item.querySelector("a[href*=\'/characters/\']");
        if (!charLink) return;
        
        const btn = item.querySelector("button.ipc-link");
        if (btn && btn.textContent.trim().toLowerCase() === "1 episode") {
          count++;
        }
      });
      
      return count;
    })()
  '

  result <- tab$Runtime$evaluate(
    expression = count_js,
    returnByValue = TRUE,
    timeout_ = timeout
  )

  result$result$value
}

# Parse the full credits cast section for rows with "1 episode"
# Returns actor/character/year info - episode details come from clicking buttons
parse_one_episode_cast_rows <- function(html) {
  doc <- read_html(html)

  all_items <- html_elements(doc, 'li[data-testid="name-credits-list-item"]')

  out <- list()

  for (item in all_items) {
    # Check if this is a CAST item by looking for /characters/ links
    char_links <- html_elements(item, 'a[href*="/characters/"]')
    if (length(char_links) == 0) {
      next
    }

    # Episode count - button with text like "1 episode"
    ep_button <- html_element(item, "button.ipc-link")
    ep_text <- ep_button |> html_text2() |> clean_text()

    if (is.na(ep_text)) {
      next
    }
    if (!str_detect(ep_text, regex("^1\\s+episode$", ignore_case = TRUE))) {
      next
    }

    # Actor name
    actor_node <- html_element(item, "a.name-credits--title-text")
    actor <- actor_node |> html_text2() |> clean_text()
    if (is.na(actor) || actor == "") {
      next
    }

    # Character name(s)
    char_names <- char_links |>
      html_text2() |>
      purrr::map_chr(clean_text) |>
      purrr::keep(~ .x != "" && .x != "...")
    character <- paste(char_names, collapse = " / ")
    if (character == "") {
      character <- NA_character_
    }

    # Year from text after button
    item_text <- item |> html_text2()
    year_match <- str_match(item_text, "1 episode\\s*[•·]\\s*(20\\d{2})")
    year <- if (!is.na(year_match[1, 2])) year_match[1, 2] else NA_character_

    out[[length(out) + 1]] <- tibble(
      Actor = actor,
      Character = character,
      Year = year
    )
  }

  if (length(out) == 0) {
    return(tibble(
      Actor = character(0),
      Character = character(0),
      Year = character(0)
    ))
  }

  bind_rows(out) |> distinct()
}

# MAIN - Click each "1 episode" button to get episode info from popup
bobs_one_episode_guests <- function(headless = FALSE, max_cast = NULL) {
  # Force non-headless mode by creating Chrome with explicit arguments
  # Include flags to reduce automation detection
  if (!headless) {
    browser <- Chrome$new(
      args = c(
        "--disable-gpu",
        "--no-sandbox",
        "--disable-dev-shm-usage",
        "--window-size=1200,900",
        "--disable-blink-features=AutomationControlled",
        "--disable-infobars",
        "--start-maximized"
      )
    )
    chromote_instance <- Chromote$new(browser = browser)
    tab <- ChromoteSession$new(parent = chromote_instance)
  } else {
    tab <- ChromoteSession$new()
  }

  on.exit(
    {
      tryCatch(tab$close(), error = function(e) {})
    },
    add = TRUE
  )

  # Warmup: First visit IMDb homepage to establish a session and look more human
  message("Warming up: visiting IMDb homepage first...")
  tab$Page$navigate(url = "https://www.imdb.com/", wait_ = FALSE)
  tryCatch(
    tab$Page$loadEventFired(timeout_ = 15000),
    error = function(e) message("Homepage load timeout, continuing...")
  )
  Sys.sleep(2) # Wait a bit like a real user

  # Apply stealth settings
  setup_browser_stealth(tab)
  Sys.sleep(1)

  message("Opening IMDb full credits page...")
  html_page <- get_rendered_html(tab, FULLCREDITS_URL, wait_ms = 3000)
  maybe_pause_for_botcheck(tab, html_page)

  # Re-fetch after potential botcheck
  html_page <- get_rendered_html(tab, FULLCREDITS_URL, wait_ms = 3000)

  # Parse cast info from HTML
  cast_info <- parse_one_episode_cast_rows(html_page)
  message(sprintf(
    "Found %d cast members with '1 episode' in HTML.",
    nrow(cast_info)
  ))

  # Get button count from live page
  button_count <- get_one_episode_button_count(tab)
  message(sprintf("Found %d clickable '1 episode' buttons.", button_count))

  if (button_count == 0) {
    message("No buttons found. IMDb may have changed markup.")
    return(invisible(NULL))
  }

  # Limit if requested
  if (!is.null(max_cast)) {
    button_count <- min(button_count, max_cast)
    message(sprintf("Processing first %d cast members.", button_count))
  }

  results <- list()

  for (i in seq_len(button_count)) {
    idx <- i - 1 # 0-based for JavaScript

    message(sprintf(
      "Processing %d/%d: %s...",
      i,
      button_count,
      if (i <= nrow(cast_info)) cast_info$Actor[i] else "Unknown"
    ))

    ep_info <- click_episode_button_and_parse(tab, idx)

    actor <- if (i <= nrow(cast_info)) cast_info$Actor[i] else NA_character_
    character <- if (i <= nrow(cast_info)) {
      cast_info$Character[i]
    } else {
      NA_character_
    }
    year <- if (i <= nrow(cast_info)) cast_info$Year[i] else NA_character_

    if (ep_info$success) {
      results[[i]] <- tibble(
        Actor = actor,
        Character = character,
        Year = year,
        Season = ep_info$season %||% NA_integer_,
        Episode = ep_info$episode %||% NA_integer_,
        EpisodeTitle = ep_info$title %||% NA_character_
      )

      if (!is.null(ep_info$debug_text) && is.na(ep_info$season)) {
        message("  Debug popup text: ", substr(ep_info$debug_text, 1, 100))
      }
    } else {
      message(sprintf("  Failed to get episode info: %s", ep_info$error))
      results[[i]] <- tibble(
        Actor = actor,
        Character = character,
        Year = year,
        Season = NA_integer_,
        Episode = NA_integer_,
        EpisodeTitle = NA_character_
      )
    }

    Sys.sleep(0.5) # Be polite
  }

  final_df <- bind_rows(results)

  write_csv(final_df, OUT_CSV)
  message(sprintf("Wrote %d rows to %s", nrow(final_df), OUT_CSV))

  final_df
}

# Run it
df <- bobs_one_episode_guests(max_cast = 5) # Test with first 5
print(df)
