library(rvest)
library(dplyr)
library(tibble)
library(robotstxt)
library(stringr)
library(readr)
library(purrr)
library(httr2)

base_url <- "https://www.heavymetalmagazinefanpage.com/hmlist.html"
full_url <- base_url
# Function to check robots.txt and respect crawl delays
check_robots <- function(full_url) {
  cat("Checking robots.txt permissions...\n")
  can_crawl <- paths_allowed(full_url)
  if (!can_crawl) {
    stop("Crawling not allowed by robots.txt")
  }
  cat("Crawling allowed by robots.txt\n")
  return(TRUE)
}

# Function to add polite delays between requests
polite_delay <- function(seconds = 2) {
  Sys.sleep(seconds)
}

# Function to safely read a webpage with error handling using httr2
safe_read_html <- function(full_url) {
  tryCatch(
    {
      polite_delay(2) # Be respectful with delays

      # Create request with httr2
      req <- request(full_url) %>%
        req_timeout(30) %>% # 30 second timeout
        req_user_agent("R web scraper (respectful bot)") %>%
        req_retry(max_tries = 3) # Retry up to 3 times on failure

      # Perform the request
      resp <- req_perform(req)

      # Check if request was successful
      if (resp_status(resp) != 200) {
        cat("HTTP error", resp_status(resp), "for", full_url, "\n")
        return(NULL)
      }

      # Get the content and parse as HTML
      content <- resp_body_html(resp)
    },
    error = function(e) {
      cat("Error reading", full_url, ":", e$message, "\n")
      return(NULL)
    }
  )
}

# Function to check if full_url is accessible using httr2
check_url_accessible <- function(full_url) {
  tryCatch(
    {
      req <- request(full_url) %>%
        req_method("HEAD") %>% # Use HEAD request for efficiency
        req_timeout(10) %>%
        req_user_agent("R web scraper (respectful bot)")

      resp <- req_perform(req)
      return(resp_status(resp) == 200)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# Function to find all TOC links on the base page
scrape_year_full_urls <- function(base_url) {
  page <- safe_read_html(base_url)
  if (is.null(page)) {
    return(tibble())
  }

  # Extract the table with idText31
  table_node <- html_node(page, "#idText31")

  if (is.null(table_node)) {
    cat("No table found with idText31\n")
    return(tibble())
  }

  # Extract all links from the table
  links <- html_nodes(table_node, "a") %>% html_attr("href")

  # Filter out NA and empty links
  links <- links[!is.na(links) & links != ""]
  #filter out links not containing the string "list" followed by two digits and ".html"
  links <- links[str_detect(links, "list\\d{2}\\.html$")]
  base_domain <- str_extract(full_url, "^https?://[^/]+")

  # Convert relative full_urls to absolute full_urls, leave absolute full_urls unchanged
  links <- ifelse(
    str_detect(links, "^https?://"),
    links,
    paste0(base_domain, "/", links)
  )

  return(links)
}

# function that extracts page data
extract_page_data <- function(parsed_toc) {
  # First, let's examine the unique location patterns
  # unique_locations <- unique(parsed_toc$location)
  # print("Unique location patterns:")
  # print(unique_locations)
  
  # Create function to extract start page and page count
  extract_page_info <- function(location) {
    if (is.na(location)) {
      return(list(start_page = NA_integer_, page_count = NA_integer_))
    }
    
    if (location == "Cover") {
      return(list(start_page = 0L, page_count = 1L))
    }
    
    if (location == "Back Cover") {
      return(list(start_page = NA_integer_, page_count = 1L)) # Will handle this separately
    }
    
    # For page locations starting with "p."
    if (str_detect(location, "^p\\.")) {
      # Remove "p." prefix
      page_part <- str_remove(location, "^p\\.")
      
      # Handle different formats:
      # p.03 -> start: 3, count: 1
      # p.29-36 -> start: 29, count: 8
      # p.55-57, 78 -> start: 55, count: 4 (55,56,57,78)
      # p.05,55-57 -> start: 5, count: 4 (5,55,56,57)
      
      if (str_detect(page_part, ",")) {
        # Contains comma - extract all individual pages and ranges
        parts <- str_split(page_part, ",\\s*")[[1]]
        all_pages <- c()
        
        for (part in parts) {
          if (str_detect(part, "-")) {
            # Range like "55-57"
            range_parts <- str_split(part, "-")[[1]]
            start <- as.integer(range_parts[1])
            end <- as.integer(range_parts[2])
            all_pages <- c(all_pages, start:end)
          } else {
            # Single page
            all_pages <- c(all_pages, as.integer(part))
          }
        }
        
        start_page <- min(all_pages)
        page_count <- length(all_pages)
      } else if (str_detect(page_part, "-")) {
        # Simple range like "29-36"
        range_parts <- str_split(page_part, "-")[[1]]
        start_page <- as.integer(range_parts[1])
        end_page <- as.integer(range_parts[2])
        page_count <- end_page - start_page + 1
      } else {
        # Single page like "03"
        start_page <- as.integer(page_part)
        page_count <- 1L
      }
      
      return(list(start_page = start_page, page_count = page_count))
    }
    
    # Default case
    return(list(start_page = NA_integer_, page_count = NA_integer_))
  }
  
  # Apply the function to extract page info
  page_info <- map_dfr(parsed_toc$location, extract_page_info)
  
  # Add the new columns to parsed_toc
  parsed_toc_with_pages <- parsed_toc %>%
    mutate(
      start_page = page_info$start_page,
      page_count = page_info$page_count
    )
  
  # Now handle Back Cover - set its start_page to last article's end page + 1
  # Group by month/volume/issue to handle each issue separately
  parsed_toc_final <- parsed_toc_with_pages %>%
    group_by(month, volume, issue) %>%
    mutate(
      # Calculate end page for each article
      end_page = ifelse(
        is.na(start_page),
        NA_integer_,
        start_page + page_count - 1
      ),
      # For Back Cover, set start_page to max end_page + 1 within each issue
      start_page = ifelse(
        location == "Back Cover",
        max(end_page[location != "Back Cover"], na.rm = TRUE) + 1,
        start_page
      )
    ) %>%
    select(-end_page) %>% # Remove temporary column
    ungroup()
  return(parsed_toc_final)
}

# Function to extract magazine info from full_url
extract_toc_info <- function(page) {
  tables <- html_nodes(page, "table")
  #   extract text that looks like a year from the first table on the page
  year <- tables[1] |>
    html_text() |>
    str_extract("\\d{4}") |>
    as.numeric()
  mag_name <- tables[1] |>
    html_text() |>
    # extract text from beginning to the word Magazine
    str_extract("^(.*?)Magazine|Comic") |> 
    str_remove("Magazine") |>
    str_remove("Comic") |>
    str_trim()

  toc_table <- tables[2] |>
    # get nodes with "FONT" tag from the second table
    html_children() |>
    as.character() |>
    #  remove all \r\n and \t characters
    str_replace_all("[\r\n\t]", "") |>
    # remove all non-ascii characters
    str_replace_all("[^\\x20-\\x7E]", "") |>
    # replace <br> tags with \r\n
    str_replace_all("<br>", "\r\n") |>
    read_html() |>
    html_text() |>
    str_replace_all("Mbius", "Moebius") |>
    str_split("\r\n") |>
    unlist()
  # Remove empty lines
  text_lines <- toc_table[toc_table != ""]
  # if nrow text_lines is less than two, return an empty tibble
  if (length(text_lines) < 2) {
    cat("Not enough content lines found in", mag_name, year, "\n")
    return(tibble())
  }
  

  # Define month names for detection
  month_names <- c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
    # add seasons
    "Winter",
    "Spring",
    "Summer",
    "Fall",
    "Autumn"
  )

  # Create regex pattern for month detection
  month_pattern <- paste0("^(", paste(month_names, collapse = "|"), ")")

  # Reset the parsing variables and tibble
  parsed_toc <- tibble(
    magazine = character(),
    month = character(),
    volume = integer(),
    issue = integer(),
    location = character(),
    title = character(),
    author = character(),
    note = character(),
    start_page = integer(),
    page_count = integer()
  )

  current_month <- NA
  current_volume <- NA
  current_issue <- NA

  for (line in text_lines) {
    # Check if line contains a month (header line)
    if (str_detect(line, month_pattern)) {
      # Extract month, volume, and issue from header
      current_month <- str_extract(line, month_pattern)
      current_volume <- str_extract(line, "Vol\\. (\\d+)") %>%
        str_extract("\\d+") %>%
        as.integer()
      current_issue <- str_extract(line, "No\\. (\\d+)") %>%
        str_extract("\\d+") %>%
        as.integer()
      cat("Processed: Magazine ", mag_name,year, current_month, "Vol.", current_volume, "No.", current_issue, "\n")
      
    } else {
      # This is a content line, parse it
      # Skip if we don't have header info yet
      if (is.na(current_month)) next

      # Parse the line: location - "title" - author (note)
      # But for covers, it's just: location - author
      if (str_detect(line, " - ")) {
        # Check if this is a cover line (Cover or Back Cover)
        if (str_detect(line, "^(Cover|Back Cover) - ")) {
          # For covers: location - author
          location <- str_extract(line, "^(Cover|Back Cover)")
          author <- str_remove(line, "^(Cover|Back Cover) - ") %>% str_trim()

          # Check for note in parentheses
          note_match <- str_extract(author, "\\([^)]+\\)$")
          if (!is.na(note_match)) {
            note <- str_remove_all(note_match, "[()]")
            author <- str_remove(author, "\\s*\\([^)]+\\)$") %>% str_trim()
          } else {
            note <- NA
          }

          title <- NA # No title for covers
        } else {
          # For regular articles: location - "title" - author (note)
          # Extract location (now with improved regex for various page formats)
          # Location can be: p.03, p.29-36, p.55-57, 78, p.05,55-57
          location <- str_extract(line, "^p\\.\\d+(?:[-,]\\d+)*(?:,\\s*\\d+)*")

          if (is.na(location)) {
            # If no page location found, skip this line or handle as error
            next
          }

          # Remove location and first " - " from line to process the rest
          remaining <- str_remove(
            line,
            paste0("^", str_escape(location), "\\s*-\\s*")
          )
          remaining <- str_trim(remaining)

          # Extract title in quotes
          title <- str_extract(remaining, "\"([^\"]+)\"")
          if (!is.na(title)) {
            title <- str_remove_all(title, "\"") # Remove quotes
            # Remove title and following " - " from remaining
            remaining <- str_remove(remaining, "\"[^\"]+\"\\s*-\\s*")
            remaining <- str_trim(remaining)
          } else {
            title <- NA
          }

          # What's left should be author and optional note
          if (!is.na(remaining) && remaining != "") {
            # Check for note in parentheses at the end
            note_match <- str_extract(remaining, "\\([^)]+\\)$")
            if (!is.na(note_match)) {
              note <- str_remove_all(note_match, "[()]")
              author <- str_remove(remaining, "\\s*\\([^)]+\\)$") %>% str_trim()
            } else {
              note <- NA
              author <- str_trim(remaining)
            }
          } else {
            author <- NA
            note <- NA
          }
        }

        # Add to results
        parsed_toc <- bind_rows(
          parsed_toc,
          tibble(
            magazine = mag_name,
            year = year,
            month = current_month,
            volume = current_volume,
            issue = current_issue,
            location = location,
            title = title,
            author = author,
            note = note,
            start_page = NA,
            page_count = NA
          )
        )
      }
    }
  }
  if (nrow(parsed_toc) == 0) {
    cat("No valid content lines found in", mag_name, year, "\n")
    return(tibble())
  }

  parsed_toc <- extract_page_data(parsed_toc)
  cat("Extracted Page Data ", mag_name,year, "Vol.", current_volume,"\n")
  
  return(parsed_toc)
}

get_annual_tocs <- function(full_url) {
  page <- safe_read_html(full_url)
  if (is.null(page)) {
    return(tibble())
  }
  volume_toc <- extract_toc_info(page)
  return(volume_toc)
}  

issue_links <- scrape_year_full_urls(base_url)

# Function to create a rate-limited request handler
create_rate_limiter <- function(requests_per_minute = 30) {
  last_request_time <- Sys.time() - 60
  request_count <- 0

  function() {
    current_time <- Sys.time()
    time_diff <- as.numeric(difftime(
      current_time,
      last_request_time,
      units = "secs"
    ))

    if (time_diff >= 60) {
      # Reset counter every minute
      request_count <<- 0
      last_request_time <<- current_time
    }

    if (request_count >= requests_per_minute) {
      # Wait until next minute
      wait_time <- 60 - time_diff
      cat("Rate limit reached. Waiting", round(wait_time), "seconds...\n")
      Sys.sleep(wait_time + 1)
      request_count <<- 0
      last_request_time <<- Sys.time()
    }

    request_count <<- request_count + 1
  }
}

# Main scraping function
scrape_heavy_metal_magazine <- function() {
  base_url <- "https://www.heavymetalmagazinefanpage.com/hmlist.html"

  # Check robots.txt
  check_robots(base_url)

  # Create rate limiter (30 requests per minute max)
  rate_limiter <- create_rate_limiter(30)

  # Find all issue links
  issue_links <- scrape_year_full_urls(base_url)

  if (length(issue_links) == 0) {
    cat("No issue links found. You may need to manually specify full_urls.\n")
    return(tibble())
  }

  # Scrape each issue page with rate limiting
  all_articles <- tibble(
    magazine = character(),
    year = integer(),
    month = character(),
    volume = integer(),
    issue = integer(),
    location = character(),
    title = character(),
    author = character(),
    note = character(),
    start_page = integer(),
    page_count = integer()
  )
  last_request_time <- Sys.time() - 60
  for (i in seq_along(issue_links)) {
    full_url <- issue_links[i]

    # Apply rate limiting
    rate_limiter()

    # Extract magazine info from full_url
    base_info <- get_annual_tocs(full_url)
    all_articles <- bind_rows(all_articles, base_info)

    # Progress indicator
    if (i %% 10 == 0) {
      cat("Processed", i, "of", length(issue_links), "pages\n")
    }
  }

  return(all_articles)
}
all_articles <- scrape_heavy_metal_magazine()
# save results to csv
write_csv(all_articles, "heavy_metal_mag/heavy_metal_magazine_articles.csv")

# change missing magazine names to "Metal Hurlant"
all_articles <- all_articles |> mutate(magazine = ifelse(magazine =="", "Metal Hurlant", magazine))
# make month column a factor in chronological order


all_articles <- all_articles |> 
  mutate(month = factor(month, levels = c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December",
    "Winter", "Spring", "Summer", "Fall", "Autumn"
  )))
# make magazine a factor in order of appearance
all_articles <- all_articles |> 
  mutate(magazine = factor(magazine, levels = c("Heavy Metal", "Epic Illustrated", "Metal Hurlant")))

arranged_articles <- all_articles |> 
  arrange(magazine,year, volume, issue)

