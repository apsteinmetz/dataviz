library(rvest)
library(dplyr)
library(tibble)
library(robotstxt)
library(stringr)
library(readr)
library(purrr)
library(httr2)

base_url <- "https://www.marksverylarge.com/issue-index/"
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
scrape_issue_content_urls <- function(base_url) {
  page <- safe_read_html(base_url)
  if (is.null(page)) {
    return(tibble())
  }

  # extract all links
  links <- page |>
    html_nodes("a") %>%
    html_attr("href")
  
  links <- links[str_detect(links, "issue(s)*-index/1")]
  
  return(links)
}

get_article_info <- function(page) {
  nodes <- page |>
    html_nodes("p")

  # loop through nodes discarding until a node with the string "COVER:" is found
  start_index <- which(str_detect(html_text(nodes), "COVER:|Cover:"))
  # retain nodes from start_index to the end
  nodes <- nodes[start_index:(length(nodes) - 1)]
  # loop through the nodes. extract the bold element as title, the italic element as description, and the remainder as authors
  issue_content <- data.frame(
    Title = character(),
    Description = character(),
    Authors = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(nodes)) {
    node <- nodes[i]
    title <- node |> html_node("b") |> html_text(trim = FALSE) |> str_trim()
    body <- node |>
      html_text2() |>
      str_remove(title) |>
      str_split("\n") |>
      unlist()

    # select only elements in list not empty
    body <- body[which(body != "")] |>
      str_squish()
    authors = body[1]
    description = body[2]
    # if title contains "COVER", remove it
    title <- title |>
      str_remove(",|:")

    # if title is NA, skip this iteration
    if (is.na(title) || title == "") next

    issue_content <- rbind(
      issue_content,
      data.frame(
        Title = title,
        Description = description,
        Authors = authors,
        stringsAsFactors = FALSE
      )
    )
  }

  return(issue_content)
}

# Function to extract issue TOC information from a full URL
get_issue_toc <- function(issue_url) {
  page <- safe_read_html(issue_url)

  months_re <- "(January|February|March|April|May|June|July|August|September|October|November|December|Winter|Spring|Summer|Fall|Autumn)"
  header_text <- page %>%
    html_element("h1") |>
    html_text2() |>
    str_squish()

  month <- str_extract(header_text, months_re)
  year <- str_extract(header_text, "\\b(?:19|20)\\d{2}\\b") %>% as.integer()
  volume <- str_match(header_text, "(?i)vol(?:\\.|ume)?\\s*(\\d+)")[, 2] %>%
    as.integer()
  issue <- str_match(header_text, "(?i)(?:no\\.|number|issue)\\s*(\\d+)")[,
    2
  ] %>%
    as.integer()
  title <- if (is.na(header_text)) {
    NA_character_
  } else {
    t <- header_text %>% str_replace("^.*?/", "") %>% str_squish()
    if (identical(t, header_text)) NA_character_ else t
  }

  header <- tibble(
    magazine = "National Lampooon",
    volume = volume,
    issue = issue,
    month = month,
    year = year,
    title = title
  )

  toc_items <- get_article_info(page)
  toc_data <- cross_join(header,toc_items)
  
  return(toc_data)
}

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
scrape_natlamp <- function() {
  # Check robots.txt
  check_robots(base_url)

  # Create rate limiter (30 requests per minute max)
  rate_limiter <- create_rate_limiter(30)

  # Find all issue links
  cat("Getting TOC links\n")
  issue_toc_links <- scrape_issue_content_urls(base_url)

  if (length(issue_toc_links) == 0) {
    cat("No issue links found. You may need to manually specify full_urls.\n")
    return(tibble())
  }

  # Apply rate limiting
 #  rate_limiter()

  all_articles <- tibble()
  # Extract magazine info from full_url
  cat("Getting TOCs\n")
  for (issue in issue_toc_links){
    cat(issue,"\n")
    toc_info <- get_issue_toc(issue)
    all_articles <- bind_rows(all_articles, toc_info)
  }
  return(all_articles)
}

all_articles <- scrape_natlamp()
