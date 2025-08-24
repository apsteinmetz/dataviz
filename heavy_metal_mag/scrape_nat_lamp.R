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
    html_attr("href") |> 
    as_tibble() |> 
    filter(str_detect(value,"issue(s)*-index/1")) |> 
    rename(url = value)
    
    return(links)
}

get_article_info <- function(node){
  art_title <- node |> html_element("strong, b") |> html_text2()
  desc <- node |> html_element("em, i") |> html_text2()
  full <- node |> html_text2() |> str_squish()
  author <- full
  if (!is.na(art_title) && art_title != "") author <- str_remove(author, fixed(art_title))
  if (!is.na(desc) && desc != "") author <- str_remove(author, fixed(desc))
  author <- author |> str_replace_all("^[:\\-–—\\s]+|[:\\-–—\\s]+$", "") |> str_squish() |> na_if("")
  tibble(
    title = na_if(art_title, ""),
    author = author,
    description = na_if(desc, "")
  )
}


# Function to extract issue TOC information from a full URL
get_issue_toc <- function(full_url) {
  page <- safe_read_html(full_url)

  main_body <- html_nodes(page,"div.mainWithBighead")
  


  header_text <- main_body %>%
    html_element("div.bighead h2, .bighead h2, .entry-title, h1, h2") %>%
    {txt <- html_text2(.); if (length(txt) == 0) NA_character_ else str_squish(txt)}
  
  months_re <- "(January|February|March|April|May|June|July|August|September|October|November|December|Winter|Spring|Summer|Fall|Autumn)"
  
  month <- str_extract(header_text, months_re)
  year <- str_extract(header_text, "\\b(?:19|20)\\d{2}\\b") %>% as.integer()
  volume <- str_match(header_text, "(?i)vol(?:\\.|ume)?\\s*(\\d+)")[, 2] %>% as.integer()
  issue <- str_match(header_text, "(?i)(?:no\\.|number|issue)\\s*(\\d+)")[, 2] %>% as.integer()
  
title <- if (is.na(header_text)) {
  NA_character_
} else {
  t <- header_text %>% str_replace("^.*?/", "") %>% str_squish()
  if (identical(t, header_text)) NA_character_ else t
}

  header <- tibble(
    month = month,
    year = year,
    volume = volume,
    issue = issue,
    title = title
  )
  
  toc_items <-  main_body |> 
    html_nodes("p") |> 
    map(\(node) get_article_info(node))
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
  issue_toc_links <- scrape_issue_content_urls(base_url)

  if (length(issue_toc_links) == 0) {
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
  for (i in seq_along(issue_toc_links)) {
    full_url <- issue_toc_links$url[i]

    # Apply rate limiting
    rate_limiter()

    # Extract magazine info from full_url
    toc_info <- get_issue_toc(full_url$value[i])
    all_articles <- bind_rows(all_articles, toc_info)

    # Progress indicator
    if (i %% 10 == 0) {
      cat("Processed", i, "of", length(issue_links), "pages\n")
    }
  }

  return(all_articles)
}

# ==============================================================================
all_articles <- scrape_natlamp_magazine()

# change missing magazine names to "Metal Hurlant"
all_articles <- all_articles |> mutate(magazine = ifelse(magazine =="", "Metal Hurlant", magazine))
# make month column a factor in chronological order
# save results to csv
write_csv(all_articles, "natlamp_mag/natlamp_mag_toc.csv")


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


hm_articles <- arranged_articles |> 
  filter(magazine == "Heavy Metal")

# who is the most frequently appearing author?
top_authors <- hm_articles |> 
  filter(!is.na(author) & author != "") |> 
  group_by(author) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

# who are the top authors by page count
top_authors_page_count <- hm_articles |> 
  filter(!is.na(author) & author != "") |> 
  group_by(author) |> 
  summarise(total_pages = sum(page_count, na.rm = TRUE)) |> 
  arrange(desc(total_pages))

# bar chart of the top 20 authors
top_authors |> 
  slice_head(n = 20) |> 
  ggplot(aes(x = reorder(author, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Authors in Heavy Metal Magazine",
       x = "Author",
       y = "Number of Articles") +
  theme_minimal()

top_authors_page_count |> 
  slice_head(n = 20) |> 
  ggplot(aes(x = reorder(author, total_pages), y = total_pages)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Authors in Heavy Metal Magazine",
       x = "Author",
       y = "Number of Articles") +
  theme_minimal()

# top author in each year by appearance count
top_authors_by_year <- hm_articles |> 
  filter(!is.na(author) & author != "") |> 
  group_by(year, author) |> 
  summarise(count = n(), total_pages = sum(page_count, na.rm = TRUE), .groups = 'drop') |> 
  arrange(year, desc(count)) |> 
  group_by(year) |> 
  slice(1)
# plot top author by year
top_authors_by_year |> 
  ggplot(aes(x = year, y = count, label = author)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(vjust = -0.5) +
  labs(title = "Top Author by Year in Heavy Metal Magazine",
       x = "Year",
       y = "Number of Articles") +
  theme_minimal()
# plot top author by year by page count
top_authors_by_year |> 
  ggplot(aes(x = year, y = total_pages, label = author)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(vjust = -0.5) +
  labs(title = "Top Author by Year in Heavy Metal Magazine",
       x = "Year",
       y = "Total Pages") +
  theme_minimal()
