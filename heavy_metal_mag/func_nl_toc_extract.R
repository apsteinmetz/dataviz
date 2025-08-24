library(rvest)
library(dplyr)
library(stringr)

extract_issue_content <- function(nodes) {
  
  # Extract issue information from the header
  header_text <- nodes %>% 
    html_nodes("h1, .title, h2") %>% 
    html_text() %>% 
    .[str_detect(., "Vol\\.|Volume|No\\.|Number|May|April|June|July|August|September|October|November|December|January|February")] %>%
    first()
  
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
  
  issue_info  <- tibble(
    month = month,
    year = year,
    volume = volume,
    issue = issue,
    title = title
  )
  
  # Extract cover information
  cover_info <- nodes %>% 
    html_nodes("p") %>% 
    html_text() %>%
    .[str_detect(., "COVER:")]
  
  %>%
    first()
  
  # Find the content section (after "Contents (listing only)" or similar)
  content_section <- nodes %>% 
    html_nodes("*") %>%
    .[html_text(.) %>% str_detect("Contents.*listing")]
  
  if(length(content_section) == 0) {
    # Fallback to main content area
    content_section <- nodes
  } else {
    # Get the parent or following content after the Contents header
    content_section <- content_section %>% 
      html_nodes(xpath = "following-sibling::*") 
    
    if(length(content_section) == 0) {
      content_section <- nodes
    }
  }
  
  # Extract articles by examining the formatting
  articles <- tibble()
  
  # Look for bold elements (titles)
  bold_elements <- nodes %>% html_nodes("b, strong, .bold")
  
  # Process each potential article section
  for(i in seq_along(bold_elements)) {
    bold_elem <- bold_elements[i]
    title_text <- html_text(bold_elem) %>% str_trim()
    
    # Skip navigation elements and headers
    if(str_detect(title_text, "ISSUES|Previous issue|Next issue|Contents|COVER")) {
      next
    }
    
    # Skip empty titles
    if(title_text == "" || str_detect(title_text, "^\\s*$")) {
      next
    }
    
    # Get the parent element to find associated author and description
    parent_elem <- bold_elem %>% html_node(xpath = "..")
    
    if(is.null(parent_elem)) {
      parent_elem <- bold_elem
    }
    
    # Get all text nodes and elements within this section
    all_text <- html_children(parent_elem)
    parent_text <- html_text(parent_elem)
    
    # Extract author (plain text that's not bold and not italic)
    author <- NA_character_
    description <- NA_character_
    
    # Look for plain text (not in bold or italic tags)
    plain_text_nodes <- parent_elem %>% 
      html_nodes(xpath = ".//text()[not(ancestor::b) and not(ancestor::strong) and not(ancestor::i) and not(ancestor::em)]")
    
    if(length(plain_text_nodes) > 0) {
      plain_text <- html_text(plain_text_nodes) %>%
        str_trim() %>%
        .[. != "" & . != ","] %>%
        # Remove the title text if it appears
        .[!str_detect(., fixed(title_text))]
      
      if(length(plain_text) > 0) {
        # Take the first substantial plain text as author
        author <- plain_text[1]
        # Clean up common artifacts
        author <- str_remove(author, "^,\\s*") %>% 
          str_remove(",$") %>%
          str_trim()
      }
    }
    
    # Look for italic text (descriptions)
    italic_nodes <- parent_elem %>% 
      html_nodes("i, em, .italic")
    
    if(length(italic_nodes) > 0) {
      italic_text <- html_text(italic_nodes) %>%
        str_trim() %>%
        .[. != ""]
      
      if(length(italic_text) > 0) {
        description <- paste(italic_text, collapse = " ")
        # Remove quotes if present
        description <- str_remove_all(description, '^"|"$')
      }
    }
    
    # Alternative approach: look at the immediate siblings of the bold element
    if(is.na(author)) {
      next_sibling <- bold_elem %>% html_node(xpath = "following-sibling::text()[1]")
      if(!is.null(next_sibling)) {
        sibling_text <- html_text(next_sibling) %>% str_trim()
        # Check if it's not empty and not punctuation only
        if(sibling_text != "" && !str_detect(sibling_text, "^[,\\.\\s]*$")) {
          author <- str_remove(sibling_text, "^,\\s*") %>% 
            str_remove(",$") %>% 
            str_trim()
        }
      }
    }
    
    # Clean up author field
    if(!is.na(author) && (author == "" || str_detect(author, "^[,\\.\\s]*$"))) {
      author <- NA_character_
    }
    
    # Clean up description field  
    if(!is.na(description) && (description == "" || str_detect(description, "^[,\\.\\s]*$"))) {
      description <- NA_character_
    }
    
    # Add to articles tibble
    articles <- articles %>%
      bind_rows(tibble(
        title = title_text,
        author = author,
        description = description
      ))
  }
  
  # Return list with issue info and articles
  return(list(
    issue_info = list(
      title = if(is.na(issue_info)) "Unknown Issue" else issue_info,
      cover_info = if(is.na(cover_info)) NA_character_ else cover_info
    ),
    articles = articles
  ))
}

# Usage example:
# result <- extract_issue_content(nodes)
# print(result$issue_info)
# print(result$articles)