library(shiny)
library(tidyverse)
library(DT)
library(googledrive)
library(slickR)
library(bslib)
library(digest)

# CHANGED: Explicit Drive auth with email and token from env var (avoid interactive auth)
drive_auth(email = "apsteinmetz@gmail.com", token = Sys.getenv("GOOGLE_API_KEY"))

# CHANGED: Added toggle to refresh Google Drive listing and cache locally for reproducibility
REFRESH <- FALSE
if(REFRESH){
  # CHANGED: List only cbz/cbr, keep name/id, and parse volume/issue via regex; then cache to CSV
  hm_issue_filenames <- drive_ls("Heavy_Metal",pattern = "\\.(cbz|cbr)$") |> 
    select(name, id) |> 
    transmute(file_name = name, drive_id = id) |> 
    mutate(
      volume = str_extract(file_name, "(?<=v)\\d+") |> as.numeric(),
      issue = str_extract(file_name, "(?<=v\\d{2,3} )\\d+") |> as.numeric()
    ) |> 
    arrange(volume, issue)
  write_csv(hm_issue_filenames, "hm_issue_filenames.csv")
} else {
  # CHANGED: Use cached listing to avoid hitting Drive every run
  hm_issue_filenames <- read_csv("hm_issue_filenames.csv")
}

# CHANGED: New helper to download, extract, cache, normalize image files for a comic archive
extract_comic <- function(filename) {
  cache_dir <- file.path(tempdir(), "comic_cache")
  shiny::withProgress(message = "Searching, downloading, and extracting", value = 0, {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    incProgress(0.1, detail = "Checking cache")
    cached_path <- file.path(cache_dir, tools::file_path_sans_ext(filename))
    if (dir.exists(cached_path)) {
      incProgress(0.9, detail = "Using cached files")
      return(cached_path)
    }
    ext <- tolower(tools::file_ext(filename))
    temp_file <- tempfile("comic_", fileext = paste0(".", ext))
    incProgress(0.2, detail = "Searching listing")
    matching_file <- hm_issue_filenames %>% filter(file_name == filename)
    if (nrow(matching_file) == 0) {
      incProgress(1, detail = "File not found")
      stop("File not found in Google Drive listing")
    }
    incProgress(0.3, detail = "Downloading from Google Drive")
    drive_download(as_id(matching_file$drive_id[1]), path = temp_file, overwrite = TRUE)
    out <- cached_path
    dir.create(out, recursive = TRUE)
    incProgress(0.6, detail = "Extracting archive")
    if (ext == "cbz") {
      unzip(temp_file, exdir = out)
    } else if (ext == "cbr") {
      archive::archive_extract(temp_file, dir = out)
    } else {
      incProgress(1, detail = "Unsupported file")
      stop("Unsupported file extension. Expected .cbz or .cbr")
    }
    incProgress(0.8, detail = "Preparing images")
    all_files <- list.files(out, recursive = TRUE, full.names = TRUE)
    image_exts <- c(".jpg", ".jpeg", ".png", ".gif", ".webp", ".bmp")
    image_files <- all_files[tolower(tools::file_ext(all_files)) %in% gsub("\\.", "", image_exts)]
    if (length(image_files) > 0) {
      sorted_images <- sort(image_files)
      for (i in seq_along(sorted_images)) {
        ext_i <- tools::file_ext(sorted_images[i])
        if (ext_i == "") ext_i <- "jpg"
        new_name <- file.path(out, sprintf("%03d.%s", i, tolower(ext_i)))
        file.rename(sorted_images[i], new_name)
      }
    }
    subdirs <- list.dirs(out, recursive = FALSE)
    if (length(subdirs) > 0) unlink(subdirs, recursive = TRUE)
    incProgress(1, detail = "Done")
    out
  })
}

# CHANGED: Read TOC and perform second Drive auth with explicit token var
toc_data <- read_csv(here::here("heavy_metal_mag/heavy_metal_mag_toc.csv"))
drive_auth(email = "apsteinmetz@gmail.com",token = Sys.getenv("GOOGLE_DRIVE_API"))

# CHANGED: Normalize NA text fields to empty strings for consistent filtering
toc_data <- toc_data %>%
  mutate(
    author = if_else(is.na(author), "", author),
    title = if_else(is.na(title), "", title),
    month = if_else(is.na(month), "", month)
  )

ui <- page_sidebar(
  theme = bs_theme(),
  sidebar = sidebar(
    selectizeInput("year", "Year", choices = NULL),
    selectizeInput("month", "Month", choices = NULL),
    selectizeInput("volume", "Volume", choices = NULL),
    selectizeInput("issue", "Issue", choices = NULL),
    textInput("author", "Author (includes partial match)"),
    textInput("title", "Title (includes partial match)"),
    actionButton("reset_filters", "Reset all filters")
  ),
  card(
    card_header("Table"),
    card_body(DTOutput("table"))
  ),
  card(
    card_header("Image Carousel"),
    card_body(slickROutput("carousel", height = "70vh"))
  )
)

server <- function(input, output, session) {
  # CHANGED: Precompute select choices with blank option for easy reset
  all_choices <- list(
    year = c("", sort(unique(toc_data$year))),
    month = c("", sort(unique(toc_data$month))),
    volume = c("", sort(unique(toc_data$volume))),
    issue = c("", sort(unique(toc_data$issue)))
  )
  
  # CHANGED: Initialize selectize inputs server-side for performance on large lists
  observe({
    updateSelectizeInput(session, "year", choices = all_choices$year, selected = "", server = TRUE)
    updateSelectizeInput(session, "month", choices = all_choices$month, selected = "", server = TRUE)
    updateSelectizeInput(session, "volume", choices = all_choices$volume, selected = "", server = TRUE)
    updateSelectizeInput(session, "issue", choices = all_choices$issue, selected = "", server = TRUE)
  })
  
  # CHANGED: Reactive filter supports exact filters and regex partial matches for author/title
  filtered <- reactive({
    dat <- toc_data
    if (!is.null(input$year) && input$year != "")   dat <- dat %>% filter(year == input$year)
    if (!is.null(input$month) && input$month != "")  dat <- dat %>% filter(month == input$month)
    if (!is.null(input$volume) && input$volume != "") dat <- dat %>% filter(volume == input$volume)
    if (!is.null(input$issue) && input$issue != "")  dat <- dat %>% filter(issue == input$issue)
    if (!is.null(input$author) && input$author != "") dat <- dat %>% filter(str_detect(author, regex(input$author, ignore_case = TRUE)))
    if (!is.null(input$title) && input$title != "")  dat <- dat %>% filter(str_detect(title, regex(input$title, ignore_case = TRUE)))
    dat
  })
  
  # CHANGED: Reset button clears all inputs to defaults
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "year",   choices = all_choices$year,   selected = "", server = TRUE)
    updateSelectizeInput(session, "month",  choices = all_choices$month,  selected = "", server = TRUE)
    updateSelectizeInput(session, "volume", choices = all_choices$volume, selected = "", server = TRUE)
    updateSelectizeInput(session, "issue",  choices = all_choices$issue,  selected = "", server = TRUE)
    updateTextInput(session, "author", value = "")
    updateTextInput(session, "title", value = "")
  })
  
  # CHANGED: Render DT with single-row selection, client-side for responsiveness
  output$table <- renderDT({
    datatable(filtered(), selection = "single", options = list(pageLength = 10), rownames = FALSE)
  }, server = FALSE)
  
  # CHANGED: Track selected row from DT
  selected_row <- reactiveVal(NULL)
  
  observeEvent(input$table_rows_selected, {
    row <- input$table_rows_selected
    if (length(row) == 1) selected_row(filtered()[row, ])
  })
  
  # CHANGED: Map selected TOC row to Drive filename via volume/issue and extract/cached path
  gdrive_match <- reactive({
    sr <- selected_row()
    if (is.null(sr)) return(NULL)
    v <- suppressWarnings(as.integer(sr$volume[1]))
    i <- suppressWarnings(as.integer(sr$issue[1]))
    if (is.na(v) || is.na(i)) return(NULL)
    dm <- hm_issue_filenames %>%
      filter(volume == v & issue == i) %>%
      slice_head(n = 1)
    if (nrow(dm) > 0) extract_comic(dm$file_name[1]) else NULL
  })
  
  # CHANGED: Build image list; respect optional start_page/page_count; ensure numeric ordering
  images_info <- reactive({
    sr <- selected_row()
    ip <- gdrive_match()
    if (is.null(sr) || is.null(ip)) return(NULL)
    all <- list.files(ip, full.names = FALSE)
    exts <- tools::file_ext(all)
    files <- all[tolower(exts) %in% c("jpg","jpeg","png","gif","webp","bmp")]
    if (length(files) == 0) return(NULL)
    ord <- order(readr::parse_number(files))
    files <- files[ord]
    n_total <- length(files)
    get_col <- function(df, candidates) {
      for (nm in candidates) if (nm %in% names(df)) return(df[[nm]][1])
      NA
    }
    s <- suppressWarnings(as.integer(get_col(sr, c("start_page","start","page_start"))))
    if (is.na(s) || s < 1) s <- 1
    n <- suppressWarnings(as.integer(get_col(sr, c("page_count","pages","n_pages"))))
    if (is.na(n) || n < 1) n <- n_total
    idx <- seq.int(s, length.out = n)
    idx <- idx[idx >= 1 & idx <= n_total]
    list(path = ip, files = files[idx])
  })
  
  # CHANGED: Dynamically register resource path for slickR to serve extracted images
  observeEvent(images_info(), {
    try(removeResourcePath("comic"), silent = TRUE)
    addResourcePath("comic", images_info()$path)
  }, ignoreNULL = TRUE)
  
  output$carousel <- renderSlickR({
    ii <- images_info()
    if (is.null(ii)) return(NULL)
    prefix <- paste0("comic_", digest(ii$path))
    try(removeResourcePath(prefix), silent = TRUE)
    addResourcePath(prefix, ii$path)
    urls <- file.path(prefix, ii$files)
    slides <- purrr::map(urls, ~ shiny::tags$figure(
      shiny::tags$img(src = .x, style = "width:100%"),
      shiny::tags$figcaption(.x)
    ))
    slickR(slides)
  })
  
}

shinyApp(ui, server)
