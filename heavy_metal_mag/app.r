
library(shiny)
library(tidyverse)
library(DT)
library(googledrive)

# authorize googledrive
drive_auth(email = "apsteinmetz@gmail.com", token = Sys.getenv("GOOGLE_API_KEY"))
REFRESH <- FALSE
if(REFRESH){
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
  hm_issue_filenames <- read_csv("hm_issue_filenames.csv")
}

extract_comic <- function(filename) {
  cat("Starting extraction for:", filename, "\n")
  
  cache_dir <- file.path(tempdir(), "comic_cache")
  if (!dir.exists(cache_dir)) {
    cat("Creating cache directory:", cache_dir, "\n")
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cached_path <- file.path(cache_dir, tools::file_path_sans_ext(filename))
  if (dir.exists(cached_path)) {
    cat("Found cached version at:", cached_path, "\n")
    return(cached_path)
  }
  
  ext <- tolower(tools::file_ext(filename))
  temp_file <- tempfile("comic_", fileext = paste0(".", ext))
  cat("Using temporary file:", temp_file, "\n")
  
  matching_file <- hm_issue_filenames %>% filter(file_name == filename)
  if (nrow(matching_file) == 0) stop("File not found in Google Drive listing")
  
  cat("Downloading from Google Drive ID:", matching_file$drive_id[1], "\n")
  drive_download(as_id(matching_file$drive_id[1]), path = temp_file)
  
  out <- cached_path
  cat("Creating extraction directory:", out, "\n")
  dir.create(out, recursive = TRUE)
  
  if (ext == "cbz") {
    cat("Extracting CBZ file...\n")
    unzip(temp_file, exdir = out)
  } else if (ext == "cbr") {
    cat("Extracting CBR file...\n")
    archive::archive_extract(temp_file, dir = out)
  } else {
    stop("Unsupported file extension. Expected .cbz or .cbr")
  }
  
  unlink(temp_file)
  
  # Remove non-image files and nested directories
  all_files <- list.files(out, recursive = TRUE, full.names = TRUE)
  image_exts <- c(".jpg", ".jpeg", ".png", ".gif", ".webp", ".bmp")
  image_files <- all_files[tolower(tools::file_ext(all_files)) %in% gsub("\\.", "", image_exts)]
  
  # Move images to root directory and rename sequentially
  if (length(image_files) > 0) {
    sorted_images <- sort(image_files)
    for (i in seq_along(sorted_images)) {
      new_name <- file.path(out, sprintf("%03d%s", i, tools::file_ext(sorted_images[i])))
      file.rename(sorted_images[i], new_name)
    }
  }
  
  # Clean up subdirectories
  subdirs <- list.dirs(out, recursive = FALSE)
  if (length(subdirs) > 0) unlink(subdirs, recursive = TRUE)
  
  cat("Extraction complete. Files available at:", out, "\n")
  return(out)
}

toc_data <- read_csv(here::here("heavy_metal_mag/heavy_metal_mag_toc.csv"))

drive_auth(email = "apsteinmetz@gmail.com",token = Sys.getenv("GOOGLE_DRIVE_API"))

toc_data <- toc_data %>%
  mutate(
    # year = as.character(year),
    # volume = as.character(volume),
    # issue = as.character(issue),
    author = if_else(is.na(author), "", author),
    title = if_else(is.na(title), "", title),
    month = if_else(is.na(month), "", month)
  )

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput("year", "Year", choices = NULL),
      selectizeInput("month", "Month", choices = NULL),
      selectizeInput("volume", "Volume", choices = NULL),
      selectizeInput("issue", "Issue", choices = NULL),
      textInput("author", "Author (includes partial match)"),
      textInput("title", "Title (includes partial match)"),
      actionButton("reset_filters", "Reset all filters")
    ),
    mainPanel(
      verbatimTextOutput("selected"),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  all_choices <- list(
    year = c("", sort(unique(toc_data$year))),
    month = c("", sort(unique(toc_data$month))),
    volume = c("", sort(unique(toc_data$volume))),
    issue = c("", sort(unique(toc_data$issue)))
  )
  
  observe({
    updateSelectizeInput(session, "year", choices = all_choices$year, selected = "", server = TRUE)
    updateSelectizeInput(session, "month", choices = all_choices$month, selected = "", server = TRUE)
    updateSelectizeInput(session, "volume", choices = all_choices$volume, selected = "", server = TRUE)
    updateSelectizeInput(session, "issue", choices = all_choices$issue, selected = "", server = TRUE)
  })
  
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
  
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "year",   choices = all_choices$year,   selected = "", server = TRUE)
    updateSelectizeInput(session, "month",  choices = all_choices$month,  selected = "", server = TRUE)
    updateSelectizeInput(session, "volume", choices = all_choices$volume, selected = "", server = TRUE)
    updateSelectizeInput(session, "issue",  choices = all_choices$issue,  selected = "", server = TRUE)
    updateTextInput(session, "author", value = "")
    updateTextInput(session, "title", value = "")
  })
  
  output$table <- renderDT({
    datatable(filtered(), selection = "single", options = list(pageLength = 10), rownames = FALSE)
  }, server = FALSE)
  
  selected_row <- reactiveVal(NULL)
  
  observeEvent(input$table_rows_selected, {
    row <- input$table_rows_selected
    if (length(row) == 1) {
      selected_row(filtered()[row, ])
    }
  })
  

  gdrive_match <- reactive({
    sr <- selected_row()
    if (is.null(sr)) return(NULL)
    v <- suppressWarnings(as.integer(sr$volume[1]))
    i <- suppressWarnings(as.integer(sr$issue[1]))
    if (is.na(v) || is.na(i)) return(NULL)
    
    dm <- hm_issue_filenames %>%
      filter(volume == v & issue == i) %>%
      slice_head(n = 1)
    image_path <- if (nrow(dm) > 0) {
      extract_comic(dm$file_name[1])
    } else {
      NULL
    }
    cat(image_path, "\n")
    return(image_path)
  })
  
  output$selected <- renderPrint({
    sr <- selected_row()
    if (is.null(sr)) {
      "Double-click a row to select an article."
    } else {
      print(sr)
      dm <- gdrive_match()
      print(dm)
      #if (!is.null(dm) && nrow(dm) > 0) {
      #  cat("\nGoogle Drive match:\n")
      #  print(dm)
      #} else {
      #  cat(" Google Drive match: none found\n")
      #}
    }
  })
}

shinyApp(ui, server)
