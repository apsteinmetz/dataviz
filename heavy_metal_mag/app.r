library(shiny)
library(tidyverse)
library(DT)
library(googledrive)
library(slickR)
library(bslib)
library(digest)

drive_auth(email = "apsteinmetz@gmail.com", token = Sys.getenv("GOOGLE_API_KEY"))

REFRESH <- FALSE
if (REFRESH) {
  hm_issue_filenames <- drive_ls("Heavy_Metal", pattern = "\\.(cbz|cbr)$") %>%
    select(name, id) %>%
    transmute(file_name = name, drive_id = id) %>%
    mutate(
      volume = str_extract(file_name, "(?<=v)\\d+") %>% as.numeric(),
      issue = str_extract(file_name, "(?<=v\\d{2,3} )\\d+") %>% as.numeric()
    ) %>%
    arrange(volume, issue)
  write_csv(hm_issue_filenames, "hm_issue_filenames.csv")
} else {
  hm_issue_filenames <- read_csv("hm_issue_filenames.csv")
}

resolve_cache_dir <- function(preferred) {
  ok <- tryCatch({
    if (!dir.exists(preferred)) dir.create(preferred, recursive = TRUE)
    probe <- file.path(preferred, paste0(".write_test_", Sys.getpid()))
    writeLines("ok", probe)
    unlink(probe)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (ok) return(preferred)
  fallback <- file.path(tempdir(), "comic_cache")
  message(
    "Cache directory '", preferred, "' is not writable; ",
    "falling back to '", fallback, "'. Cache will not persist across restarts."
  )
  dir.create(fallback, recursive = TRUE, showWarnings = FALSE)
  fallback
}

CACHE_DIR <- resolve_cache_dir(tools::R_user_dir("heavy_metal_mag", which = "cache"))

cache_size_str <- function(cache_dir = CACHE_DIR) {
  if (!dir.exists(cache_dir)) return("0 MB")
  files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE)
  total_bytes <- sum(file.size(files), na.rm = TRUE)
  paste(format(round(total_bytes / 1024^2, 1), nsmall = 1), "MB")
}

clear_cache <- function(cache_dir = CACHE_DIR) {
  if (dir.exists(cache_dir)) {
    unlink(list.files(cache_dir, full.names = TRUE), recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}

extract_comic <- function(filename) {
  cache_dir <- CACHE_DIR
  shiny::withProgress(message = "Searching, downloading, and extracting", value = 0, {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    incProgress(0.1, detail = "Checking cache")
    cached_path <- file.path(cache_dir, tools::file_path_sans_ext(filename))
    if (dir.exists(cached_path)) {
      incProgress(0.9, detail = "Using cached files")
      return(cached_path)
    }
    ext <- tolower(tools::file_ext(filename))
    archive_file <- file.path(cache_dir, filename)
    if (file.exists(archive_file)) {
      incProgress(0.3, detail = "Using cached archive")
    } else {
      incProgress(0.2, detail = "Searching listing")
      matching_file <- hm_issue_filenames %>% filter(file_name == filename)
      if (nrow(matching_file) == 0) {
        incProgress(1, detail = "File not found")
        stop("File not found in Google Drive listing")
      }
      incProgress(0.3, detail = "Downloading from Google Drive")
      drive_download(as_id(matching_file$drive_id[1]), path = archive_file, overwrite = TRUE)
    }
    out <- cached_path
    dir.create(out, recursive = TRUE)
    incProgress(0.6, detail = "Extracting archive")
    if (ext == "cbz") {
      unzip(archive_file, exdir = out)
    } else if (ext == "cbr") {
      archive::archive_extract(archive_file, dir = out)
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

toc_data <- read_csv(here::here("heavy_metal_mag/heavy_metal_mag_toc.csv"))
drive_auth(email = "apsteinmetz@gmail.com", token = Sys.getenv("GOOGLE_DRIVE_API"))

toc_data <- toc_data %>%
  mutate(
    author = if_else(is.na(author), "", author),
    title = if_else(is.na(title), "", title),
    month = if_else(is.na(month), "", month)
  )

ui <- page_sidebar(
  theme = bs_theme(),
  tags$head(
    tags$style(HTML("
      #carousel img { cursor: pointer; }
      .carousel-fullscreen {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        bottom: 0 !important;
        width: 100vw !important;
        height: 100vh !important;
        max-width: 100vw !important;
        z-index: 9999;
        background: #000;
        margin: 0 !important;
        border-radius: 0 !important;
      }
      .carousel-fullscreen .card-body {
        height: 100vh !important;
        display: flex;
        align-items: center;
      }
      .carousel-fullscreen #carousel {
        width: 100%;
        height: 100%;
      }
      .carousel-fullscreen .slick-slide img {
        max-height: 96vh;
        width: auto;
        margin: 0 auto;
        object-fit: contain;
      }
    ")),
    tags$script(HTML("
      $(document).on('click', '#carousel img', function() {
        var $card = $(this).closest('.card');
        $card.toggleClass('carousel-fullscreen');
        setTimeout(function() { $(window).trigger('resize'); }, 300);
      });
      $(document).on('keydown', function(e) {
        var $fs = $('.carousel-fullscreen');
        if ($fs.length === 0) return;
        var $slider = $fs.find('.slick-slider');
        if (e.key === 'ArrowLeft') {
          $slider.slick('slickPrev');
        } else if (e.key === 'ArrowRight') {
          $slider.slick('slickNext');
        } else if (e.key === 'Escape') {
          $fs.removeClass('carousel-fullscreen');
        }
      });
    "))
  ),
  sidebar = sidebar(
    selectizeInput("year", "Year", choices = NULL),
    selectizeInput("month", "Month", choices = NULL),
    selectizeInput("volume", "Volume", choices = NULL),
    selectizeInput("issue", "Issue", choices = NULL),
    textInput("author", "Author (includes partial match)"),
    textInput("title", "Title (includes partial match)"),
    tags$p(
      class = "text-muted",
      tags$small("When a new article is selected, if the image is not the first page of the article, adjust the offset (usually backwards) to reach the first article page.")
    ),
    sliderInput("offset", "Page offset", min = -10, max = 10, value = 0, step = 1),
    actionButton("reset_filters", "Reset all filters"),
    tags$hr(),
    tags$strong("Admin"),
    tags$p(textOutput("cache_size", inline = TRUE), "cached"),
    tags$p(tags$small(class = "text-muted", textOutput("cache_path", inline = TRUE))),
    actionButton("clear_cache", "Clear comic cache", icon = icon("trash"), class = "btn-outline-danger")
  ),
  card(
    full_screen = TRUE,
    card_header("Table"),
    card_body(DTOutput("table"))
  ),
  card(
    card_header(textOutput("carousel_title", inline = TRUE)),
    card_body(slickROutput("carousel", height = "50vh"))
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
  
  cache_refresh <- reactiveVal(0)
  
  output$cache_size <- renderText({
    cache_refresh()
    cache_size_str()
  })
  
  output$cache_path <- renderText({
    persistent <- !startsWith(normalizePath(CACHE_DIR, mustWork = FALSE), normalizePath(tempdir(), mustWork = FALSE))
    paste0(CACHE_DIR, if (persistent) " (persistent)" else " (temporary - will not survive restart)")
  })
  
  observeEvent(input$clear_cache, {
    showModal(modalDialog(
      title = "Clear comic cache?",
      paste0("This will delete all downloaded and extracted comic files (", cache_size_str(), ") from the cache. They will be re-downloaded on next use."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_cache", "Clear cache", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear_cache, {
    clear_cache()
    cache_refresh(cache_refresh() + 1)
    removeModal()
    showNotification("Comic cache cleared.", type = "message")
  })
  
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "year",   choices = all_choices$year,   selected = "", server = TRUE)
    updateSelectizeInput(session, "month",  choices = all_choices$month,  selected = "", server = TRUE)
    updateSelectizeInput(session, "volume", choices = all_choices$volume, selected = "", server = TRUE)
    updateSelectizeInput(session, "issue",  choices = all_choices$issue,  selected = "", server = TRUE)
    updateTextInput(session, "author", value = "")
    updateTextInput(session, "title", value = "")
    updateSliderInput(session, "offset", value = 0)
  })
  
  output$table <- renderDT({
    datatable(filtered(), selection = "single", options = list(pageLength = 10), rownames = FALSE)
  }, server = FALSE)
  
  selected_row <- reactiveVal(NULL)
  
  observeEvent(input$table_rows_selected, {
    row <- input$table_rows_selected
    if (length(row) == 1) selected_row(filtered()[row, ])
  })
  
  output$carousel_title <- renderText({
    sr <- selected_row()
    title <- if (!is.null(sr)) sr$title[1] else NA
    if (is.na(title) || title == "") "Image Carousel" else title
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
    if (nrow(dm) > 0) extract_comic(dm$file_name[1]) else NULL
  })
  
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
    s <- s + as.integer(input$offset %||% 0L)
    n <- suppressWarnings(as.integer(get_col(sr, c("page_count","pages","n_pages"))))
    if (is.na(n) || n < 1) n <- n_total
    idx <- seq.int(s, length.out = n)
    idx <- idx[idx >= 1 & idx <= n_total]
    list(path = ip, files = files[idx])
  })
  
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
