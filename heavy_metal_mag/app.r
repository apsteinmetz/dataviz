library(shiny)
library(tidyverse)
library(DT)
library(googledrive)


extract_comic <- function(filename) {
  out <- tempfile("comic_")
  dir.create(out)
  ext <- tolower(tools::file_ext(filename))
  if (ext == "cbz") {
    unzip(file, exdir = out)
    return(out)
  }
  if (ext == "cbr") {
    sevenzip <- c(Sys.which("7z"))
    sevenzip <- sevenzip[nzchar(sevenzip)][1]
    if (!nzchar(sevenzip)) stop("Install the 'archive' package or 7-Zip ('7z'/'7za'/'7zz').")
    system2(sevenzip, c("x", "-y", 
                        paste0("-o", shQuote(normalizePath(out))), 
                        shQuote(normalizePath(filename))), stdout = FALSE, stderr = FALSE)
    return(out)
  }
  stop("Unsupported file extension. Expected .cbz or .cbr")
}

toc_data <- read_csv(here::here("heavy_metal_mag/heavy_metal_mag_toc.csv"))

drive_auth(email = "apsteinmetz@gmail.com",token = Sys.getenv("GOOGLE_DRIVE_API"))

toc_data <- toc_data %>%
  mutate(
    year = as.character(year),
    volume = as.character(volume),
    issue = as.character(issue),
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
  all_choices <- reactiveVal(list(
    year = c("", sort(unique(toc_data$year))),
    month = c("", sort(unique(toc_data$month))),
    volume = c("", sort(unique(toc_data$volume))),
    issue = c("", sort(unique(toc_data$issue)))
  ))
  
  observe({
    updateSelectizeInput(session, "year", choices = all_choices()$year, selected = "", server = TRUE)
    updateSelectizeInput(session, "month", choices = all_choices()$month, selected = "", server = TRUE)
    updateSelectizeInput(session, "volume", choices = all_choices()$volume, selected = "", server = TRUE)
    updateSelectizeInput(session, "issue", choices = all_choices()$issue, selected = "", server = TRUE)
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
  
  observe({
    dat <- filtered()
    updateSelectizeInput(session, "year",   choices = c("", sort(unique(dat$year))),   selected = input$year,   server = TRUE)
    updateSelectizeInput(session, "month",  choices = c("", sort(unique(dat$month))),  selected = input$month,  server = TRUE)
    updateSelectizeInput(session, "volume", choices = c("", sort(unique(dat$volume))), selected = input$volume, server = TRUE)
    updateSelectizeInput(session, "issue",  choices = c("", sort(unique(dat$issue))),  selected = input$issue,  server = TRUE)
  })
  
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "year",   choices = all_choices()$year,   selected = "", server = TRUE)
    updateSelectizeInput(session, "month",  choices = all_choices()$month,  selected = "", server = TRUE)
    updateSelectizeInput(session, "volume", choices = all_choices()$volume, selected = "", server = TRUE)
    updateSelectizeInput(session, "issue",  choices = all_choices()$issue,  selected = "", server = TRUE)
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
  
  #output$selected <- renderPrint({
  #  if (!is.null(selected_row())) selected_row() else "Double-click a row to select an article."
  #})
  
  gdrive_match <- reactive({
    sr <- selected_row()
    if (is.null(sr)) return(NULL)
    v <- suppressWarnings(as.integer(sr$volume[1]))
    i <- suppressWarnings(as.integer(sr$issue[1]))
    if (is.na(v) || is.na(i)) return(NULL)
    q <- sprintf("name contains 'Heavy Metal v' and name contains '%d' and name contains '%d' and trashed = false", v, i)
    pat <- paste0("^Heavy Metal v0{0,2}", v, " 0{0,2}", i, ".+\\.(cbr|cbz)$")
    googledrive::drive_find(q = q, n_max = 200) %>%
      filter(str_detect(name, regex(pat, ignore_case = TRUE))) %>%
      transmute(file_name = name, drive_id = id) %>%
      slice_head(n = 1)
  })
  
  output$selected <- renderPrint({
    sr <- selected_row()
    if (is.null(sr)) {
      "Double-click a row to select an article."
    } else {
      print(sr)
      dm <- gdrive_match()
      if (!is.null(dm) && nrow(dm) > 0) {
        cat("\nGoogle Drive match:\n")
        print(dm)
      } else {
        cat("\nGoogle Drive match: none found\n")
      }
    }
  })
}

shinyApp(ui, server)
