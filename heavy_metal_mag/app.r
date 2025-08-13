library(shiny)
library(tidyverse)
library(DT)

toc_data <- read_csv("heavy_metal_mag_toc.csv", show_col_types = FALSE)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Year", choices = NULL),
      selectInput("month", "Month", choices = NULL),
      selectInput("volume", "Volume", choices = NULL),
      selectInput("issue", "Issue", choices = NULL),
      selectInput("author", "Author", choices = NULL),
      selectInput("title", "Title", choices = NULL)
    ),
    mainPanel(
      verbatimTextOutput("selected"),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    dat <- toc_data
    if (input$year != "")   dat <- dat %>% filter(year == input$year)
    if (input$month != "")  dat <- dat %>% filter(month == input$month)
    if (input$volume != "") dat <- dat %>% filter(volume == input$volume)
    if (input$issue != "")  dat <- dat %>% filter(issue == input$issue)
    if (input$author != "") dat <- dat %>% filter(author == input$author)
    if (input$title != "")  dat <- dat %>% filter(title == input$title)
    dat
  })
  
  observe({
    dat <- filtered()
    updateSelectInput(session, "year", choices = c("", sort(unique(dat$year))), selected = input$year)
    updateSelectInput(session, "month", choices = c("", sort(unique(dat$month))), selected = input$month)
    updateSelectInput(session, "volume", choices = c("", sort(unique(dat$volume))), selected = input$volume)
    updateSelectInput(session, "issue", choices = c("", sort(unique(dat$issue))), selected = input$issue)
    updateSelectInput(session, "author", choices = c("", sort(unique(dat$author))), selected = input$author)
    updateSelectInput(session, "title", choices = c("", sort(unique(dat$title))), selected = input$title)
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
  
  output$selected <- renderPrint({
    if (!is.null(selected_row())) selected_row() else "Double-click a row to select an article."
  })
}

shinyApp(ui, server)
