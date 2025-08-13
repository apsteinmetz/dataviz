library(tidyverse)
library(shiny)
library(DT)

toc_data <- read_csv("heavy_metal_mag/heavy_metal_mag_toc.csv", show_col_types = FALSE)

ui <- fluidPage(
  titlePanel("TOC Data Filter"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("year_filter"),
      uiOutput("month_filter"),
      uiOutput("volume_filter"),
      uiOutput("issue_filter"),
      uiOutput("author_filter"),
      uiOutput("title_filter")
    ),
    mainPanel(
      verbatimTextOutput("selected_article"),
      DT::dataTableOutput("filtered_table")
    )
  )
)

server <- function(input, output, session) {
  
  output$year_filter <- renderUI({
    selectInput("year_filter", "Year:", 
                choices = c("All", sort(unique(data$year))),
                selected = "All")
  })
  
  output$month_filter <- renderUI({
    selectInput("month_filter", "Month:", 
                choices = c("All", sort(unique(data$month))), 
                selected = "All")
  })
  
  output$volume_filter <- renderUI({
    data <- toc_data
    if(!is.null(input$year_filter) && input$year_filter != "All") {
      data <- data %>% filter(year == input$year_filter)
    }
    if(!is.null(input$month_filter) && input$month_filter != "All") {
      data <- data %>% filter(month == input$month_filter)
    }
    if(!is.null(input$volume_filter) && input$volume_filter != "All") {
      data <- data %>% filter(volume == input$volume_filter)
    }
    if(!is.null(input$issue_filter) && input$issue_filter != "All") {
      data <- data %>% filter(issue == input$issue_filter)
    }
    if(!is.null(input$author_filter) && input$author_filter != "All") {
      data <- data %>% filter(author == input$author_filter)
    }
    selectInput("volume_filter", "Volume:", 
                choices = c("All", sort(unique(data$volume))), 
                selected = "All")
  })
  
  output$issue_filter <- renderUI({
    data <- toc_data
    if(!is.null(input$year_filter) && input$year_filter != "All") {
      data <- data %>% filter(year == input$year_filter)
    }
    if(!is.null(input$month_filter) && input$month_filter != "All") {
      data <- data %>% filter(month == input$month_filter)
    }
    if(!is.null(input$volume_filter) && input$volume_filter != "All") {
      data <- data %>% filter(volume == input$volume_filter)
    }
    if(!is.null(input$issue_filter) && input$issue_filter != "All") {
      data <- data %>% filter(issue == input$issue_filter)
    }
    if(!is.null(input$author_filter) && input$author_filter != "All") {
      data <- data %>% filter(author == input$author_filter)
    }
    selectInput("issue_filter", "Issue:", 
                choices = c("All", sort(unique(data$issue))), 
                selected = "All")
  })
  
  output$author_filter <- renderUI({
    data <- toc_data
    if(!is.null(input$year_filter) && input$year_filter != "All") {
      data <- data %>% filter(year == input$year_filter)
    }
    if(!is.null(input$month_filter) && input$month_filter != "All") {
      data <- data %>% filter(month == input$month_filter)
    }
    if(!is.null(input$volume_filter) && input$volume_filter != "All") {
      data <- data %>% filter(volume == input$volume_filter)
    }
    if(!is.null(input$issue_filter) && input$issue_filter != "All") {
      data <- data %>% filter(issue == input$issue_filter)
    }
    if(!is.null(input$author_filter) && input$author_filter != "All") {
      data <- data %>% filter(author == input$author_filter)
    }
    selectizeInput("author_filter", "Author:", 
                   choices = c("All", sort(unique(data$author[!is.na(data$author)]))),
                   selected = "All",
                   options = list(placeholder = "Select author"))
  })
  
  output$title_filter <- renderUI({
    data <- toc_data
    if(!is.null(input$year_filter) && input$year_filter != "All") {
      data <- data %>% filter(year == input$year_filter)
    }
    if(!is.null(input$month_filter) && input$month_filter != "All") {
      data <- data %>% filter(month == input$month_filter)
    }
    if(!is.null(input$volume_filter) && input$volume_filter != "All") {
      data <- data %>% filter(volume == input$volume_filter)
    }
    if(!is.null(input$issue_filter) && input$issue_filter != "All") {
      data <- data %>% filter(issue == input$issue_filter)
    }
    if(!is.null(input$author_filter) && input$author_filter != "All") {
      data <- data %>% filter(author == input$author_filter)
    }
    selectizeInput("title_filter", "Title:", 
                   choices = c("All", sort(unique(data$title[!is.na(data$title)]))),
                   selected = "All",
                   options = list(placeholder = "Select title"))
  })
  
  filtered_data <- reactive({
    data <- toc_data
    if(!is.null(input$year_filter) && input$year_filter != "All") data <- data %>% filter(year == input$year_filter)
    if(!is.null(input$month_filter) && input$month_filter != "All") data <- data %>% filter(month == input$month_filter)
    if(!is.null(input$volume_filter) && input$volume_filter != "All") data <- data %>% filter(volume == input$volume_filter)
    if(!is.null(input$issue_filter) && input$issue_filter != "All") data <- data %>% filter(issue == input$issue_filter)
    if(!is.null(input$author_filter) && input$author_filter != "All") data <- data %>% filter(author == input$author_filter)
    if(!is.null(input$title_filter) && input$title_filter != "All") data <- data %>% filter(title == input$title_filter)
    data
  })
  
  selected_row <- reactiveVal(NULL)
  
  observeEvent(input$filtered_table_rows_selected, {
    if(length(input$filtered_table_rows_selected)) {
      selected_row(filtered_data()[input$filtered_table_rows_selected[1], ])
    }
  })
  
  output$selected_article <- renderPrint({
    if(!is.null(selected_row())) {
      selected_row()
    } else {
      "No article selected"
    }
  })
  
  output$filtered_table <- DT::renderDataTable({
    filtered_data()
  }, selection = "single", options = list(scrollX = TRUE))
}

shinyApp(ui = ui, server = server)
