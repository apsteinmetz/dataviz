library(shiny)
library(DT)
library(dplyr)

ui <- fluidPage(
  titlePanel("Heavy Metal Magazine Reader"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("issue_filter"),
      uiOutput("year_filter"),
      uiOutput("title_filter"),
      uiOutput("author_filter"),
      br(),
      DTOutput("filtered_table")
    ),
    
    mainPanel(
      uiOutput("article_images")
    )
  )
)

server <- function(input, output, session) {
  toc_data <- reactive({
    read.csv("heavy metal mag_toc.csv", stringsAsFactors = FALSE)
  })
  
  output$issue_filter <- renderUI({
    selectInput("issue", "Issue:", 
                choices = c("All", sort(unique(toc_data()$issue))))
  })
  
  output$year_filter <- renderUI({
    selectInput("year", "Year:", 
                choices = c("All", sort(unique(toc_data()$year))))
  })
  
  output$title_filter <- renderUI({
    selectInput("title", "Title:", 
                choices = c("All", sort(unique(toc_data()$title))))
  })
  
  output$author_filter <- renderUI({
    selectInput("author", "Author:", 
                choices = c("All", sort(unique(toc_data()$author))))
  })
  
  filtered_data <- reactive({
    req(input$issue, input$year, input$title, input$author)
    
    data <- toc_data()
    
    if(input$issue != "All") {
      data <- data %>% filter(issue == input$issue)
    }
    if(input$year != "All") {
      data <- data %>% filter(year == input$year)
    }
    if(input$title != "All") {
      data <- data %>% filter(title == input$title)
    }
    if(input$author != "All") {
      data <- data %>% filter(author == input$author)
    }
    
    data
  })
  
  output$filtered_table <- renderDT({
    datatable(filtered_data(), 
              selection = "single",
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$article_images <- renderUI({
    req(input$filtered_table_rows_selected)
    
    selected_row <- filtered_data()[input$filtered_table_rows_selected, ]
    article_folder <- paste0(selected_row$issue, "_", selected_row$title)
    
    jpg_files <- list.files(pattern = paste0("^", article_folder, ".*\\.jpg$"), 
                            ignore.case = TRUE)
    
    if(length(jpg_files) > 0) {
      img_tags <- lapply(jpg_files, function(file) {
        tags$img(src = file, style = "width: 100%; margin-bottom: 10px;")
      })
      do.call(tagList, img_tags)
    } else {
      h3("No images found for this article")
    }
  })
}

shinyApp(ui = ui, server = server)

