options(shiny.minified = FALSE) # TRUE for production, FALSE for development (makes it easier to debug)
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(bobsburgersR)

eps <- bobsburgersR::episode_data
trans <- bobsburgersR::transcript_data
guest_stars <- read_csv(
  "bobs_burgers_single_episode_cast.csv",
  show_col_types = FALSE
)

ui <- fluidPage(
  titlePanel("Bob's Burgers Episode Ratings"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Episode Details"),
      uiOutput("episode_details")
    ),

    mainPanel(
      width = 8,
      plotOutput("heatmap", click = "plot_click", height = "600px"),
      tags$p(
        tags$a(
          href = "https://github.com/poncest/bobsburgersR",
          "Data source: bobsburgersR",
          target = "_blank"
        ),
        style = "text-align: right; font-size: 0.85em; color: #666; margin-top: 5px;"
      )
    )
  )
)

server <- function(input, output, session) {
  selected_episode <- reactiveVal(NULL)

  output$heatmap <- renderPlot({
    ep <- selected_episode()

    p <- ggplot(
      eps,
      aes(x = factor(episode), y = factor(season), fill = rating)
    ) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(
        option = "C",
        name = "Rating",
        na.value = "grey50"
      ) +
      scale_y_discrete(limits = rev) +
      labs(
        title = "Click on a cell to see episode details",
        x = "Episode",
        y = "Season"
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold")
      )

    # Add black border around selected cell
    if (!is.null(ep)) {
      n_seasons <- length(unique(eps$season))
      y_pos <- n_seasons - ep$season + 1
      p <- p +
        annotate(
          "rect",
          xmin = ep$episode - 0.5,
          xmax = ep$episode + 0.5,
          ymin = y_pos - 0.5,
          ymax = y_pos + 0.5,
          fill = NA,
          color = "black",
          linewidth = 2
        )
    }

    p
  })

  observeEvent(input$plot_click, {
    click <- input$plot_click

    episode_num <- round(click$x)
    n_seasons <- length(unique(eps$season))
    season_num <- n_seasons - round(click$y) + 1

    ep_data <- eps |>
      filter(season == season_num, episode == episode_num)

    if (nrow(ep_data) == 1) {
      selected_episode(ep_data)
    } else {
      selected_episode(NULL)
    }
  })

  output$episode_details <- renderUI({
    ep <- selected_episode()

    if (is.null(ep)) {
      return(p(
        "Click on a cell in the heatmap to see episode details.",
        style = "color: #666; font-style: italic;"
      ))
    }

    tagList(
      h3(ep$title),
      hr(),
      p(strong("Season: "), ep$season, " | ", strong("Episode: "), ep$episode),
      p(strong("Aired: "), format(ep$aired_date, "%B %d, %Y")),
      p(
        strong("TMDB Rating: "),
        span(
          sprintf("%.1f", ep$rating),
          style = "font-size: 1.5em; font-weight: bold; color: #E69F00;"
        ),
        paste0(" (", ep$votes, " votes)")
      ),
      p(
        strong("US Viewers: "),
        if (!is.na(ep$us_viewers_millions)) {
          paste0(ep$us_viewers_millions, " million")
        } else {
          "N/A"
        }
      ),
      hr(),
      p(strong("Synopsis:")),
      p(ep$synopsis, style = "font-size: 0.9em;"),
      hr(),
      p(strong("Directed by: "), ep$directed_by),
      p(strong("Written by: "), ep$written_by),
      hr(),
      p(strong("Guest star who only appears in one episode:")),
      {
        guests <- guest_stars |>
          filter(Season == ep$season, Episode == ep$episode)
        if (nrow(guests) > 0) {
          tagList(
            lapply(seq_len(nrow(guests)), function(i) {
              p(
                paste0(guests$Actor[i], " as ", guests$`Character(s)`[i]),
                style = "margin-left: 10px; font-size: 0.9em;"
              )
            })
          )
        } else {
          p(
            "None",
            style = "margin-left: 10px; font-style: italic; color: #666;"
          )
        }
      },
      hr(),
      actionButton(
        "show_script",
        "View Script",
        icon = icon("scroll"),
        class = "btn-primary btn-block"
      ),
      hr(),
      tags$a(
        href = paste0(
          "https://bobs-burgers.fandom.com/wiki/",
          gsub(" ", "_", ep$title)
        ),
        target = "_blank",
        class = "btn btn-info btn-block",
        icon("external-link-alt"),
        " View on Fandom Wiki"
      )
    )
  })

  observeEvent(input$show_script, {
    ep <- selected_episode()
    req(ep)

    # Get the script for this episode
    script_lines <- trans |>
      filter(season == ep$season, episode == ep$episode) |>
      arrange(line) |>
      pull(raw_text)

    script_text <- paste(script_lines, collapse = "\n")

    showModal(modalDialog(
      title = paste0(
        "Script: ",
        ep$title,
        " (S",
        ep$season,
        "E",
        ep$episode,
        ")"
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(
        style = "max-height: 500px; overflow-y: auto; font-family: monospace; 
                 white-space: pre-wrap; background-color: #f8f9fa; padding: 15px;
                 border-radius: 5px; font-size: 0.85em;",
        if (length(script_lines) > 0) {
          script_text
        } else {
          "No script available for this episode."
        }
      )
    ))
  })
}

shinyApp(ui, server)
