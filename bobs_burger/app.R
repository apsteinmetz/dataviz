options(shiny.minified = FALSE) # TRUE for production, FALSE for development (makes it easier to debug)
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(bobsburgersR)

eps <- bobsburgersR::episode_data
trans <- bobsburgersR::transcript_data
guest_stars <- read_csv(
  "bobs_burgers_single_episode_cast.csv",
  show_col_types = FALSE
)
imdb_data <- read_csv(
  "imdb_bob.csv",
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
      radioButtons(
        "rating_source",
        label = NULL,
        choices = c("TMDB Rating" = "rating", "IMDB Rating" = "imdb_rating"),
        selected = "imdb_rating",
        inline = TRUE
      ),
      checkboxInput(
        "filter_holiday",
        label = "Show Holiday Episodes Only",
        value = FALSE
      ),
      plotOutput("heatmap", click = "plot_click", height = "600px"),
      tags$p(
        tags$a(
          href = "https://github.com/poncest/bobsburgersR",
          "Data source: bobsburgersR",
          target = "_blank"
        ),
        style = "text-align: right; font-size: 0.85em; color: #666; margin-top: 5px;"
      ),
      tags$p(
        style = "text-align: left; font-size: 1.7em; color: #666; margin-top: 15px;",
        "A Shiny for R app by ",
        tags$a(
          href = "https://github.com/apsteinmetz",
          "Art Steinmetz",
          target = "_blank"
        ),
        ".",
        tags$br(),
        "Derived from the ",
        tags$a(
          href = "https://github.com/poncest/bobsburgersR",
          "bobsburgersR",
          target = "_blank"
        ),
        " package by ",
        tags$a(
          href = "https://github.com/poncest",
          "Steven Ponce",
          target = "_blank"
        ),
        "."
      )
    )
  )
)

server <- function(input, output, session) {
  selected_episode <- reactiveVal(NULL)

  # Join episode data with IMDB ratings

  eps_with_imdb <- eps |>
    left_join(
      imdb_data |> select(season, episode, imdb_rating, is_holiday),
      by = c("season", "episode")
    )

  output$heatmap <- renderPlot({
    ep <- selected_episode()
    rating_col <- input$rating_source
    rating_label <- if (rating_col == "imdb_rating") {
      "IMDB Rating"
    } else {
      "TMDB Rating"
    }

    # Create plot data with holiday filtering
    plot_data <- eps_with_imdb |>
      mutate(
        display_rating = if (input$filter_holiday) {
          ifelse(is_holiday, .data[[rating_col]], NA_real_)
        } else {
          .data[[rating_col]]
        }
      )

    p <- ggplot(
      plot_data,
      aes(x = factor(episode), y = factor(season), fill = display_rating)
    ) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(
        option = "C",
        name = rating_label,
        na.value = "grey85"
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

    # Get IMDB data for this episode
    imdb_ep <- imdb_data |>
      filter(season == ep$season, episode == ep$episode)

    tagList(
      # Thumbnail image at top
      if (nrow(imdb_ep) > 0 && !is.na(imdb_ep$thumbnail_url)) {
        tags$img(
          src = imdb_ep$thumbnail_url,
          style = "width: 100%; max-width: 400px; border-radius: 8px; margin-bottom: 15px;"
        )
      },
      h3(ep$title),
      # Holiday Episode label
      if (
        nrow(imdb_ep) > 0 && !is.na(imdb_ep$is_holiday) && imdb_ep$is_holiday
      ) {
        tags$span(
          "Holiday Episode",
          style = "background-color: #007bff; color: white; padding: 4px 10px; border-radius: 4px; font-size: 0.9em; font-weight: bold;"
        )
      },
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
      # IMDB Rating
      if (nrow(imdb_ep) > 0 && !is.na(imdb_ep$imdb_rating)) {
        p(
          strong("IMDB Rating: "),
          span(
            sprintf("%.1f", imdb_ep$imdb_rating),
            style = "font-size: 1.5em; font-weight: bold; color: #F5C518;"
          ),
          paste0(" (", scales::comma(imdb_ep$imdb_vote_count), " votes)")
        )
      },
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
      p(strong(
        "Super special guest star because they only appear in one episode:"
      )),
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
        " View at Fandom Wiki"
      ),
      # IMDB Episode Link
      if (nrow(imdb_ep) > 0 && !is.na(imdb_ep$imdb_episode_url)) {
        tags$a(
          href = imdb_ep$imdb_episode_url,
          target = "_blank",
          class = "btn btn-warning btn-block",
          style = "margin-top: 10px;",
          icon("imdb"),
          " View at IMDB"
        )
      },
      # TMDB Episode Link
      tags$a(
        href = paste0(
          "https://www.themoviedb.org/tv/32726-bob-s-burgers/season/",
          ep$season,
          "/episode/",
          ep$episode,
          "?language=en-US"
        ),
        target = "_blank",
        class = "btn btn-success btn-block",
        style = "margin-top: 10px;",
        icon("film"),
        " View at TMDB"
      ),
      hr(),
      tags$p(
        style = "font-size: 0.8em; color: #666; text-align: center;",
        "Data sources: ",
        tags$a(href = "https://www.fandom.com", "Fandom", target = "_blank"),
        ", ",
        tags$a(
          href = "https://www.wikipedia.com",
          "Wikipedia",
          target = "_blank"
        ),
        ", ",
        tags$a(href = "https://www.imdb.com", "IMDB", target = "_blank"),
        ", ",
        tags$a(href = "https://www.themoviedb.org", "TMDB", target = "_blank")
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
