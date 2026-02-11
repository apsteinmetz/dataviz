options(shiny.minified = FALSE) # TRUE for production, FALSE for development (makes it easier to debug)
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(stringr)

# Load episode data from combined CSV
eps <- read_csv("www/burgers.csv", show_col_types = FALSE)

# Load transcript data from RData file
load("www/transcript_data.RData")
trans <- transcript_data

# Load most unique n-grams data
ngrams <- read_csv("www/most_unique_words.csv", show_col_types = FALSE)


ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      @font-face {
        font-family: 'BobsBurgers';
        src: url('bobs_burgers.ttf') format('truetype');
      }
      .episode-title {
        font-family: 'BobsBurgers', sans-serif;
        font-size: 3em;
      }
    "
    ))
  ),
  tags$div(
    style = "text-align: center; margin-bottom: 20px;",
    tags$img(
      src = "storefronts.png",
      style = "max-width: 100%; height: auto;"
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      uiOutput("episode_details")
    ),

    mainPanel(
      width = 8,
      tags$fieldset(
        style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px; margin-bottom: 10px; background-color: #f5f5f5;",
        tags$legend(
          "Choose Rating",
          style = "font-weight: bold; padding: 0 5px; background-color: #f5f5f5;"
        ),
        fluidRow(
          column(
            6,
            radioButtons(
              "rating_source",
              label = NULL,
              choices = c(
                "IMDB Rating" = "imdb_rating",
                "TMDB Rating" = "tmdb_rating",
                "Word Count" = "word_count"
              ),
              selected = "imdb_rating",
              inline = TRUE
            )
          ),
          column(
            6,
            conditionalPanel(
              condition = "input.rating_source == 'word_count'",
              textInput(
                "search_word",
                label = "Word to count:",
                value = "Hamburger"
              )
            )
          )
        )
      ),
      checkboxInput(
        "filter_holiday",
        label = "Show Holiday Episodes Only",
        value = FALSE
      ),
      plotOutput("heatmap", click = "plot_click", height = "600px"),
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
        "Inspired by the ",
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
        ".",
        tags$br(),
        "Source code on ",
        tags$a(
          href = "https://github.com/apsteinmetz/dataviz/tree/master/bobs_burger",
          "GitHub",
          target = "_blank"
        ),
        "."
      )
    )
  )
)

server <- function(input, output, session) {
  selected_episode <- reactiveVal(NULL)

  # Episode data already includes IMDB ratings from burgers.csv
  eps_with_imdb <- eps

  # Reactive to compute word counts from transcripts
  word_counts <- reactive({
    # Only compute if we're in word_count mode
    req(input$rating_source == "word_count")

    search_word <- input$search_word
    if (is.null(search_word) || trimws(search_word) == "") {
      search_word <- "fart"
    }
    search_word <- tolower(trimws(search_word))

    trans |>
      mutate(
        count = stringr::str_count(
          tolower(raw_text),
          stringr::regex(paste0("\\b", search_word, "\\b"))
        )
      ) |>
      summarise(word_count = sum(count, na.rm = TRUE), .by = c(season, episode))
  })

  output$heatmap <- renderPlot({
    ep <- selected_episode()
    rating_col <- input$rating_source

    rating_label <- if (rating_col == "imdb_rating") {
      "IMDB Rating"
    } else if (rating_col == "tmdb_rating") {
      "TMDB Rating"
    } else {
      paste0("'", input$search_word, "' Count")
    }

    # Create plot data with holiday filtering
    if (rating_col == "word_count") {
      plot_data <- eps_with_imdb |>
        left_join(word_counts(), by = c("season", "episode")) |>
        mutate(
          display_rating = if (input$filter_holiday) {
            ifelse(is_holiday, word_count, NA_real_)
          } else {
            word_count
          }
        )
    } else {
      plot_data <- eps_with_imdb |>
        mutate(
          display_rating = if (input$filter_holiday) {
            ifelse(is_holiday, .data[[rating_col]], NA_real_)
          } else {
            .data[[rating_col]]
          }
        )
    }

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
        title = "Click on a tile to see episode details",
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
        "Click on a tile in the heatmap to see episode details.",
        style = "color: #000000ff; font-style: italic; font-size: 2em;"
      ))
    }

    tagList(
      # Thumbnail image at top
      if (!is.na(ep$thumbnail_url)) {
        tags$img(
          src = ep$thumbnail_url,
          style = "width: 100%; max-width: 400px; border-radius: 8px; margin-bottom: 15px;"
        )
      },
      h3(ep$title, class = "episode-title"),
      # Holiday Episode label and Rating Rank badge
      {
        # Determine rating percentile based on selected rating source
        rating_col <- input$rating_source

        # Skip rating badge for word_count since it's not a real rating
        if (rating_col == "word_count") {
          rating_badge <- NULL
        } else {
          current_rating <- ep[[rating_col]]
          all_ratings <- eps[[rating_col]]
          all_ratings <- all_ratings[!is.na(all_ratings)]

          # Calculate percentile
          percentile <- sum(all_ratings <= current_rating) / length(all_ratings)

          # Viridis "C" (plasma) colors: low = #0D0887 (dark purple), high = #F0F921 (bright yellow)
          rating_badge <- if (!is.na(current_rating) && percentile >= 0.90) {
            tags$span(
              "Top 10% Rated",
              style = "background-color: #F0F921; color: black; padding: 4px 10px; border-radius: 4px; font-size: 0.9em; font-weight: bold; margin-left: 8px;"
            )
          } else if (!is.na(current_rating) && percentile <= 0.10) {
            tags$span(
              "Bottom 10% Rated",
              style = "background-color: #0D0887; color: white; padding: 4px 10px; border-radius: 4px; font-size: 0.9em; font-weight: bold; margin-left: 8px;"
            )
          } else {
            NULL
          }
        }

        tagList(
          if (!is.na(ep$is_holiday) && ep$is_holiday) {
            tags$span(
              "Holiday Episode",
              style = "background-color: #007bff; color: white; padding: 4px 10px; border-radius: 4px; font-size: 0.9em; font-weight: bold;"
            )
          },
          rating_badge
        )
      },
      p(strong("Season: "), ep$season, " | ", strong("Episode: "), ep$episode),
      p(strong("Aired: "), format(as.Date(ep$aired_date), "%B %d, %Y")),
      # IMDB Rating
      if (!is.na(ep$imdb_rating)) {
        p(
          strong("IMDB Rating: "),
          span(
            sprintf("%.1f", ep$imdb_rating),
            style = "font-size: 1.5em; font-weight: bold; color: #F5C518;"
          ),
          paste0(" (", scales::comma(ep$imdb_vote_count), " votes)")
        )
      },
      p(
        strong("TMDB Rating: "),
        span(
          sprintf("%.1f", ep$tmdb_rating),
          style = "font-size: 1.5em; font-weight: bold; color: #E69F00;"
        ),
        paste0(" (", ep$tmdb_vote_count, " votes)")
      ),
      p(
        strong("US Viewers: "),
        if (!is.na(ep$us_viewers_millions)) {
          paste0(ep$us_viewers_millions, " million")
        } else {
          "N/A"
        }
      ),
      p(strong("Synopsis:")),
      p(ep$synopsis, style = "font-size: 0.9em;"),
      # Most unique word in episode
      {
        unique_data <- ngrams |>
          filter(season == ep$season, episode == ep$episode, n_gram == 1)
        if (nrow(unique_data) > 0) {
          p(
            strong("Most Unique Word in Episode: "),
            span(
              unique_data$ngram,
              style = "font-size: 1.2em; font-weight: bold; color: #2E86AB;"
            ),
            paste0(" (", unique_data$occurrences, " times)")
          )
        }
      },
      # Most unique bigram in episode
      {
        unique_bigram <- ngrams |>
          filter(season == ep$season, episode == ep$episode, n_gram == 2)
        if (nrow(unique_bigram) > 0) {
          p(
            strong("Most Unique Phrase in Episode: "),
            span(
              unique_bigram$ngram,
              style = "font-size: 1.2em; font-weight: bold; color: #2E86AB;"
            ),
            paste0(" (", unique_bigram$occurrences, " times)")
          )
        }
      },
      # Word count display (only when word_count mode is selected)
      if (input$rating_source == "word_count") {
        word_count_data <- word_counts() |>
          filter(season == ep$season, episode == ep$episode)
        count_val <- if (nrow(word_count_data) > 0) {
          word_count_data$word_count
        } else {
          0
        }
        p(
          span(
            paste0(
              "'",
              input$search_word,
              "' is uttered ",
              count_val,
              " time(s)"
            ),
            style = "font-size: 1.2em; font-weight: bold; color: #9B59B6;"
          )
        )
      },
      hr(),
      p(strong("Directed by: "), ep$directed_by),
      p(strong("Written by: "), ep$written_by),
      {
        # Guest star data is in eps as semicolon-separated strings
        if (!is.na(ep$guest_actors) && nchar(ep$guest_actors) > 0) {
          actors <- strsplit(ep$guest_actors, "; ")[[1]]
          characters <- strsplit(ep$guest_characters, "; ")[[1]]
          tagList(
            p(strong(
              "Super special guest star because they only appear in one episode:"
            )),
            lapply(seq_along(actors), function(i) {
              p(
                paste0(actors[i], " as ", characters[i]),
                style = "margin-left: 10px; font-size: 0.9em;"
              )
            })
          )
        }
      },
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
      if (!is.na(ep$imdb_episode_url)) {
        tags$a(
          href = ep$imdb_episode_url,
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
