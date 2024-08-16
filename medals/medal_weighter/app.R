# Olympic country  ranker on user selected weights
library(shiny)
library(bslib)
library(tidyverse)
library(gt)


medal_counts <- read_csv(here::here("medals/data/medal_counts_iso.csv")) |> 
    select(-Total, - country_name) |>
    pivot_longer(cols = -c(country_code), names_to = "Medal", values_to = "Count") |> 
    mutate(Medal = str_remove(Medal, " Medal")) |> 
    mutate(Medal = as_factor(Medal)) |> 
    mutate(country_code = as_factor(country_code))

# medal_counts

macro_data <- read_csv(here::here("medals/data/macro_data.csv")) |> 
    mutate(country_code = as_factor(country_code)) |> 
    rename(flag = flag_url)

medal_weights <- tibble(
    Medal = as_factor(c("Gold", "Silver", "Bronze")),
    Weight = c(3, 2, 1)
)

medals_data <- left_join(medal_counts, medal_weights, by = "Medal") 

# medals_data

change_weights <- function(dt=medals_data,g = 1,s = 1, b = 1){
    medal_weights <- tibble(
        Medal = c("Gold", "Silver", "Bronze"),
        Weight = c(g, s, b)
    )
    return(left_join(select(dt,-Weight), medal_weights, by = "Medal"))
}

country_rollup <- function(df,
                           main_country_code,
                           other_country_codes,
                           main_country_name) {
    df |>
        mutate(country_code = str_replace(
            country_code,
            paste(other_country_codes, collapse = "|"),
            main_country_code
        )) |>
        mutate(
            country_name = if_else(
                country_code == main_country_code,
                main_country_name,
                country_name
            )
        ) |>
        summarise(
            .by = c(country_code, country_name),
            Score_Wgt = sum(Score_Wgt),
            GDP = sum(GDP),
            population = sum(population)
        ) |>
        left_join(select(macro_data, country_code, flag)) |>
        ungroup()
}


test_china <- function(df,rollup_flag){
    if(rollup_flag){
        return(country_rollup(df,"CHN",c("TWN","HKG"),"China"))
    } else {
        return(df)
    }
}

sort_countries <- function(dt,sort_by = c("medal_wgt","pop_wgt","gdp_wgt","all_wgt")){
        # swtich based on sort_by
        dt <- case_when(
            sort_by == "medal_wgt" ~ arrange(dt, desc(Score_Wgt)),
            sort_by == "pop_wgt" ~ arrange(dt,desc(Score_per_MM_pop)),
            sort_by == "gdp_wgt" ~ arrange(dt,desc(Score_per_GDP_USD_BN)),
            sort_by == "all_wgt" ~ arrange(dt,desc(Score_per_capita_GDP))
        )
    return(dt)
}

ui <- page_sidebar(
    # Application title
    title = "My Country is The Best!",
    # Sidebar with a slider input for number of bins
    sidebar = sidebar(
        sliderInput(
            "gw",
            "Gold Weight:",
            min = 0,
            max = 10,
            value = 3
        ),
        sliderInput(
            "sw",
            "Silver Weight:",
            min = 0,
            max = 10,
            value = 2
        ),
        sliderInput(
            "bw",
            "Bronze Weight:",
            min = 0,
            max = 10,
            value = 1
        ),
        # add radio button
        radioButtons("sorting", "Sort On:", choices = c("medal_wgt", "pop_wgt", "gdp_wgt","all_wgt")),
        # add checkbox
        checkboxInput("big_china", "Rollup HK and Taiwan into China?", value = FALSE)
    ),
    
    card(gt_output("table"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    score_countries <- reactive({
            medals_data |> 
            change_weights(g = input$gw, s = input$sw, b = input$bw) |> 
            mutate(Score_Wgt = Count * Weight) |>
            summarize(.by = c(country_code), across(starts_with("Score"),\(x) sum(x, na.rm = TRUE))) |> 
            left_join(macro_data, by = "country_code") |> 
            test_china(input$big_china) |>
            select(-country_code) |> 
            na.omit() |>
            # score per million people
            mutate(Score_per_MM_pop = (Score_Wgt/population)) |>
            # score per $billion GDP
            mutate(Score_per_GDP_USD_BN = (Score_Wgt/GDP)) |>
            mutate(Score_per_capita_GDP = (Score_Wgt/GDP/population)) |> 
            select(country_name,starts_with("Score"),everything()) |>
            sort_countries(sort_by = input$sorting)
    })
    col_to_color <- reactive({
        switch(input$sorting,
            "medal_wgt" = "Score_Wgt",
            "pop_wgt" = "Score_per_MM_pop",
            "gdp_wgt" = "Score_per_GDP_USD_BN",
            "all_wgt" = "Score_per_capita_GDP"
        )
    })
    
    output$table <- render_gt(
        score_countries() |>
            gt() |>
            opt_interactive(use_pagination = FALSE,use_resizers = TRUE) |> 
            fmt_number(columns = c(Score_Wgt,GDP,population), decimals = 0) |>
            fmt_number(
                columns = c(Score_per_MM_pop, Score_per_GDP_USD_BN, Score_per_capita_GDP),
                decimals = 2
            ) |>
            data_color(
                columns = c(col_to_color()),
                method = "numeric",
                palette = "viridis") |> 
            tab_header(title = "Medal Scoreboard") |> 
            text_transform(
                locations = cells_body(columns = flag),
                fn = function(x) { web_image(url = x, height = 30) }
            ) |>
            cols_label(
                flag = "Flag",
                Score_Wgt = "Medal Score",
                Score_per_MM_pop = "Pop Score",
                Score_per_GDP_USD_BN = "GDP Score",
                Score_per_capita_GDP = "Per Capita GDP Score",
                country_name = "Country",
                population = "Pop.(MM)",
                GDP = "GDP ($ BN)")

    )
}

shinyApp(ui = ui, server = server)
