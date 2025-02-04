# lake erie ice
# https://www.glerl.noaa.gov/data/ice/#historical
# https://www.glerl.noaa.gov/data/ice/#currentConditions
# Daily average ice cover (%) by lake are calculated from gridded ASCII files provided by USNIC and CIS.
# Columns correspond to individual ICE years and rows correspond to days of year. Ice years are defined by the January calendar year of a given winter.  (for example, the 1975 column is populated with ice values for Nov '74, Dec '74, Jan '75, and so on.)
# NA's indicate that ice cover was NOT recorded on a given day and are therefore distinct from values of zero.

library (tidyverse)
library(gghighlight)
library(gganimate)
library(magick)
library(gifski)
library(ggridges)

setwd("weather")

# get current year conditions
ice_data_cy_raw <- read_table(
  "data/g2024_2025_ice.txt",
  col_names = TRUE,
  skip = 5,
  skip_empty_rows = TRUE
) |>
  # remove first row
  slice(-1) |>
  select(Year, Day, Erie) |>
  rename(ice_cover = Erie) |> 
  rename(year = Year)

ice_data_cy <- ice_data_cy_raw |>
  filter(Day < 366) |>
  # make all columns numeric
  mutate(across(everything(), as.numeric)) |>
  # add a column with the date
  mutate(date = as.Date(paste0("1900-", Day), format = "%Y-%j")) |>
  # add a column for month
  mutate(month = as_factor(month.abb[month(date)])) |>
  # if date_num is in December change year to previous year
  mutate(date = if_else(month == "Dec", date - years(1), date)) |>
  # since year is really season, make all years equal to max year
  mutate(year = as.integer(max(year))) |>
  mutate(season = as_factor(paste0(year - 1, "-", year))) |>
  # change NA to zero
  mutate(across(-1, ~ replace_na(., 0))) |>
  select(-Day) |> 
  # add column with mean ice cover by month
  mutate(.by = "month",mean_ice_cover = mean(ice_cover))

# read .txt file as data where the first row is the column names and spaces are the delimter
# text file is hand-edited to put the string "date" as the first column name
ice_data_raw <- read_table("data/erie ice cover.txt",col_names = TRUE) |> 
  # make all columns except first numeric
  mutate(across(-1, as.numeric)) |> 
  # change NA to zero
  mutate(across(-1, ~replace_na(.,0))) |> 
  mutate(date = as_factor(date))


ice_data <- ice_data_raw |> 
  pivot_longer(-1,names_to="year",values_to="ice_cover") |> 
  mutate(year=as.integer(year)) |>
  arrange(year) |>
  mutate(season = as_factor(paste0(year-1,"-",year))) |> 
  # filter out rows with Nov May or June in date
  filter(!grepl("Nov|May|Jun",date)) |> 
  # extract month string from date
  mutate(month = as_factor(str_extract(date,"[A-Z][a-z]+"))) |>
  # add column with ordered dates
  mutate(date =  as.Date(paste0("1900","-",date),format = "%Y-%b-%d")) |>
  # if date_num is in December change year to previous year
  mutate(date = if_else(month == "Dec", date - years(1), date)) |>
  # remove NA date_num
  filter(!is.na(date)) |>
  mutate(.by = season,mean_ice_cover=mean(ice_cover))

# add rows in ice_data_cy to ice_data
ice_data <- bind_rows(ice_data,ice_data_cy)

# add section for average season to ice data
ice_data <- ice_data |> 
  summarise(.by = c(month,date),ice_cover=mean(ice_cover)) |> 
  mutate(season = "Average") |> 
  mutate(year = 2025) |>
  mutate(.by = "season",mean_ice_cover=mean(ice_cover)) |> 
  bind_rows(ice_data)
  
# create a new data frame grouped into 5-year periods of mean ice level by day
# compute number of 5-year intervals in ice_data
n_intervals <- n_distinct(ice_data$year) %/% 5

ice_data_5yr <- ice_data |> 
  filter(season != "Average") |>
  mutate(period = cut(year,breaks = n_intervals)) |> 
  summarise(.by = c(period,date),mean_ice_cover=mean(ice_cover))

# function to create a color palette based on the number of unique values in a column
color_palette <- function(n) {
  # create a color palette with n colors
  colorRampPalette(c("lightblue", "darkblue"))(n)
}

#ice_data <- ice_data |> 
#  filter(season == "2002-2003")
  
# add a column containing a color value based on the season rank
ice_data_100 <- ice_data|> filter(ice_cover > 90)

# ------------------------------------------------------------------------------
# plot mean ice cover by year
ice_data |> 
  filter(season != "Average") |>
  summarise(.by = year,mean_ice_cover=mean(ice_cover)) |>
  ggplot(aes(x=year,y=mean_ice_cover))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +

   geom_smooth(method = "lm", se = FALSE, color = "red") +
   labs(
     title = "Mean Ice Cover on Lake Erie by Year",
     x = "Season",
     y = "Mean Ice Cover (%)"
   )

# plot mean ice cover by 5-year period
ice_data_5yr |> 
  group_by(period) |> 
  ggplot(aes(x=period,y=mean_ice_cover))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  labs(
    title = "Mean Ice Cover on Lake Erie by Year",
    x = "5-Year Period",
    y = "Mean Ice Cover"
  )

  
# ------------------------------------------------------------------------------
# plot mean ice cover by month
ice_data |> 
  summarise(.by=month,mean_ice_cover=mean(ice_cover)) |>
  ggplot(aes(x=month,y=mean_ice_cover))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
    title = "Mean Ice Cover on Lake Erie by Month",
    x = "Month",
    y = "Mean Ice Cover"
  )

# plot mean ice cover by ordinal day

# ------------------------------------------------------------------------------

ice_data_season_ridge <- function() {
  ridge_scaling = .05
  gg <- ice_data |>
    filter(season != "Average") |>
    ggplot(aes(x = date, y = year, group = year)) +
    #geom_line(linewidth = 2) +
    ggridges::geom_ridgeline(
      aes(height = ice_cover * ridge_scaling),
          color = "blue",
          fill = "lightblue") +
     ggridges::geom_ridgeline(
      data = ice_data_100,
      mapping = aes(height = ice_cover * ridge_scaling,
                    group =year),
      fill = NA,
      color = alpha("red", 1.0)) +
    labs(
      title = "Ice Cover on Lake Erie",
      x = "Day of the Season",
      y = "Season",
      subtitle = "Red Line means Greater Than 95% Covered",
      caption = "Data Source: https://www.glerl.noaa.gov/data/ice/#historical"
    ) +
    #reverse order of y-axis
    scale_y_reverse() +
    theme(legend.position = "none") + 
    # omit grid lines
    theme(panel.grid = element_blank()) +
    cowplot::theme_cowplot()
  return(gg)
}

ice_data_season_ridge()

ice_data_season_highlight <- function() {
  seasons <- c("2024-2025","Average")
  gg <- ice_data |>
    ggplot(aes(x = date, y= ice_cover, group = season, color = season)) +
    geom_line(linewidth = 2) +
    gghighlight(season %in% seasons,
                unhighlighted_params = list(linewidth = .5, color = alpha("grey", 0.9)),
                 use_group_by = FALSE) +
    labs(
      title = "Ice Cover on Lake Erie",
      x = "Day of the Season",
      y = "Ice Cover (%)",
      caption = "Data Source: https://www.glerl.noaa.gov/data/ice/#historical"
    ) +
    # scale_x_continuous(breaks = seq(min(ice_data$ordinal_day), max(ice_data$ordinal_day), by = 29), labels = month.abb[c(11:12, 1:4)]) +
    # add a manual color scale going from yellow to blue
    # scale_color_identity() +
    # reverse y axis
    # scale_y_discrete(limits = rev(levels(ice_data$year))) +
    theme(legend.position = "none") + 
    # omit grid lines
    # reduce font size of y-axis
    # theme(axis.text.y = element_text(size = 8)) +
    theme(panel.grid = element_blank()) +
    cowplot::theme_cowplot()
  return(gg)
}
ice_data_season_highlight()


# ------------------------------------------------------------------------------
ice_data_season_anim <- function(seasons = c("2017-2018","2020-2021")) {
  gg <- ice_data |>
    ggplot(aes(x = ordinal_day, y = ice_cover, color = season),linewidth = 5) +
    geom_line(linewidth = 2) +
    
    gghighlight(season %in% seasons,
                unhighlighted_params = list(linewidth = .5, color = alpha("grey", 0.9)),
                use_group_by = FALSE) +
    # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Ice Cover on Lake Erie", x = "Month", y = "Mean Ice Cover") +
    # replace x axis labels with month abbreviations
    # charcter vector of months of the year starting in November and ending in june
    # replace x axis labels with month abbreviations
    scale_x_continuous(
      breaks = seq(min(ice_data$ordinal_day), max(ice_data$ordinal_day), by = 29),
      # labels are month of year starting with "Nov" for day 0
      labels = month.abb[c(11:12, 1:4)]
      
    ) +
    # scale color gradient
    # scale_color_viridis_d() +
    # omit chart legend
    # coord_polar() +
    theme(legend.position = "none") +
    transition_states(
      seasons,
      transition_length = 2,
      state_length = 1
    ) +
    enter_fade() + 
    exit_shrink() +
    ease_aes('sine-in-out')
  return(gg)
}

plots <- levels(ice_data$season) |> map(ice_data_season_plot)

# ------------------------------------------------------------------------------
# convert plots to bitmapped images
  for (i in 1:length(plots)) {
    # pad i with leading zeros to two digits
    # print(plots[[i]])
    fname <- paste0("img/ice_",sprintf("%02d", i), ".png")
    print(fname)
    ggsave(fname, plot = plots[[i]], device = "png",width = 800, height = 600,units = "px")
  }
  # return a list with all the file paths

files <- list.files("img", full.names = TRUE)

# ------------------------------------------------------------------------------
# Function to create an animated GIF from a list of image file paths
  # Read in the images using magick
  img_list <- map(files, image_read)
  
  # Combine images into animation using magick pipe
  img_animation <- img_list %>%
    image_join() %>%
    image_animate(fps = 10, loop = 0, delay = NULL)
  
  # Save animation using magick pipe
  img_animation %>%
    image_write("ice_anim.gif", format = "gif")

# ------------------------------------------------------------------------------
