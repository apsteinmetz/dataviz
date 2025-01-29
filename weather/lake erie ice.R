# lake erie ice
library (tidyverse)
library(gghighlight)
library(gganimate)
setwd("weather")

ice_data_cols <- c("mmm_dd",
                   read_lines("data/erie ice cover.txt", skip = 0, n_max = 1) |>
                     str_split(" ") |>
                     unlist()
)

ice_data_raw <- read_csv(
  "data/erie ice cover.csv",
  col_types = cols(.default = "c"),
  col_names = FALSE,
  skip = 1) |>
  # make all columns except first numeric
  mutate(across(-1, as.numeric)) |> 
  # change NA to zero
  mutate(across(-1, ~replace_na(.,0))) |> 
  set_names(ice_data_cols)

ice_data <- ice_data_raw |> 
  pivot_longer(-1,names_to="year",values_to="ice_cover") |> 
  mutate(date=as.Date(paste0(year,"-",mmm_dd),format="%Y-%b-%d")) |> 
  # remove rows with NA in date
  filter(!is.na(date)) |>
  mutate(year=as.integer(year)) |> 
  mutate(month=month(date)) |> 
  # add column for ordinal day of the year
  mutate(ordinal_day=yday(date)) |> 
  # change mmm_dd from character to factor ordered by ordinal day
  mutate(mmm_dd=factor(mmm_dd,levels=unique(mmm_dd[order(ordinal_day)]))) |> 
  # add column to for season where winter is november to april of following year
  mutate(season=as.factor(case_when(
    ordinal_day <= 183 ~ paste(year-1,"-",year,sep=""),
    ordinal_day > 183 ~ paste(year,"-",year+1,sep=""))
  )) |> 
  # offset ordinal days to make December 31 the middle of the year
  mutate(ordinal_day= (ordinal_day + 182) %% 365) |> 
  # omit summer months
  filter(month(date) %in% c(11:12,1:4))
  


# plot mean ice cover by year
ice_data |> 
  group_by(year) |> 
  summarise(mean_ice_cover=mean(ice_cover)) |>
  ggplot(aes(x=year,y=mean_ice_cover))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +

   geom_smooth(method = "lm", se = FALSE, color = "red") +
   labs(
     title = "Mean Ice Cover on Lake Erie by Year",
     x = "Year",
     y = "Mean Ice Cover"
   )

# plot mean ice cover by month
ice_data |> 
  group_by(month) |> 
  summarise(mean_ice_cover=mean(ice_cover)) |>
  ggplot(aes(x=month,y=mean_ice_cover))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(
    breaks = seq(1, 12, by = 1),
    labels = month.abb
  ) +
    labs(
    title = "Mean Ice Cover on Lake Erie by Month",
    x = "Month",
    y = "Mean Ice Cover"
  )

# plot mean ice cover by ordinal day

ice_data_season_plot <- function(seasons = c("2017-2018","2020-2021")) {
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
    theme(legend.position = "none")
  return(gg)
}

plots <- levels(ice_data$season) |> map(ice_data_season_plot)


ice_data |>
  #filter(season %in% c("2018-2019")) |>
  ggplot(aes(x = ordinal_day, y = ice_cover, color = season)) +
  geom_line() +
  gghighlight(max(mean(ice_data$ice_cover))) +
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
  theme(legend.position = "none")

