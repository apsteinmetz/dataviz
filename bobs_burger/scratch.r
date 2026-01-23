# require bobsburgeR package
# install.packages("devtools")
devtools::install_github("poncest/bobsburgersR")

library(bobsburgersR)
# Example usage of bobsburgeR package
data("burgers")
head(burgers)

eps <- bobsburgersR::episode_data
trans <- bobsburgersR::transcript_data


library(ggplot2)

ggplot(eps, aes(x = factor(episode), y = factor(season), fill = rating)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", name = "Rating") +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Bob's Burgers Episode Ratings",
    x = "Episode",
    y = "Season"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
