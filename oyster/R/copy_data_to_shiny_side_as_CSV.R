# copy data files to shiny app www directory as csv
library(tidyverse)
datasets <- c("wq_data", "wq_meta", "weather")

copy_to_shiny <- function(name) {
  load(paste0("data/",name,".rdata"))
  dframe = eval(parse(text = name))
  fname = paste0("NY_Harbor_WQ/www/", name, ".csv")
  write_csv(dframe,fname)
}

walk(datasets, copy_to_shiny)


