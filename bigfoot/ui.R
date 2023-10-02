
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Annual Bigfoot Sightings"),

  # Sidebar with a slider input for number of bins
  fluidRow(
    column(6,
      sliderInput("yr",
                  "Year of Sighting:",
                  min = startYear,
                  max = 2018,
                  value = startYear,
                  sep="",
                  animate=TRUE)
    ),
    column(6,
           plotOutput("seriesPlot"))
  ),
  fluidRow(
    column(12,
           plotOutput("distPlot")
    )
  ),
  
  fluidRow(
    column(12,
           "source: www.bfro.net"
    )
  )
  
  # Show a plot of the generated distribution
))
