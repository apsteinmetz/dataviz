
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
library(sp)

shinyServer(function(input, output) {
  load("state_sum.RData")
  states_map <- map_data("state")

  
  getMap<-function(n) {
    gg<-gg %+% state_sum[Year==n]
    title = paste("Sighting Year",n)
    #assume any year into future is cumulative sightings for all years
    if (n > year(Sys.Date())) {
      title = "All Sighting Years"
      gg <- gg + scale_fill_distiller(name="Bigfoot\nSightings", palette="Greens", na.value="#7f7f7f")
      title<- "All Sighting Years"
    }
    gg<-gg + ggtitle(title)
    gg<-gg + coord_map()
    return (gg)
  }
  
 
  sightYear<-2012
  thisYear<-year(Sys.Date())
  maxSights=max(state_sum[Year<thisYear+1]$count)
  gg<-ggplot(state_sum, aes(map_id = state)) + geom_map(aes(fill = count), map = states_map) + expand_limits(x = states_map$long, y = states_map$lat)
  gg <- gg + theme_bw()
  gg <- gg + scale_fill_distiller(name="Bigfoot\nSightings", palette="Greens", na.value="#7f7f7f",limits=c(0,maxSights))
  gg <- gg + theme(plot.title=element_text(face="bold", hjust=0, size=24))
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_blank())
  gg<- gg + labs(x="",y="")
  gg <- gg + theme(strip.background=element_blank())
  gg <- gg+borders("state",inherit.aes=FALSE, colour = "black")
  

#  for (n in 1960:thisYear)  drawMap(n)
   #draw cumulative summary
#  gg <- gg + scale_fill_distiller(name="Bigfoot\nSightings", palette="Greens", na.value="#7f7f7f")
#  gg<- gg + ggtitle("All Sighting Years")
#  drawMap(thisYear+1)
  
  
  output$distPlot <- renderPlot({
     
    print(getMap(input$yr))
 
  })
  output$seriesPlot <- renderPlot({
    inputYr<-min(input$yr,thisYear-2)
    fills<-c(rep("grey50",inputYr-startYear),"red",rep("grey50",thisYear-inputYr-2))
    p<-ggplot(data=subsetYear)
    p<-p+geom_bar(aes(x=Year,y=number),stat="identity",fill=(fills))
    print(p)
    
  })

})
