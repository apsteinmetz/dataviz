

library(shiny)
library(ggplot2)
library(dplyr)


shinyServer(function(input, output) {
  
  #------------------------------------------------
      
  output$distPlot <- renderPlot({
     
    inputYr<-min(input$yr,maxYear)
    one_year<-state_year_sum %>% 
      filter(Year==inputYr) %>% 
      ungroup() %>% 
      select(region,value)
    
    maxSights<-max(one_year$value)
  
    gg<-one_year %>% state_choropleth(num_colors = 1)
    gg <- gg + scale_fill_distiller(name="Bigfoot\nSightings", 
                                    palette="Greens",
                                    direction = 1,
                                    limits=c(0,maxSights))
    gg<-gg + ggtitle(inputYr)
    if (input$yr>maxYear){
      gg<-state_choropleth(state_sum,num_colors = 1)+ggtitle("All Sighting Years")
      gg <- gg + scale_fill_distiller(name="Bigfoot\nSightings", palette="Greens",direction = 1)
    }
    
    print(gg)
    
  })
  
  output$seriesPlot <- renderPlot({
    inputYr<-min(input$yr,maxYear)
    #timeline of all sightings highlighting particular year  
    fills<-c(rep("lightgreen",inputYr-startYear),"darkgreen",rep("lightgreen",maxYear-inputYr))
    subsetYear<-state_year_sum %>% 
      filter(Year>=startYear,Year<=maxYear) %>% 
      group_by(Year) %>% 
      summarize(Sightings=sum(value))
    p<-ggplot(data=subsetYear)
    p<-p+geom_col(aes(x=Year,y=Sightings),fill=(fills))
    
    print(p)
    
  })
})
