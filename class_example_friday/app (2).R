# Title: Friday In Class 
# Author: Madeleine Maylath
# Date Created: 2021-04-16

# friday class - code for shiny app

# Purpose: practice creating a shiny app that produces visualizations of data from 
# the GSS data set.

# Set Up####
# Libraries
library(shiny)

# UI####
ui <- fluidPage(
  # Page title 
  titlePanel("POSC-3410 Example 2"),
  
  #Configure layout
  sidebarLayout(
    # Control panel 
    sidebarPanel(
      # Slider input for Year
      sliderInput( 
        "gss_years",
        "Title that people will see",
        min = min(gss_shiny$YEAR, na.rm=TRUE),
        max = max(gss_shiny$YEAR, na.rm=TRUE),
        value=c(min(gss_shiny$YEAR, na.rm=TRUE), max(gss_shiny$YEAR, na.rm=TRUE)),
        sep=""
        )
    ),
    checkboxGroupInput(
      "political_party",
      "Select the Party IDs you want to display",
      c("Strong republican","Not str republican", "Not str democrat", "Strong democrat" ),
      c("Strong republican", "Strong democrat")
    )
    
  ),
    mainPanel(
      h1("Hello World"),
      plotOutput("gss_plot"),
      p("Description text goes here.")
    )
  

)

# Server####
server<- function(input, output, session) {
  # GSS Plot 
  output$gss_plot <- renderPlot({
    data <- gss_shiny %>% 
      filter(YEAR >= input$gss_years [1] & YEAR <= input$gss_years [2]& PARTYID %in% input$political_party)
    data %>% 
      ggplot(aes(x=PARTYID, y=n, fill=NATSPAC)) +
      geom_bar(stat="identity")
  })
}

# App####
shinyApp(ui=ui, server=server)

# Copyright (c) Grant Allard, 2021
  