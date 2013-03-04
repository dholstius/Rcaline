library(shiny)
library(beacon)

shinyUI(pageWithSidebar(

  headerPanel("CALINE demonstration"),

  sidebarPanel(
    h3("Meteorology"),
    sliderInput("windBearing", "Wind bearing (degrees):", 
                min = 0, max = 359, value = 45, step = 15),
    sliderInput("windSpeed", "Wind speed (m/s):", 
                min = 1, max = 7, value = 2, step = 0.1),
    sliderInput("stabilityClass", "Stability class:", 
                min = 1, max = 6, value = 6, step = 1),
    sliderInput("mixingHeight", "Mixing height (m):", 
                min = 0, max = 1000, value = 1000, step = 10)
  ),

  mainPanel(
    tabsetPanel(
        tabPanel(
          "Model", 
          plotOutput("linksPlot"),
          h3("Results"),
          tableOutput("resultTable")
        )
    )
  )
  
))
