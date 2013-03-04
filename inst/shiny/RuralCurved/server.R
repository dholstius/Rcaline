library(shiny)
library(Rcaline)
library(ggplot2)
library(sp)

shinyServer(function(input, output) {
  
  .meteorology <- reactive({
    Rcaline::Meteorology(data.frame(
      windSpeed = input$windSpeed,
      windBearing = input$windBearing,
      stabilityClass = input$stabilityClass,
      mixingHeight = input$mixingHeight
    ))
  })

  .links <- reactive({
    require(sp)
    .link <- function(ID, XL1, YL1, XL2, YL2) {
    	coords <- matrix(c(XL1, YL1, XL2, YL2), ncol=2, byrow=TRUE)
    	return(Lines(list(Line(coords)), ID = ID))
    }
    Rcaline::FreeFlowLinks(SpatialLinesDataFrame(
			SpatialLines(list(
				.link('LINK A', -707, -707, 0, 0),
				.link('LINK B', 0, 0, 120, 175),
				.link('LINK C', 120, 175, 150, 350),
				.link('LINK D', 150, 350, 150, 1350),
				.link('LINK E', 150, 1350, 175, 1510),
				.link('LINK F', 175, 1510, 265, 1640),
				.link('LINK G', 265, 1640, 350, 1760),
				.link('LINK H', 350, 1760, 475, 1830),
				.link('LINK I', 475, 1830, 650, 1850),
				.link('LINK J', 650, 1850, 1650, 1850))),
			data = data.frame(
				VPHL = rep(8500, 10),
				EFL = rep(30, 10),
				WL = rep(28, 10), 
				row.names = paste('LINK', LETTERS[1:10]))),
		vehiclesPerHour = VPHL,
		emissionFactor = EFL,
		width = WL
		)
  })
  
  .receptors <- reactive({
    Rcaline::Receptors(cbind(x=c(400, 100, 200, 100), y=c(1700, 1500, 1300, 350)), z = 1.8)
  })
  
  .pollutant <- reactive({
    Rcaline::Pollutant('CO', molecularWeight = 28.0)
  })
  
  .ambient <- reactive({
    3.0
  })
  
  .terrain <- reactive({
    Rcaline::Terrain(surfaceRoughness = 10.0)
  })
  
  .model <- reactive({
    Rcaline::Caline3Model(.links(), .meteorology(), .receptors(), .terrain(), .pollutant())
  })
  
  .result <- reactive({
    predict(.model())
  })

  output$resultTable <- renderTable({
    data.frame(.receptors(), result=as.numeric(.result()))
    #format(as.matrix(.result()), digits=2)
  })
  
  output$linksPlot <- renderPlot({
    fig <- ggplot() + coord_equal()
    fig <- fig + geom_segment(aes(XL1, YL1, xend=XL2, yend=YL2, color=VPHL), data=as.data.frame(.links()))
    fig <- fig + geom_point(aes(x, y), data=as.data.frame(.receptors()))
    fig <- fig + scale_x_continuous("X-coordinate (m)")
    fig <- fig + scale_y_continuous("Y-coordinate (m)")
    print(fig)
  })

  
  output$metTable <- renderTable({
    format(.meteorology(), digits=5)
  })
   
})
