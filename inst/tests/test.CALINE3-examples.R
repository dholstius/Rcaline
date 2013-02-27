require(Rcaline)
require(testthat)
context("CALINE3-examples")

require(Rcaline)  # must install first with --no-data option

.link <- function(ID, XL1, YL1, XL2, YL2) {
	coords <- matrix(c(XL1, YL1, XL2, YL2), ncol=2, byrow=TRUE)
	return(Lines(list(Line(coords)), ID = ID))
}

CO <- Pollutant('CO', molecularWeight = 28.0)

SingleLink <- Caline3Model(
	links = FreeFlowLinks(
		SpatialLinesDataFrame(
			SpatialLines(list(
				.link("LINK A", 0.00, -5000.00, 0.00, 5000.00))), 
			data = data.frame(
				VPHL = 7500,
				EFL = 30,
				WL = 30.0,
				row.names = "LINK A")),
		vehiclesPerHour = VPHL,
		emissionFactor = EFL,
		width = WL), 
	meteorology = Meteorology(data.frame(
		windSpeed = 1.00, 
		windBearing = 270.00, 
		stabilityClass = Pasquill(6), 
		mixingHeight = 1000.00)),
	receptors = Receptors(
		cbind(x = 30.0, y = 0.0), 
		z = 1.8),
	terrain = Terrain(
		surfaceRoughness = 10.0), 
	pollutant = CO)

RuralCurved <- Caline3Model(
	links = FreeFlowLinks(
		SpatialLinesDataFrame(
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
		width = WL), 
	meteorology = Meteorology(data.frame(
		windSpeed = 1.0, 
		windBearing = 45.0, 
		stabilityClass = Pasquill(6), 
		mixingHeight = 1000.0)),
	receptors = Receptors(
		cbind(x=c(400, 100, 200, 100), y=c(1700, 1500, 1300, 350)), 
		z = 1.8),
	terrain = Terrain(
		surfaceRoughness = 50.0), 
	pollutant = CO)

rounded <- function(x) round(matrix(x), digits=1)

test_that("Case 1: single link", {
	expect_that(SingleLink, is_a('Caline3Model'))
	pred <- predict(SingleLink)
	ambient <- 3.0
	expect_equivalent(
		rounded(pred) + ambient, 
		matrix(c(7.6)))
})

test_that("Case 2: rural curved alignment", {	
	expect_that(RuralCurved, is_a('Caline3Model'))
	pred <- predict(RuralCurved)
	ambient <- 3.0
	expect_equivalent(
		rounded(pred) + ambient,
		matrix(c(6.1, 10.7, 4.4, 8.4)))
})