## ----options, echo = FALSE, message = FALSE-----------------------------------
library(knitr)
options(width = 80, digits = 3, continue = " ")
opts_chunk$set(echo = FALSE, message = FALSE, cache = TRUE)
knit_hooks$set(small.mar = function(before, options, envir) {
    if (before) par(mar = c(.1, .1, .1, .1))  # smaller margin on top and right
})

## ----library, cache = FALSE, message = FALSE----------------------------------
library(Rcaline)

## ----SF_data------------------------------------------------------------------
data(SanFrancisco, package = "Rcaline")
summary(geometry(SF_highways))
str(SF_highways@data)

## ----met_data-----------------------------------------------------------------
met_file <- system.file("extdata", "BayArea", "Meteorology", "met_5801.isc", package = "Rcaline")
met_data <- ISCFile(met_file)

## ----SF_conditions, dependson = "met_data"------------------------------------
SF_conditions <- Meteorology(met_data, use = "urban")
summary(SF_conditions)

## ----SF_links, dependson = "SF_data"------------------------------------------
SF_links <- FreeFlowLinks(
  SF_highways,
	vehiclesPerHour = TRVol2009 / 24, # convert from 24 h to 1 h scale
	emissionFactor = 1.0,             # in grams per vehicle-mile
	width = 30.0)                     # in meters

## ----receptors_from_shapefile-------------------------------------------------
## Not run:
# fn <- file.choose()
# layer_name <- sub("\\.shp$", "", fn, ignore.case = TRUE)
# features <- spTransform(readOGR(dirname(fn), layer_name), CRS(proj4string(SF_county)))
# stopifnot(inherits(features, "SpatialPoints"))
# receptors <- Receptors(features)

## ----SF_receptor_grid, dependson = "SF_links"---------------------------------
SF_receptor_grid <- ReceptorGrid(
  SF_links, 
  resolution = 250, 
  maxDistance = 1e3)

## ----SF_receptor_rings, dependson = "SF_links"--------------------------------
SF_receptor_rings <- ReceptorRings(
  SF_links, 
  distances = c(100, 250, 500, 1000))

## ----small.mar = TRUE, echo = FALSE, fig.keep = 'high', fig.scap = c("Regular grid", "Rings by distance")----
plot(SF_county, col = "light gray", border = NA)
lines(SF_links)
points(SF_receptor_grid, pch = '+', cex = 0.5)

plot(SF_county, col = "light gray", border = NA)
lines(SF_links)
points(SF_receptor_rings, pch = '+', cex = 0.5)

## ----urban_terrain------------------------------------------------------------
urban_terrain <- Terrain(surfaceRoughness = 80.0)

## ----fine_PM------------------------------------------------------------------
fine_PM <- Pollutant("PM2.5", molecularWeight = NA)
show(fine_PM)

## ----pred_matrix, cache = TRUE, dependson = c("terrain_and_pollutant", "SF_receptor_rings", "SF_conditions", "SF_links", "urban_terrain", "fine_PM")----
sample_rows <- function(x, p) {
  x[sample(1:nrow(x), p * nrow(x)),]
}

SF_model <- Caline3Model(
  SF_links, 
  sample_rows(SF_conditions, p = 0.01), 
  SF_receptor_rings, 
  urban_terrain, 
  fine_PM)

pred_matrix <- predict(SF_model, units = "mg/m^3")

## ----pred_stats, dependson = "pred_matrix", fig.width = 6---------------------
pred_stats <- aggregate(pred_matrix)
show(colnames(pred_stats))

## ----pred_spdf, dependson = "pred_stats"--------------------------------------
pred_spatial <- as(pred_stats, "SpatialPointsDataFrame")
show(pred_spatial[1:3, c("distance", "min", "mean", "median", "max")])

## ----density_by_distance, dependson = "pred_spatial", fig.width = 6, fig.cap = "Distributions of the mean predicted concentration, by distance-to-roadway."----
suppressPackageStartupMessages(library(ggplot2))
ggplot(pred_spatial@data) + geom_histogram(aes(mean)) + facet_wrap(~ distance)

## ----bubble_plot, dependson = "pred_stats", fig.width = 8, fig.height = 6-----
label_kilo <- function (x) sprintf("%0.0f", x / 1000.0)
ggplot(pred_stats) + 
  geom_point(aes(x, y, size = mean, color = mean, order = mean, alpha = mean)) +
  scale_color_gradient(expression(paste(bold(hat(Z)(s)), " ", mg/m^3)), low = "#BEBEBE00", high = "brown") + 
  scale_size(guide = "none") + scale_alpha(guide = "none") + 
  scale_x_continuous("UTM10 Easting (km)", labels = label_kilo) +
  scale_y_continuous("UTM10 Northing (km)", labels = label_kilo)

