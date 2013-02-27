is.installed <- function(pkgname) pkgname %in% .packages(TRUE)

#' real64
#'
#' Convert a numeric vector to double precision.
#'
#' @param x numeric vector
#' @return a \link{double}-precision vector
real64 <- function(x) as.real(x)

#' real32
#'
#' Convert a numeric vector to single precision.
#'
#' @param x numeric vector
#' @return a \link{single}-precision vector
real32 <- function(x) as.single(x)

kilo <- function(x) x / 1000

#' easting and northing
#'
#' Convenience functions for creating map scales.
#'
#' @param ... arguments passed to ggplot2::scale_continuous
#' @rdname ggplot-helpers
#' @export
easting <- function(...) scale_x_continuous("Easting (km)", labels=kilo, ...)
northing <- function(...) scale_y_continuous("Northing (km)", labels=kilo, ...)

#' Take a random sample of rows from a data.frame.
#'
#' @param x a data.frame
#' @param p the fraction of rows to sample (between 0 and 1)
#' @keywords sample data.frame
#' @return a subset of the original data.frame
#' @export
sample.rows <- function(x, p) x[sample(1:nrow(x), p * nrow(x)),]

suppressOutput <- function(expr) {
	if(.Platform$OS.type == 'windows') {
		sink("NUL")
	} else {
		sink("/dev/null")
	}
	result <- eval(expr)
	sink()
	return(result)
}

#' @export
read.shp <- function(filename, ...) {
	stop('read.shp() is deprecated. See help("FreeFlowLinks") for more.')
}

# read.shp <- function(filename, ...) {
# 	message("Loading shapefile: ", filename)
# 	if(is.installed("rgdal")) {
# 		suppressPackageStartupMessages(require(rgdal))
# 		dsn <- dirname(filename)
# 		layer <- gsub(".shp$", "", basename(filename))
# 		spobj <- suppressOutput(rgdal::readOGR(dsn, layer, ...)) 
# 	} else {
# 		stop("rgdal must be installed in order to read shapefiles.") 
# 	}
# 	if(is.na(proj4string(spobj))) {
# 		warning("Missing projection info for ", filename, ". Assuming planar coords.")
# 	} else {
# 		if(!is.projected(spobj)) {
# 			stop(filename, " has non-planar coordinates. See ?read.shp for more.")
# 		}
# 	}
# 	return(spobj)
# }
