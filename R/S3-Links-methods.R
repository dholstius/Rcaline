#' lines
#'
#' Plot a FreeFlowLinks object.
#'
#' @param x a FreeFlowLinks object
#' @param ... other arguments
#' 
#' @keywords links
#' @S3method lines FreeFlowLinks
#' @export
lines.FreeFlowLinks <- function(x, ...) {
	lines(centerlines(x), ...)
}

#' ggplot
#'
#' Plot a FreeFlowLinks object using \code{ggplot2}.
#'
#' Roadways are colored by the total emissions strength, Q (g/mi),
#' the product of flow \code{vehiclesPerHour} and \code{emissionFactor}.
#' 
#' Prettier but slower than \code{\link{lines}}.
#'
#' @param x a FreeFlowLinks object
#' @param ... other arguments
#' 
#' @keywords links
#' @S3method ggplot FreeFlowLinks
#' @export
ggplot.FreeFlowLinks <- function(x, ...) {
	links <- x
	require(ggplot2)
	kilo <- function(x) x / 1000.0
	easting <- function(...) scale_x_continuous("Easting (km)", labels = kilo, ...)
	northing <- function(...) scale_y_continuous("Northing (km)", labels = kilo, ...)
	dat <- as.data.frame(links)
	dat <- transform(dat, Q = vehiclesPerHour * emissionFactor)
	Q.breaks <- pretty(dat$Q, n = 3)
	map <- ggplot(dat) + coord_equal() + easting() + northing()
	centerlines <- geom_segment(aes(x=XL1, y=YL1, xend=XL2, yend=YL2, color=Q))
	return(map + centerlines + scale_color_gradient2(expression(bold(Q) ~~ bgroup("(", over(g, mi %.% hr), ")")), 
		low="green", mid="yellow", high="red", midpoint=median(dat$Q)))
}

#' as.data.frame
#'
#' Convert a FreeFlowLinks object to a data.frame.
#'
#' Each row in the data.frame corresponds to one or more segments from
#' the original (polyline) geometry. The starting coordinates (XL1, YL1)
#' and ending coordinates (XL2, YL2) are preserved as columns in the data.frame. 
#' Attributes are also preserved as columns.
#'
#' @param x a FreeFlowLinks object
#' @param row.names TODO
#' @param optional TODO
#' @param ... other arguments
#' 
#' @return a data.frame
#' @keywords links
#' @export
as.data.frame.FreeFlowLinks <- function(x, row.names, optional, ...) {
	try({
		.data <- attr(x, ".data")
		if(!is.null(.data))
			return(.data)
	})
	links <- x
	segments <- decimate(centerlines(links))
	colnames(segments) <- c("XL1", "YL1", "XL2", "YL2")
	attrs <- do.call("transform", c(list(centerlines(links)@data), links$transformArgs))
	dat <- suppressWarnings(merge(segments, attrs, by="row.names"))
	colnames(dat)[colnames(dat) == "Row.names"] <- "ID"
	return(dat)
}

as.Fortran.FreeFlowLinks <- function(x) {
	dat <- as.data.frame(x)
	with(dat, list(
		XL1 = real64(XL1),
		YL1 = real64(YL1),
		XL2 = real64(XL2),
		YL2 = real64(YL2),
		WL = real64(width),
		HL = real64(height),
		NTYP = as.integer(classification),  # was: TYP = as.character(classification)
		VPHL = real64(vehiclesPerHour),
		EFL = real64(emissionFactor)
	))
}

proj4string.FreeFlowLinks <- function(obj) {
	spobj <- centerlines(obj)
	return(proj4string(spobj))
}

setOldClass("FreeFlowLinks")
setMethod("as.Fortran", "FreeFlowLinks", as.Fortran.FreeFlowLinks)
setMethod("proj4string", "FreeFlowLinks", proj4string.FreeFlowLinks)