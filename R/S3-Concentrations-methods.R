#' Handling model results
#'
#' Aggregate the "raw" result matrix obtained from \code{\link{predict.Caline3Model}},
#' summarizing the pred estimates for each receptor. 
#'
#' Use \code{as(x, "SpatialPointsDataFrame")} to re-bind these summary statistics 
#' with the locations (and other attributes) of the rcp used in the prediction step.
#'
#' @param x concentrations obtained from \code{\link{predict.Caline3Model}}
#' @param FUN a list of summary functions to apply to each receptor location
#' @param na.rm logical; passed to each summary function in turn
#' @param ... other arguments
#'
#' @return matrix of summary statistics\
#'
#' @S3method aggregate HourlyConcentrations
#' @importFrom stats aggregate
#' @rdname HourlyConcentrations-methods
#' @export
aggregate.HourlyConcentrations <- function(x, FUN=list("min", "mean", "median", "GM", "max", "sd"), na.rm=T, ...) {
	pred <- x
	GM <- function(x, ...) exp(mean(log(x), ...))
	agg <- do.call(cbind, lapply(FUN, function(f) apply(pred, MARGIN=1, FUN=f, na.rm=na.rm)))
	colnames(agg) <- FUN
	rownames(agg) <- rownames(pred)
	class(agg) <- c("AggregatedConcentrations", "matrix")
	attr(agg, "model") <- attr(pred, "model")
	return(agg)
}

#' @param from an AggregatedConcentrations object
#'
#' @return a SpatialPointsDataFrame
#'
#' @name as.AggregatedConcentrations.SpatialPointsDataFrame
#' @rdname AggregatedConcentrations-methods
#' @export
as.AggregatedConcentrations.SpatialPointsDataFrame <- function(from) {
	model <- attr(from, 'model')
	rcp <- receptors(model)
	if("data" %in% slotNames(rcp)) {
		dat <- data.frame(rcp@data, as.data.frame(from))
	} else {
		dat <- as.data.frame(from)
	}
	spdf <- SpatialPointsDataFrame(coordinates(rcp), data=dat)
	proj4string(spdf) <- proj4string(rcp)
	return(spdf)
}

setOldClass("AggregatedConcentrations")
setAs("AggregatedConcentrations", "SpatialPointsDataFrame", as.AggregatedConcentrations.SpatialPointsDataFrame)

#' Visualization
#'
#' Plot aggregate concentrations using spplot or ggplot2.
#'
#' @param x an AggregatedConcentrations object, obtained by calling aggregate() on the results of a modeling run
#' @param ... further arguments to spplot
#'
#' @keywords spplot
#' @importFrom sp spplot
#' @rdname AggregatedConcentrations-methods
#' @export
spplot.AggregatedConcentrations <- function(obj, ...) {
	spdf <- as(obj, 'SpatialPointsDataFrame')
	spplot(spdf, ...)
}

setOldClass("AggregatedConcentrations")
setMethod("spplot", "AggregatedConcentrations", spplot.AggregatedConcentrations)

#' Visualization
#'
#' @keywords ggplot2
#' @importFrom ggplot2 ggplot
#' @S3method ggplot AggregatedConcentrations
#' @rdname AggregatedConcentrations-methods
#' @export
ggplot.AggregatedConcentrations <- function(data, bounds, ...) {
	model <- attr(data, 'model')
	rcp <- receptors(model)
	lnk <- links(model)
	receptor.data <- as.data.frame(rcp)
	link.data <- as.data.frame(lnk)
	receptor.layer <- geom_point(aes(x=x, y=y), pch=3, alpha=0.1, data=receptor.data)
	link.layer <- geom_segment(aes(x=XL1, y=YL1, xend=XL2, yend=YL2), alpha=0.5, data=link.data)
	results.spatial <- as(data, 'SpatialPointsDataFrame')
	results.data <- as.data.frame(results.spatial)
	#results.geom <- geom_point(aes(x=x, y=y, order=mean, alpha=mean, color=mean))
	map <- ggplot(results.data) + coord_equal() + link.layer + receptor.layer
	if(!missing(bounds)) {
		bb <- bbox(bounds)
		map <- map + easting(limits=bb['x',]) + northing(limits=bb['y',])
	}
	return(map)
}