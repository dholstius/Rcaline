#' LinkType
#'
#' Create a vector of link classifications.
#'
#' @param x TODO
#' @keywords links
#' @export
#' @examples
#' LinkType(rep(0, 10))
LinkType <- function(x) {
	factor(x, levels = 1:4, labels = c("AG", "BR", "FL", "DP"))
}

#' FreeFlowLinks
#'
#' Construct a FreeFlowLinks object from a SpatialLinesDataFrame.
#'
#' \code{vehiclesPerHour}, \code{emissionFactor}, and other arguments
#' can all be specified as expressions. Use them the same way you
#' would use arguments to \code{\link{transform}}.
#
#' A FreeFlowLinks object can be plotted with \code{\link{plot}} or coerced 
#' to a \code{data.frame} with \code{\link{as.data.frame}}.
#' 
#' @param spobj a SpatialLinesDataFrame object
#' @param vehiclesPerHour average number of vehicles per hour (also known as "flow")
#' @param emissionFactor emissions per vehicle, in grams per mile (per hour)
#' @param width mixing zone width, in meters
#' @param height elevation above ground level (not sea level!)
#' @param classification see \code{\link{LinkType}} for possible values
#' @param ... other arguments
#' 
#' @return a FreeFlowLinks object
#' @keywords links
#' @export
FreeFlowLinks <- function(spobj, vehiclesPerHour, emissionFactor, width, height=0.0, classification=LinkType(0), ...) 
{
	if(missing(height))
		height <- 0.0
	
	if(missing(classification))
		classification <- LinkType(1)
		
	args <- as.list(match.call())[-1]
	formalArgs <- args[names(args) %in% names(formals())]
	
	obj <- list()
	class(obj) <- "FreeFlowLinks"
	obj$centerlines <- spobj
	obj$transformArgs <- formalArgs[-1]
	if(is.null(obj$transformArgs$height))
            obj$transformArgs$height = height
	if(is.null(obj$transformArgs$classification))
            obj$transformArgs$classification = classification
	
	# Precompute the segments
	attr(obj, ".data") <- as.data.frame(obj)
	
	return(obj)
}