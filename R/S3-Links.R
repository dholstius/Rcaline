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

#' lines
#'
#' Plot a FreeFlowLinks object.
#'
#' @param x a FreeFlowLinks object
#' @param ... other arguments
#'
#' @keywords links
#' @method lines FreeFlowLinks
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
#' @method ggplot FreeFlowLinks
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
  link_segments <- Rcaline::extract_segments(centerlines(links))
  colnames(link_segments) <- c("XL1", "YL1", "XL2", "YL2")
  link_attrs <- do.call("transform", c(list(centerlines(links)@data), links$transformArgs))
  link_data <- suppressWarnings(merge(link_segments, link_attrs, by="row.names"))
  colnames(link_data)[colnames(link_data) == "Row.names"] <- "ID"
  return(link_data)
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

#' FreeFlowLinks accessors
#'
#' Inspect attributes of a FreeFlowLinks object.
#'
#' @param obj a FreeFlowLinks object
#' @param ... further arguments
#' @rdname FreeFlowLinks-accessors
#' @export
centerlines <- function(obj, ...) UseMethod("centerlines")

#' FreeFlowLinks accessors
#'
#' @method centerlines FreeFlowLinks
#' @rdname FreeFlowLinks-accessors
#' @export
centerlines.FreeFlowLinks <- function(obj, ...) obj$centerlines
