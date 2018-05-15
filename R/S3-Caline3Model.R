#' Modeling
#'
#' Construct, run, summarize and show results from a Caline3Model object.
#'
#' The model (object) contains all of the link and met data,
#' as well as site-specific model parameters, such as surface roughness.
#' Internally, these are stored as single-precision arrays. If you want to
#' experiment by changing a parameter, construct a new Caline3Model, unless
#' you really know what you are doing.
#'
#' Use \code{predict} to predict concentrations; \code{aggregate} to compute summary statistics,
#' such as means and maxima; and \code{spplot} or \code{ggplot} to visualize results.
#'
#' @param links a \code{\link{FreeFlowLinks}} object
#' @param meteorology a \code{\link{Meteorology}} object
#' @param receptors a \code{\link{Receptors}} object
#' @param terrain a \code{\link{Terrain}} object
#' @param pollutant a \code{\link{Pollutant}} object
#'
#' @return a Caline3Model object
#'
#' @keywords model predict aggregate spplot ggplot2
#' @export
Caline3Model <- function(links, meteorology, receptors, terrain, pollutant) {
	stopifnot(inherits(links, "FreeFlowLinks"))
	stopifnot(inherits(meteorology, "Meteorology"))
	stopifnot(inherits(receptors, "SpatialPoints"))
	stopifnot(inherits(terrain, "Terrain"))
	stopifnot(inherits(pollutant, "Pollutant"))
	obj <- list(
		links = links,
		meteorology = meteorology,
		receptors = receptors,
		terrain = terrain,
		pollutant = pollutant)
	class(obj) <- "Caline3Model"
	return(obj)
}

#' Caline3Model accessors
#'
#' Inspect model elements from a Caline3Model.
#'
#' @param model a Caline3Model object
#' @param ... further arguments
#' @rdname Caline3Model-accessors
#' @export
links <- function(model, ...) UseMethod("links")

#' Caline3Model accessors
#'
#' @method links default
#' @rdname Caline3Model-accessors
#' @export
links.default <- function(model, ...) model$links

#' Caline3Model accessors
#'
#' @rdname Caline3Model-accessors
#' @export
receptors <- function(model, ...) UseMethod("receptors")

#' Caline3Model accessors
#'
#' @method receptors default
#' @rdname Caline3Model-accessors
#' @export
receptors.default <- function(model, ...) {
  rcp <- model$receptors
  stopifnot(identical(coordnames(rcp), c('x', 'y', 'z')))
  return(rcp)
}

#' Caline3Model accessors
#'
#' @rdname Caline3Model-accessors
#' @export
meteorology <- function(model, ...) UseMethod("meteorology")

#' Caline3Model accessors
#'
#' @method meteorology default
#' @rdname Caline3Model-accessors
#' @export
meteorology.default <- function(model, ...) model$meteorology

#' Caline3Model accessors
#'
#' @rdname Caline3Model-accessors
#' @export
pollutant <- function(model, ...) UseMethod("pollutant")

#' Caline3Model accessors
#'
#' @method pollutant default
#' @rdname Caline3Model-accessors
#' @export
pollutant.default <- function(model, ...) model$pollutant

#' Caline3Model accessors
#'
#' @rdname Caline3Model-accessors
#' @export
terrain <- function(model, ...) UseMethod("terrain")

#' Caline3Model Methods
#'
#' @method terrain default
#' @rdname Caline3Model-accessors
#' @export
terrain.default <- function(model, ...) model$terrain

#' predict.Caline3Model
#'
#' @param object a \code{\link{Caline3Model}} object
#' @param units one of 'mg/m^3', 'ug/m^3', or 'ppm'
#'
#' @return matrix of predicted values
#'
#' @keywords predict model
#' @method predict Caline3Model
#' @importFrom stats predict
#' @export
predict.Caline3Model <- function (object, max_dist = 3000, units = "ppm", CAL_MODEL = "CAL3RXL") {

  stopifnot(inherits(object, 'Caline3Model'))
  lnk <- links(object)
  met <- meteorology(object)
  rcp <- receptors(object)
  ter <- terrain(object)
  pol <- pollutant(object)

  NR <- nrow(as.data.frame(rcp))
  NM <- nrow(as.data.frame(met))
  NL <- nrow(as.data.frame(lnk))

  # Initialize the full matrix
  pred <- matrix(
    NA_real_, nrow = NR, ncol = NM,
    dimnames = list(RCP = row.names(rcp), MET = row.names(met)))

  # Compute only the conditions for which wind speed >= 1.0
  rcp_args <- Rcaline:::as.Fortran(rcp)
  lnk_args <- Rcaline:::as.Fortran(lnk)
  non_calm <- with(met, which(windSpeed >= 1.0))
  met_args <- Rcaline:::as.Fortran(met[non_calm,])

  # For restricting to (link, receptor) pairs of distance ≤ 1000 m
  hypot2 <- function (x1, y1, x2, y2) (x2 - x1)^2 + (y2 - y1)^2
  squared_distances <- with(as.list(c(lnk_args, rcp_args)), outer(1:NR, 1:NL, function (i, j) {
    XL <- (XL1[j] + XL2[j]) / 2
    YL <- (YL1[j] + YL2[j]) / 2
    hypot2(XR[i], YR[i], XL, YL)
  }))
  LXR <- (squared_distances <= (max_dist ^ 2))

  aux_args <- list(
    ATIM = 60.0, Z0 = ter$surfaceRoughness,
    VS = pol$settlingVelocity, VD = pol$depositionVelocity,
    LXR = LXR)

  all_args <- c(rcp_args, lnk_args, met_args, aux_args)
  C <- do.call(CAL_MODEL, all_args)

  # Assign the computed estimates back to the matrix
  require(testthat)
  expect_that(nrow(C), equals(nrow(pred)))
  expect_that(ncol(C), equals(length(non_calm)))
  pred[,non_calm] <- rowSums(C)

  # Optional: convert to ppm
  if(units == 'ppm') {
    pred <- pred * 0.0245 / pol$molecularWeight
  } else if (units == "ug/m^3") {
    pred <- pred * 1.0e3
  } else if (units != "mg/m^3") {
    warning("Unrecognized value for 'units': ", units)
  }

  # Reclass and return
  class(pred) <- c("HourlyConcentrations", "matrix")
  attr(pred, "model") <- object
  return(pred)

}

#' Visualization
#'
#' Plot model elements using ggplot2.
#'
#' @param data a Caline3Model object
#'
#' @keywords ggplot ggplot2
#' @importFrom ggplot2 ggplot
#' @method ggplot Caline3Model
#' @export
ggplot.Caline3Model <- function(data, ...) {
  receptor.data <- as.data.frame(data$receptors)
  link.data <- as.data.frame(data$links)
  receptor.layer <- geom_point(aes(x=x, y=y), pch=3, alpha=0.5, data=receptor.data)
  link.layer <- geom_segment(aes(x=XL1, y=YL1, xend=XL2, yend=YL2), data=link.data)
  base.layer <- ggplot() + coord_equal() + easting() + northing()
  return(base.layer + link.layer + receptor.layer)
}
