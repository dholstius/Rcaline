#' predict.Caline3Model
#'
#' @param object a \code{\link{Caline3Model}} object
#' @param units one of 'ppm', 'ug/m3', or 'mg/m3'
#'
#' @return matrix of predicted values
#'
#' @keywords predict model
#' @S3method predict Caline3Model
#' @importFrom stats predict
#' @export
predict.Caline3Model <- function(object, units='ppm') {
	
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
      pred <- matrix(NA, 
		nrow = nrow(as.data.frame(rcp)), 
		ncol = nrow(as.data.frame(met)), 
		dimnames = list(rownames(rcp), rownames(met)))	

	# Compute only the conditions (columns) for which wind speed >= 1.0
	non.calm <- with(met, which(windSpeed >= 1.0))
	args <- c(
		as.Fortran(rcp),
            as.Fortran(lnk), 
		as.Fortran(met[non.calm,]), 
		list(
			ATIM = as.single(60.0),
			Z0 = as.single(ter$surfaceRoughness),
			VS = as.single(pol$settlingVelocity),
			VD = as.single(pol$depositionVelocity)))		
	computed <- do.call("CALINE3", args)

	# Assign the computed estimates back to the matrix
	stopifnot(nrow(computed) == nrow(pred))
	stopifnot(ncol(computed) == length(non.calm))
	pred[,non.calm] <- computed

      if(units == 'ppm') {
		pred <- pred * 0.0245 / pol$molecularWeight
	} else if (units == 'mg/m3') {
	      pred <- pred * 1.0e3
	} else {
	      # Default is ug/m3
	}

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
#' @S3method ggplot Caline3Model
#' @export
ggplot.Caline3Model <- function(data, ...) {
	receptor.data <- as.data.frame(data$receptors)
	link.data <- as.data.frame(data$links)
	receptor.layer <- geom_point(aes(x=x, y=y), pch=3, alpha=0.5, data=receptor.data)
	link.layer <- geom_segment(aes(x=XL1, y=YL1, xend=XL2, yend=YL2), data=link.data)
	base.layer <- ggplot() + coord_equal() + easting() + northing()
	return(base.layer + link.layer + receptor.layer)
}