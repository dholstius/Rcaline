#' ggplot.Meteorology
#'
#' Plots a wind rose using ggplot2.
#'
#' @param x a Meteorology object
#' @param ... other arguments
#'
#' @keywords meteorology
#' @S3method ggplot Meteorology
#' @export
ggplot.Meteorology <- function(data, ...) {
	require(ggplot2)
	dat <- as.data.frame(data)
	speeds <- with(dat, c(0:3, 5, 10, floor(1 + max(windSpeed))))
	dat <- transform(dat, windLevel = cut(windSpeed, speeds, right=FALSE))
	p <- ggplot(aes(x=windBearing, y=..count../8760, fill=windLevel), data=dat)
	p <- p + coord_polar(theta='x')
	p <- p + geom_bar(binwidth=30)
	p <- p + scale_x_continuous("", limits=c(0, 360), breaks=seq(0, 360, by=45)) 
	p <- p + scale_y_continuous("")	# , labels="percent")
	p <- p + scale_fill_brewer("Speed (m/s)", pal="Blues")
	return(p)
}

as.Fortran.Meteorology <- function(x) {
	dat <- as.data.frame(x)
	with(dat, list(	
		UM = real64(windSpeed),
		BRGM = real64(windBearing),
		CLASM = as.integer(pmin(stabilityClass, Pasquill(6))),
		MIXHM = real64(mixingHeight)
	))
}

setOldClass("Meteorology")
setMethod("as.Fortran", "Meteorology", as.Fortran.Meteorology)