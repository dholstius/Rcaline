#' Construct a Parameters object.
#'
#' @param surfaceRoughness in centimeters
#' @param averagingTime in minutes
#' @param settlingVelocity of the pollutant
#' @param depositionVelocity of the pollutant
#'
#' @return a Parameters object
#'
#' @export
Parameters <- function(surfaceRoughness, averagingTime=60.0, settlingVelocity=0.0, depositionVelocity=0.0) {
    obj <- list(
		surfaceRoughness = surfaceRoughness,
		averagingTime = averagingTime, 
		settlingVelocity = settlingVelocity, 
		depositionVelocity = depositionVelocity
	)
    class(obj) <- "Parameters"
    return(obj)
}

as.Fortran.Parameters <- function(x) {
	dat <- as.data.frame.list(x)
	with(dat, list(
		VS = real64(settlingVelocity),
		VD = real64(depositionVelocity)
	))
}

setOldClass("Parameters")
setMethod("as.Fortran", "Parameters", as.Fortran.Parameters)