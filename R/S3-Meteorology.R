#' Pasquill
#'
#' Construct a vector of atmospheric stability classes.
#'
#' @param x TODO
#' @keywords meteorology
#' @export
#' @examples
#' LinkType(rep('AG', 5))
Pasquill <- function(x) {
	obj <- ordered(x, levels=1:7, labels=LETTERS[1:7])
	class(obj) <- c('Pasquill', 'ordered')
	return(obj)
}

#' ISCFile
#'
#' Read records from a file in ISC format.
#'
#' NOTE: Rotates the "flow vector" (wind direction) column
#' by 180 degrees to obtain the wind bearing.
#'
#' @param filename filename
#' @return an ISCFile object (essentially a \link{data.frame})
#' @keywords meteorology
#' @export
ISCFile <- function(filename) {

	# Assert that the file exists
	stopifnot(file.exists(filename))
	
	# Skip the header and read the records
	obj <- read.fwf(filename, 
		widths = c(2, 2, 2, 2, 9, 9, 6, 2, 7, 7),
		skip = 1,
		stringsAsFactors = FALSE,
		# c("4X2", "2F9", "F6", "I2", "2F7"),
		colClasses = c("integer", "integer", "integer", "integer",
			"numeric", "numeric", "numeric", "integer",
			"numeric", "numeric"),
		col.names = c("year", "month", "day", "hour", 
			"flowVector", "windSpeed", "temperature", "stabilityClass", 
			"mixingHeight.rural", "mixingHeight.urban")
	)
	
	if(all(obj$year < 70)) {
		message("Adding 2000 to two-digit years")
		obj <- transform(obj, year = year + 2000)
      } else if(all(obj$year < 100)) {
      	message("Adding 1900 to two-digit years")
      	obj <- transform(obj, year = year + 1900)
      }
	
	# FIXME put the header data here
	attr(obj, "metadata") <- NA
	
	row.names(obj) <- with(obj,
		as.POSIXlt(sprintf("%02d/%02d/%02d %02d:00:00", year, month, day, hour - 1))
	)	
      
	class(obj) <- c("ISCFile", class(obj))
	return(obj)

}

#' Meteorology
#'
#' Construct a Meteorology object, generally from a file containing ISC-formatted records.
#' 
#' @param x a filename, \link{ISCFile} object, or data.frame
#' @param use whether to use urban or rural mixing heights
#' @return a Meteorology object (essentially a \link{data.frame})
#' @keywords meteorology
#' @export
Meteorology <- function(x, ...) UseMethod("Meteorology")

#' @S3method Meteorology data.frame
#' @rdname Meteorology
#' @export
Meteorology.data.frame <- function(x) {
	stopifnot(identical(names(x),
		c('windSpeed', 'windBearing', 'stabilityClass', 'mixingHeight')))
	class(x) <- c('Meteorology', 'data.frame')
	return(x)
}

#' @S3method Meteorology character
#' @rdname Meteorology
#' @export
Meteorology.character <- function(x) {
	isc <- ISCFile(filename=x)
	met <- Meteorology(isc)
	return(met)
}

#' @S3method Meteorology ISCFile
#' @rdname Meteorology
#' @export
Meteorology.ISCFile <- function(x, use = c("urban", "rural")) {

	# Determine whether to use urban or rural mixing heights column
	use <- match.arg(use)
	message("Using ", use, " mixing heights (See ?Meteorology for more).")
	
	# Rotate flow vector by 180 to get wind bearing;
	# Convert stability classes to Pasquill;
	# Discard rural or urban mixing height (depending on user's preference)
	rotate.degrees <- function(angle, by) ((angle + by) + 360) %% 360
	met <- with(x, data.frame( 
			windSpeed = windSpeed, 
			windBearing = rotate.degrees(flowVector, by = 180.0),
			stabilityClass = Pasquill(stabilityClass),
			mixingHeight = switch(use, 
				rural = mixingHeight.rural, 
				urban = mixingHeight.urban
			)
		)
	)
	row.names(met) <- row.names(x)

	# Warn about calm wind speeds
	calm <- which(met$windSpeed < 1)
	if(length(calm) > 0) {
		message(length(calm), " wind speeds less than 1.0 m/s (will produce NAs): ", paste(calm, collapse=', '))
	}

	met <- within(met, {
		attr(windSpeed, 'units') <- "m/s"
		attr(windBearing, 'units') <- "deg"
		attr(mixingHeight, 'units') <- "m"
	})
	class(met) <- c("Meteorology", "data.frame")
	return(met)
}