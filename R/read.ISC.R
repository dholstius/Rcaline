read.ISC <- function(
	file
) {
	warning("read.ISC() is deprecated. See ?Meteorology or ?ISCFile for more.")
	header <- readLines(file, 1)
	header.tokens <- strsplit(header, '\\s+')[[1]]
	result <- lapply(as.list(header.tokens)[-1], as.integer)
	names(result) <- c('surface.station','surface.year','upper.station','upper.year')
	result$records <- read.fortran(
		file,
		c("4I2","2F9","F6","I2","2F7"),
		skip = 1,
		col.names = c(
			"year", 
			"month", 
			"day", 
			"hour", 
			"flow.vector", 
			"wind.speed", 
			"temperature", 
			"stability.class", 
			"rural.mixing.height", 
			"urban.mixing.height")
	)
	
	# Label the rows of the resulting data frame.
	row.names(result$records) <- with(result$records, 
		sprintf('%02d-%02d-%02d %02d:00', year, month, day, hour-1))
		
	# Rotate the flow vector by 180 degrees to obtain the wind bearing.
	rotate <- function(angle, by) ((angle + by) + 360) %% 360
	result$records$wind.bearing <- with(result$records, rotate(flow.vector, 180.0))
	result$records$flow.vector <- NULL
		return(result)
	
}
