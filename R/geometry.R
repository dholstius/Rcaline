as.SpatialLines.SpatialPolygons <- function(spobj) {
    stopifnot(class(spobj)=="SpatialPolygons")
    as.Lines <- function(polygons) Lines(lapply(polygons@Polygons, Line), ID=polygons@ID)
    result <- SpatialLines(lapply(spobj@polygons, as.Lines))
    proj4string(result) <- proj4string(spobj)
    return(result)
}

require(sp)
as.SpatialLines <- function(spobj) stop(paste("Sorry, I don't know what to do with a", class(spobj)))
setGeneric("as.SpatialLines")
setMethod("as.SpatialLines", "SpatialPolygons", as.SpatialLines.SpatialPolygons)

# Generic function for splitting polylines into segments
setGeneric("decimate", function(x, ...) standardGeneric("decimate"))

decimate.default <- function(x, suffixes=c(".start",".end")) {
	coords <- coordinates(x)
	result <- cbind(coords[-nrow(coords),,drop=FALSE], coords[-1,,drop=FALSE])
	if(!is.null(colnames(coords)))
		colnames(result) <- paste(colnames(coords), rep(suffixes, each=ncol(coords)), sep="")
	return(result)
}

decimate.Lines <- function(x, ...) {
	nested <- rapply(x@Lines, decimate.default, ..., how="list")
	flattened <- do.call(rbind, nested)
	rownames(flattened) <- rep(x@ID, nrow(flattened))
	return(flattened)
}

decimate.SpatialLines <- function(x, ...) {
	nested <- lapply(x@lines, decimate.Lines)
	do.call(rbind, nested)
}

setMethod("decimate", "Lines", decimate.Lines)
setMethod("decimate", "SpatialLines", decimate.SpatialLines)