as.SpatialLines.SpatialPolygons <- function(spobj) {
  stopifnot(class(spobj) == "SpatialPolygons")
  as.Lines <- function(polygons) Lines(lapply(polygons@Polygons, Line), ID=polygons@ID)
  result <- SpatialLines(lapply(spobj@polygons, as.Lines))
  proj4string(result) <- proj4string(spobj)
  return(result)
}

require(sp)
as.SpatialLines <- function(spobj) stop(paste("Sorry, I don't know what to do with a", class(spobj)))
setGeneric("as.SpatialLines")
setMethod("as.SpatialLines", "SpatialPolygons", as.SpatialLines.SpatialPolygons)

#' Extract segments from a polyline or polylines
#'
#' @param x Lines or SpatialLines object
#' @return matrix of coordinates
#' @export
extract_segments <- function(x, ...) standardGeneric("extract_segments")
setGeneric("extract_segments", extract_segments)

#' @rdname extract_segments
#' @export
extract_segments.default <- function(x, suffixes=c(".start", ".end")) {
  coords <- coordinates(x)
  result <- cbind(coords[-nrow(coords),,drop=FALSE], coords[-1,,drop=FALSE])
  if(!is.null(colnames(coords)))
    colnames(result) <- paste(colnames(coords), rep(suffixes, each=ncol(coords)), sep="")
  return(result)
}

#' @rdname extract_segments
#' @export
extract_segments.Lines <- function(x, ...) {
  nested <- rapply(x@Lines, extract_segments.default, ..., how="list")
  flattened <- do.call(rbind, nested)
  rownames(flattened) <- rep(x@ID, nrow(flattened))
  return(flattened)
}

#' @rdname extract_segments
#' @export
extract_segments.SpatialLines <- function(x, ...) {
  nested <- lapply(x@lines, extract_segments.Lines)
  do.call(rbind, nested)
}

setMethod("extract_segments", "Lines", extract_segments.Lines)
setMethod("extract_segments", "SpatialLines", extract_segments.SpatialLines)
