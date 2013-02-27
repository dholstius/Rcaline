resize.Polygon <- function(obj, size=1.0) {
	coords <- coordinates(obj)
	n <- nrow(coords)
	tmp <- scale(coords[-n,], scale=rep(1/size, 2))
	scaled <- t(tmp) + attr(tmp, 'scaled:center')
	closed <- t(cbind(scaled, scaled[,1]))
	return(Polygon(closed, hole=obj@hole))
}

resize.Polygons <- function(obj, size=1.0) {
	poly <- lapply(obj@Polygons, function(x) resize(x, size=size))
	return(Polygons(poly, ID=obj@ID))
}

resize.SpatialPolygons <- function(obj, size=1.0) {
	poly <- lapply(obj@polygons, function(x) resize(x, size=size))
	return(SpatialPolygons(poly, pO=obj@plotOrder, proj4string=CRS(proj4string(obj))))
}

resize.Rectangle <- function(obj, size=1.0) {
	poly <- as(obj, 'SpatialPolygons') 
	resized <- resize(poly, size=size)
	rec <- Rectangle(bbox(resized))
	attr(rec, 'proj4string') <- attr(obj, 'proj4string')
	return(rec)
}

#' resize
#'
#' Shrink or grow features within a Spatial* object.
#'
#' @export
setGeneric('resize', function(obj, size=1.0) standardGeneric("resize"))
setMethod('resize', 'Polygon', resize.Polygon)
setMethod('resize', 'Polygons', resize.Polygons)
setMethod('resize', 'SpatialPolygons', resize.SpatialPolygons)
setMethod('resize', 'Rectangle', resize.Rectangle)