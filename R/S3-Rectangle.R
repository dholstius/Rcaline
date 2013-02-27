setClass('Rectangle', contains='matrix')

#' Rectangle
#'
#' Create a Rectangle object.
#'
#' @param obj a matrix or Spatial* object
#' @return a SpatialPolygons object with one Polygon
#' @rdname Rectangle
#' @keywords Rectangle
#' @export
setGeneric('Rectangle', function(obj, ...) standardGeneric('Rectangle'))

#' Rectangle.matrix
#'
#' @rdname Rectangle
Rectangle.matrix <- function(obj) {
	class(obj) <- c('Rectangle', 'matrix')
	return(obj)
}
setMethod('Rectangle', 'matrix', Rectangle.matrix)

#' Rectangle.Spatial
#'
#' @rdname Rectangle
Rectangle.Spatial <- function(obj) {
	mat <- bbox(obj)
	rec <- Rectangle(mat)
	attr(rec, 'proj4string') <- proj4string(obj)
	return(rec)
}
setMethod('Rectangle', 'Spatial', Rectangle.Spatial)

#' bbox.Rectangle
#' 
#' @rdname Rectangle-methods
bbox.Rectangle <- function(obj) {
	return(matrix(obj, ncol=ncol(obj), nrow=nrow(obj), dimnames=dimnames(obj)))
}
setMethod('bbox', 'Rectangle', bbox.Rectangle)

#' as.Rectangle.SpatialPolygons
#' 
#' @rdname Rectangle-methods
as.SpatialPolygons.Rectangle <- function(from) {
	arr <- array(from, dimnames=list(c('xmin','ymin','xmax','ymax')))
	coords <- with(as.list(arr), 
		matrix(c(xmin, ymin, 
			     xmin, ymax, 
			     xmax, ymax, 
			     xmax, ymin, 
			     xmin, ymin), ncol=2, byrow=TRUE))
	colnames(coords) <- c('x', 'y')
	poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=0)))
	proj4 <- attr(from, 'proj4string')
	if(!is.null(proj4))
		proj4string(poly) <- proj4
	return(poly)
}
setAs(from='Rectangle', to='SpatialPolygons', def=as.SpatialPolygons.Rectangle)

#' as.array.Rectangle
#'
#' @rdname Rectangle-methods
as.array.Rectangle <- function(from) {
	mat <- as(from, 'matrix')
	lbl <- paste(rownames(mat), rep(colnames(mat), each=2), sep='')
	arr <- array(mat, dimnames=list(lbl))
	return(arr)
}
setAs(from='Rectangle', to='array', def=as.array.Rectangle)

#' as.data.frame.Rectangle
#'
#' @rdname Rectangle-methods
as.data.frame.Rectangle <- function(from) {
	arr <- as(from, 'array')
	dat <- data.frame(t(arr))
	return(dat)
}
setAs(from='Rectangle', to='data.frame', def=as.data.frame.Rectangle)