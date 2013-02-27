#' Receptor grids
#'
#' Construct a set of receptors, using a \link{FreeFlowLinks} as the basis.
#'
#' ReceptorGrid constructs a regular Cartesian grid of receptors no more 
#' than \code{maxDistance} from \code{links}.
#'
#' @param links a \link{FreeFlowLinks} object
#' @param z elevation in meters
#' @param resolution spacing between receptors, in meters
#' @param maxDistance buffer radius, in meters
#' @param rgeos.scale can be adjusted if precision warnings occur
#'
#' @return SpatialPointsDataFrame
#'
#' @keywords receptors
#' @seealso ReceptorRings
#' @rdname ReceptorGrids
#' @export
ReceptorGrid <- function(links, z=1.8, resolution=1000.0, maxDistance=1000.0, 
		rgeos.scale=1e+06) {
	require(rgeos)
	rgeos::setScale(rgeos.scale)
	
	# FIXME: take roadway width into account (don't measure distance from centerline, but from edge of road)
	polylines <- centerlines(links)
	buf <- rgeos::gBuffer(polylines, width=maxDistance)
	xy <- spsample(buf, cellsize = c(resolution, resolution), type = "regular")
	coordnames(xy) <- c("x", "y")
	spobj <- SpatialPoints(xy)
	proj4string(spobj) <- proj4string(links)
	rcp <- Receptors(spobj, z=z)
	
	# Set row names
	row.names(rcp) <- paste('RECP.', 1:length(rcp))
	
	return(rcp)
}

#' Receptor grids
#'
#' ReceptorRings constructs concentric rings of receptors at specific
#' distances from \code{links}. 
#'
#' @param links a \link{FreeFlowLinks} object
#' @param z elevation in meters
#' @param distances list of distances to the roadway centerline, in meters
#' @param spacing TODO
#' @param rgeos.scale TODO
#'
#' @return SpatialPointsDataFrame
#'
#' @keywords receptors
#' @rdname ReceptorGrids
#' @export
ReceptorRings <- function(links, z=1.8, distances=c(50, 100, 250, 500, 1000), 
	spacing=identity, rgeos.scale=1e+06) {
		
	require(rgeos)
	rgeos::setScale(rgeos.scale)
	
	# Create buffers from centerlines and 'distances' vector;
	# then discard inside/outside topology (save only the edges)
	# TODO: take width into account (don't measure distance from centerline, but from edge of road)
	buffers <- lapply(distances, function(x) gBuffer(centerlines(links), width = x)) 
	rings <- lapply(buffers, as.SpatialLines)
	
	# Sample at fixed intervals along each ring
	perimeter <- function(ring) sum(unlist(lapply(ring@lines, LinesLength)))
	spsample.ring <- function(ring, ring.width, spacings) {
		pts <- spsample(ring, type = "regular", n = perimeter(ring) / spacings) 
		coordnames(pts) <- c("x", "y") 
		d <- rep(ring.width, nrow(pts@coords)) 
		SpatialPointsDataFrame(pts, data.frame(distance = d, spacing = spacings))
	}
	spobj <- do.call(rbind, mapply(spsample.ring, rings, distances, spacings=spacing(distances)))
	rcp <- Receptors(spobj, z=z)
      proj4string(rcp) <- proj4string(centerlines(links))
	
	# Set row names
	row.names(rcp) <- paste('RECP.', 1:length(rcp))
	
	return(rcp)
}

#' Receptor locations
#'
#' Construct a set of receptors. All coordinates must be in meters!
#'
#' @param obj a SpatialPoints or SpatialPointsDataFrame object, or a two-dimensional matrix of coordinates
#' @param z elevation(s) above ground, in meters
#' @param check.projection checks to see that obj has an associated projection
#'
#' @return SpatialPoints* object
#'
#' @keywords receptors
#' @seealso ReceptorGrid ReceptorRings
#' @export
Receptors <- function(obj, ...) UseMethod("Receptors")

#' @S3method Receptors SpatialPointsDataFrame
#' @rdname Receptors
Receptors.SpatialPointsDataFrame <- function(obj, z=1.8, check.projection=TRUE) {
	if('z' %in% names(obj@data)) {
		warning('z attribute detected in SpatialPointsDataFrame')
	}
	spobj <- as(obj, 'SpatialPoints')
	rcp <- Receptors.SpatialPoints(spobj, z=z, check.projection=check.projection)
	SpatialPointsDataFrame(rcp, data=obj@data)
}

#' @S3method Receptors SpatialPoints
#' @rdname Receptors
Receptors.SpatialPoints <- function(obj, z=1.8, check.projection=TRUE) {
	if(check.projection) {
		if(is.na(is.projected(obj))) {
			warning("No projection information found. Assuming planar coordinates, in meters.")
		} else if(!is.projected(obj)) {
			stop("Coordinates are not projected. Maybe they're in latitude and longitude? You need to reproject this data first.")
		}
	}
	rcp <- Receptors.matrix(coordinates(obj), z=z)
	proj4string(rcp) <- CRS(proj4string(obj))
	return(rcp)
}

#' @S3method Receptors matrix
#' @rdname Receptors
Receptors.matrix <- function(obj, z=1.8) {
	coords <- obj
	if (ncol(coords) == 2) {
	      coords <- cbind(coords, z)
	}
      stopifnot(ncol(coords) == 3)
	colnames(coords) <- c('x', 'y', 'z')
	if(is.null(rownames(obj))) {
      	rownames(coords) <- paste('RECP.', 1:nrow(obj))      
      } else {
            rownames(coords) <- rownames(obj)
      }
      return(SpatialPoints(coords))
}

as.Fortran.Receptors <- function(x) {
	dat <- as.data.frame(x)
	with(dat, list(XR=real64(x), YR=real64(y), ZR=real64(z)))
}

setMethod("as.Fortran", "SpatialPoints", as.Fortran.Receptors)

