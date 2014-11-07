#' @export
CALINE3.predict <- function(
	receptors,
	links,
	meteorology,
	surface.roughness,
	mixing.heights = "urban",
	averaging.time = 60.0,
	settling.velocity = 0.0,
	deposition.velocity = 0.0
) {

  .Deprecated(Caline3Model, msg="CALINE3.predict() is deprecated. Please use Caline3Model() and predict.Caline3Model() instead.")

	# Receptor specifications. A SpatialPoints object or SpatialPointsDataFrame
	# is the preferred currency.
	if(hasMethod('coordinates', class(receptors))) {
		if(inherits(receptors, 'SpatialPoints')) {
			if(is.na(proj4string(receptors))) {
				warning("Receptors missing proj4string. See ?proj4string for more.")
			} else {
				if(!is.projected(receptors)) {
					stop("Receptor coordinates must be given in a projected coordinate system (in meters). See the documentation for the 'sp' package for details, or ?SpatialPoints.")
				}
			}
		}
		coords <- coordinates(receptors)
	} else {
		coords <- receptors[,c('x','y','z')]
	}
	XR <- as.single(coords[, 1])
	YR <- as.single(coords[, 2])
	if(!is.null(receptors$z)) {
		ZR <- as.single(receptors$z)
	} else if(ncol(coords) > 2) {
		ZR <- as.single(coords[,3])
	} else {
		ZR <- as.single(rep(1.8, length(XR)))
		warning("receptors$z is missing. Defaulting to 1.8 m above ground level (human height).")
	}

	if(inherits(links, "SpatialLines")) {
		link.segments <- Rcaline::extract_segments(links)
	} else if(is.data.frame(links)) {
		link.segments <- links
	} else {
		stop("links must be a SpatialLines* object or a data.frame giving coordinates for each segment. See ?CALINE3.predict for more.")
	}

	if(all(c("x0","y0","x1","y1") %in% names(link.segments))) {
		XL1 <- as.single(link.segments$x0)
		YL1 <- as.single(link.segments$y0)
		XL2 <- as.single(link.segments$x1)
		YL2 <- as.single(link.segments$y1)
	} else if(all(c("x","y","xend","yend") %in% names(link.segments))) {
		XL1 <- as.single(link.segments$x)
		YL1 <- as.single(link.segments$y)
		XL2 <- as.single(link.segments$xend)
		YL2 <- as.single(link.segments$yend)
	} else {
		stop("Link coordinates must be specified. Please supply $x0, $x1, $y0, $y1 or $x, $y, $xend, $yend attributes to specify start and end coordinates. Units should be in meters. See ?as.segments and/or ?CALINE3.predict for more.")
	}

	if(is.null(links$classification)) {
		warning("links$classification is missing. Defaulting to 'AG' (at grade). See ?CALINE3.predict for more.")
		TYP <- as.character(rep('AG', nrow(links)), 2)
	} else {
		TYP <- as.character(links$classification, 2)
	}

	if(is.null(links$height)) {
		warning('links$height is missing. Defaulting to 0.0 m above ground level. See ?CALINE3.predict for more.')
		HL <- as.single(rep(0.0, nrow(links)))
	} else {
		HL <- as.single(links$height)
	}

	if(is.null(links$width)) {
		stop("links$width is missing. Please supply a $width attribute (in meters).")
	}
	WL <- as.single(links$width)

	if(is.null(links$flow)) {
		stop("links$flow is missing. Please supply a $flow attribute (in vehicles per hour).")
	}
	VPHL <- as.single(links$flow)

	if(is.null(links$emissions)) {
		stop("links$emissions is missing. Please supply an $emissions attribute (in g/veh-mi).")
	}
	EFL <- as.single(links$emissions)

	# Meteorology specifications
	if(is.null(meteorology$mixing.height)) {
		mixing.heights <- match.arg(mixing.heights, c("urban", "rural"))
		if(mixing.heights == "urban") {
			MIXHM <- as.single(meteorology$urban.mixing.height)
		} else if(mixing.heights == "rural") {
			MIXHM <- as.single(meteorology$rural.mixing.height)
		} else {
			stop("mixing.heights must be either 'urban' or 'rural'.")
		}
	} else {
		MIXHM <- as.single(meteorology$mixing.height)
	}
	if(any(is.null(meteorology$wind.speed),
	       is.null(meteorology$wind.bearing),
	       is.null(meteorology$stability.class),
	       is.null(MIXHM))) {
		stop("meteorology must include $wind.speed, $wind.bearing, $stability.class, and $mixing.height. Please see ?read.ISC for more.")
	}
	UM <- as.single(meteorology$wind.speed)
	BRGM <- as.single(meteorology$wind.bearing)
	CLASM <- as.integer(meteorology$stability.class)

	# Model parameters
	if(missing(surface.roughness)) {
		stop("A value for surface roughness length (Z0) is required. See ?CALINE3.predict for more.")
	}
	ATIM 	<- as.single(averaging.time)
	Z0 		<- as.single(surface.roughness)
	VS 		<- as.single(settling.velocity)
	VD 		<- as.single(deposition.velocity)

	C <- CALINE3_RECEPTOR_TOTALS(
		XR, YR, ZR,
		XL1, YL1, XL2, YL2, WL, HL, TYP, VPHL, EFL,
		UM, BRGM, CLASM, MIXHM,
		ATIM, Z0, VS, VD
  )

	# Use row names, if provided, to label results
	dimnames(C) <- list(row.names(receptors), row.names(meteorology))
	return(C)

}
