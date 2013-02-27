#
# Public API
#

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
		link.segments <- Rcaline::as.segments(links)
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

	predicted <- .caline3.receptor_totals(
		XR, YR, ZR,
		XL1, YL1, XL2, YL2, WL, HL, TYP, VPHL, EFL,
		UM, BRGM, CLASM, MIXHM,
		ATIM, Z0, VS, VD)

	# Use row names, if provided, to label results
	dimnames(predicted) <- list(row.names(receptors), row.names(meteorology))
	
	return(predicted)
	
}

#
# Native code (Fortran) wrapper function
#

.caline3.receptor_totals <- function(
	XR, YR, ZR,
	XL1, YL1, XL2, YL2, WL, HL, TYP, VPHL, EFL,
	UM, BRGM, CLASM, MIXHM,
	ATIM, Z0, VS, VD
) {	
	
	#message("Entering .caline3.receptor_totals()")
	#.call <- match.call()
	#print(.call)
		
	# Receptor specifications
	NR <- as.integer(length(XR))
	stopifnot( all.equal(NR, length(YR), length(ZR)))
	if(any(is.na(c(XR,YR,ZR))))
		stop("Receptor coordinates cannot include NA values.")

	# Link specifications
	NL <- as.integer(length(XL1))
	if(any(is.na(c(XL1,YL1,XL2,YL2))))
		stop("Link coordinates cannot include NA values.")
	if(any(is.na(WL)))
		stop("Link widths cannot include NA values.")
	if(any(is.na(HL)))
		stop("Link heights cannot include NA values.")
	if(any(is.na(TYP)))
		stop("Link classifications cannot include NA values.")
	if(any(is.na(VPHL)))
		stop("Link flows cannot include NA values.") 
	if(any(is.na(EFL)))
		stop("Link emission factors cannot include NA values.")
	
	# Meteorology specifications.
	NM <- as.integer(length(UM))
	stopifnot( all.equal(NM, length(BRGM), length(CLASM), length(MIXHM)))
	if(any(is.na(UM)))
		stop("Wind speeds cannot include NA values. See ?read.ISC for more.")
	if(any(is.na(BRGM)) || any(BRGM < 0) || any(BRGM > 360))
		stop("Wind bearings must be between 0 and 360 degrees, and cannot include NA values. See ?read.ISC for more.")
	if(any(is.na(CLASM)))
		stop("Stability classes must not include NA values. See ?read.ISC for more.")
	if(any(CLASM > 6)) {
		CLASM <- pmin(CLASM, 6)
		warning("Rounding down stability class 7 to class 6.")
	}
	if(any(is.na(MIXHM)) || any(MIXHM < 0))
		stop("Mixing heights must not be negative, and cannot include NA values.")
		
	# Convert type classifications to integers.
	# (because you can't pass characters to .Fortran() with DUP = FALSE )
	if(is.character(TYP)) {
		clas.lookup <- list(AG=0, BR=1, FL=2, DP=3, 
			`At Grade`=0, `Bridge`=1, `Fill`=2, `Depressed`=3)
		NTYP <- as.integer(clas.lookup[TYP])
	} else if(is.integer(TYP)) {
		NTYP <- TYP
	} else {
		stop('TYP argument must be character or integer')
	}
	
	# TODO: more rigorous type checking?
	stopifnot(lapply(list(XR, YR, ZR), is.numeric) == TRUE)
	stopifnot(lapply(list(XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL), is.numeric) == TRUE)
	stopifnot(lapply(list(UM, BRGM, CLASM, MIXHM), is.numeric) == TRUE)
	
	stopifnot(all.equal(NL, length(YL1), length(XL2), length(YL2),
		length(WL), length(HL), length(NTYP), length(VPHL), length(EFL)))
		
	# for(varname in c("NR", "XR", "YR", "ZR", "NL", "XL1", "YL1", "XL2", "YL2", "WL", "HL", "NTYP", "VPHL", "EFL", "NM", "UM", "BRGM", "CLASM", "MIXHM", "ATIM", "Z0", "VS", "VD")) {
	# 		value <- get(varname)
	# 		message(varname, " is ", value)
	# 	}
	
	# Call native code, allocating array C to hold results
	array.shape <- c(NR, NM)
	returned_data <- .Fortran(
	 	"CALINE3_MATRIX_SINGLE", 
	 	NR, XR, YR, ZR,
	 	NL, XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
	 	NM, UM, BRGM, CLASM, MIXHM,
	 	ATIM, Z0, VS, VD,
	 	C = as.single(array(-1, dim=array.shape)),
	 	PACKAGE = "Rcaline"
	)
	
	# Reshape results, substituting NA for missing values
	predicted <- matrix(
		with(returned_data, replace(C, which(C < 0), NA)),
		ncol = NM, 
		nrow = NR)

	# Label with appropriate units.
	# NOTE: CALINE3 defaults to reporting values in 'ppm CO'.
	#       Rcaline:::libcaline3.f does not multiply by FPPM, so values are in ug / m3.
	attr(predicted, 'units') <- 'ug/m3'
	
	return(predicted)
	
}
