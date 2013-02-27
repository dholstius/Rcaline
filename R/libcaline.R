#' CALINE3
#'
#' Wrapper for single-precision Fortran routine.
#'
#' All coordinates are in meters unless otherwise specified.
#' 
#' @param XR x-coordinates of the receptors
#' @param YR y-coordinates of the receptors
#' @param ZR z-coordinates of the receptors (above ground level)
#' @param XL1 starting x-coordinates of the links
#' @param YL1 starting y-coordinates of the links
#' @param XL2 ending x-coordinates of the links
#' @param YL2 ending y-coordinates of the links
#' @param WL widths of the links
#' @param HL heights of the links (above ground level)
#' @param NTYP link classifications
#' @param VPHL link-level traffic volumes, in vehicles per hour
#' @param EFL link-level emission factors, in grams per vehicle-mile [per hour]
#' @param UM wind speeds, in meters per second (<1.0 results in NA)
#' @param BRGM wind bearings, in degrees (direction wind is blowing from)
#' @param CLASM stability classes (1, 2, 3, 4, 5, or 6)
#' @param MIXHM mixing heights
#' @param ATIM averaging time, in minutes
#' @param Z0 surface roughness, in centimeters
#' 
#' @return matrix of concentrations, in ug/m3
#'
#' @export
CALINE3 <- function(
	XR, YR, ZR,
	XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
	UM, BRGM, CLASM, MIXHM,
	ATIM, Z0, VS, VD
) {	
	
	# FIXME: check that all arrays are of the correct length
	NR <- length(XR)
	NL <- length(XL1)
	NM <- length(UM)
	
	# Allocate array C to hold results
	array.shape <- c(NR, NM)
	C <- as.single(array(-1, dim=array.shape))
	
	# Call native code
	returned_data <- .Fortran(
	 	'CALINE3_MATRIX_SINGLE', 
	 	as.integer(NR), 
	 	as.single(XR), 
	 	as.single(YR), 
	 	as.single(ZR),
	 	as.integer(NL), 
	 	as.single(XL1), 
	 	as.single(YL1), 
	 	as.single(XL2), 
	 	as.single(YL2), 
	 	as.single(WL), 
	 	as.single(HL), 
	 	as.integer(NTYP), 
	 	as.single(VPHL), 
	 	as.single(EFL),
	 	as.integer(NM), 
	 	as.single(UM), 
	 	as.single(BRGM), 
	 	as.integer(CLASM), 
	 	as.single(MIXHM),
	 	as.single(ATIM), 
	 	as.single(Z0), 
	 	as.single(VS), 
	 	as.single(VD),
	 	C = C,
	 	PACKAGE = "Rcaline"
	)
	
	# Reshape results, substituting NA for missing values
      C <- with(returned_data, replace(C, which(C < 0), NA))
	C <- matrix(C, ncol = NM, nrow = NR)

	return(C)
	
}