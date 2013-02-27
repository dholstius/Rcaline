.onLoad <- function(libname=NULL, pkgname="Rcaline") {
	try(library.dynam('Rcaline', pkgname, libname))
}
.onAttach <- function(libname, pkgname) {}
.onUnload <- function(libpath) library.dynam.unload('Rcaline', libpath)

#' Rcaline provides an interface to Fortran implementations of the CALINE model family. These steady-state, Gaussian dispersion models are used to predict aerosol concentrations downwind from mobile emission sources such as highway traffic.
#'
#' @name Rcaline
#' @docType package
#'
#' @title Rcaline
#' @author David Holstius \email{david.holstius@@berkeley.edu}
#'
#' @references 
#' Benson, P. (1979) CALINE-3: A versatile dispersion model for predicting air pollution levels near highways and urban streets. Federal Highway Authority report FHWA/CA/TL-79/23, California DOT, Sacramento (1979).
#'
#' @keywords package environmental modeling disperson CALINE CALINE3 traffic
#'
#' @importClassesFrom sp SpatialPoints SpatialPointsDataFrame SpatialLines SpatialLinesDataFrame SpatialPolygons SpatialPolygonsDataFrame
#'
NA
