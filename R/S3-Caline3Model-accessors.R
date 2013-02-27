#' Caline3Model accessors
#'
#' Inspect model elements from a Caline3Model.
#'
#' @param model a Caline3Model object
#' @param ... further arguments
#' @rdname Caline3Model-accessors
#' @export
links <- function(model, ...) UseMethod("links")

#' Caline3Model accessors
#'
#' @S3method links default
#' @rdname Caline3Model-accessors
#' @export
links.default <- function(model, ...) model$links

#' Caline3Model accessors
#'
#' @rdname Caline3Model-accessors
#' @export
receptors <- function(model, ...) UseMethod("receptors")

#' Caline3Model accessors
#'
#' @S3method receptors default
#' @rdname Caline3Model-accessors
#' @export
receptors.default <- function(model, ...) {
	rcp <- model$receptors
	stopifnot(identical(coordnames(rcp), c('x', 'y', 'z')))
	return(rcp)
}

#' Caline3Model accessors
#'
#' @rdname Caline3Model-accessors
#' @export
meteorology <- function(model, ...) UseMethod("meteorology")

#' Caline3Model accessors
#'
#' @S3method meteorology default
#' @rdname Caline3Model-accessors
#' @export
meteorology.default <- function(model, ...) model$meteorology

#' Caline3Model accessors
#'
#' @rdname Caline3Model-accessors
#' @export
pollutant <- function(model, ...) UseMethod("pollutant")

#' Caline3Model accessors
#'
#' @S3method pollutant default
#' @rdname Caline3Model-accessors
#' @export
pollutant.default <- function(model, ...) model$pollutant

#' Caline3Model accessors
#'
#' @rdname Caline3Model-accessors
#' @export
terrain <- function(model, ...) UseMethod("terrain")

#' Caline3Model Methods
#'
#' @S3method terrain default
#' @rdname Caline3Model-accessors
#' @export
terrain.default <- function(model, ...) model$terrain