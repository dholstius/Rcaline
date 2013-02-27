#' FreeFlowLinks accessors
#'
#' Inspect attributes of a FreeFlowLinks object.
#'
#' @param obj a FreeFlowLinks object
#' @param ... further arguments
#' @rdname FreeFlowLinks-accessors
#' @export
centerlines <- function(obj, ...) UseMethod("centerlines")

#' FreeFlowLinks accessors
#'
#' @S3method centerlines FreeFlowLinks
#' @rdname FreeFlowLinks-accessors
#' @export
centerlines.FreeFlowLinks <- function(obj, ...) obj$centerlines