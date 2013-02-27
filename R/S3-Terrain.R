#' Construct a Terrain object.
#'
#' @param surfaceRoughness
#' @return a Terrain object
#' @keywords terrain model
#' @export
Terrain <- function(surfaceRoughness) {
      obj <- list(
            surfaceRoughness = surfaceRoughness
      )
      class(obj) <- "Terrain"
      return(obj)
}