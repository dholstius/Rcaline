#' Construct a Pollutant object.
#'
#' Use to override the default pollutant in \code{\link{Caline3Model}}.
#'
#' @param name TODO
#' @param molecularWeight TODO
#' @param settlingVelocity TODO
#' @param depositionVelocity TODO
#' @return a Pollutant object
#' @keywords pollutant model
#' @export
Pollutant <- function(name, molecularWeight, settlingVelocity=0.0, depositionVelocity=0.0) {
      obj <- list(
            name = name,
            molecularWeight = molecularWeight,
            settlingVelocity = settlingVelocity,
            depositionVelocity = depositionVelocity
      )
      class(obj) <- "Pollutant"
      return(obj)
}

CO <- Pollutant("CO", molecularWeight=28.0)
PM <- Pollutant("PM25", molecularWeight=NA)