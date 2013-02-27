\name{Caline3Model}
\alias{Caline3Model}
\title{Modeling}
\usage{
  Caline3Model(links, meteorology, receptors, terrain,
    pollutant)
}
\arguments{
  \item{links}{a \code{\link{FreeFlowLinks}} object}

  \item{meteorology}{a \code{\link{Meteorology}} object}

  \item{receptors}{a \code{\link{Receptors}} object}

  \item{terrain}{a \code{\link{Terrain}} object}

  \item{pollutant}{a \code{\link{Pollutant}} object}
}
\value{
  a Caline3Model object
}
\description{
  Construct, run, summarize and show results from a
  Caline3Model object.
}
\details{
  The model (object) contains all of the link and met data,
  as well as site-specific model parameters, such as
  surface roughness. Internally, these are stored as
  single-precision arrays. If you want to experiment by
  changing a parameter, construct a new Caline3Model,
  unless you really know what you are doing.

  Use \code{predict} to predict concentrations;
  \code{aggregate} to compute summary statistics, such as
  means and maxima; and \code{spplot} or \code{ggplot} to
  visualize results.
}
\keyword{aggregate}
\keyword{ggplot2}
\keyword{model}
\keyword{predict}
\keyword{spplot}

