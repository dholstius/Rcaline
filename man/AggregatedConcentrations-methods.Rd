\name{as.AggregatedConcentrations.SpatialPointsDataFrame}
\alias{as.AggregatedConcentrations.SpatialPointsDataFrame}
\alias{ggplot.AggregatedConcentrations}
\alias{spplot.AggregatedConcentrations}
\title{Visualization}
\usage{
  as.AggregatedConcentrations.SpatialPointsDataFrame(from)

  spplot.AggregatedConcentrations(obj, ...)

  ggplot.AggregatedConcentrations(data, bounds, ...)
}
\arguments{
  \item{from}{an AggregatedConcentrations object}

  \item{x}{an AggregatedConcentrations object, obtained by
  calling aggregate() on the results of a modeling run}

  \item{...}{further arguments to spplot}
}
\value{
  a SpatialPointsDataFrame
}
\description{
  Plot aggregate concentrations using spplot or ggplot2.

  Visualization
}
\keyword{ggplot2}
\keyword{spplot}

