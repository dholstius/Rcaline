\name{aggregate.HourlyConcentrations}
\alias{aggregate.HourlyConcentrations}
\title{Handling model results}
\usage{
  aggregate.HourlyConcentrations(x,
    FUN = list("min", "mean", "median", "GM", "max", "sd"),
    na.rm = T, ...)
}
\arguments{
  \item{x}{concentrations obtained from
  \code{\link{predict.Caline3Model}}}

  \item{FUN}{a list of summary functions to apply to each
  receptor location}

  \item{na.rm}{logical; passed to each summary function in
  turn}

  \item{...}{other arguments}
}
\value{
  matrix of summary statistics\
}
\description{
  Aggregate the "raw" result matrix obtained from
  \code{\link{predict.Caline3Model}}, summarizing the pred
  estimates for each receptor.
}
\details{
  Use \code{as(x, "SpatialPointsDataFrame")} to re-bind
  these summary statistics with the locations (and other
  attributes) of the rcp used in the prediction step.
}

