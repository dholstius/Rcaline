\name{ggplot.FreeFlowLinks}
\alias{ggplot.FreeFlowLinks}
\title{ggplot}
\usage{
  ggplot.FreeFlowLinks(x, ...)
}
\arguments{
  \item{x}{a FreeFlowLinks object}

  \item{...}{other arguments}
}
\description{
  Plot a FreeFlowLinks object using \code{ggplot2}.
}
\details{
  Roadways are colored by the total emissions strength, Q
  (g/mi), the product of flow \code{vehiclesPerHour} and
  \code{emissionFactor}.

  Prettier but slower than \code{\link{lines}}.
}
\keyword{links}

