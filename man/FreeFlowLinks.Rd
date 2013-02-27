\name{FreeFlowLinks}
\alias{FreeFlowLinks}
\title{FreeFlowLinks}
\usage{
  FreeFlowLinks(spobj, vehiclesPerHour, emissionFactor,
    width, height = 0, classification = LinkType(0), ...)
}
\arguments{
  \item{spobj}{a SpatialLinesDataFrame object}

  \item{vehiclesPerHour}{average number of vehicles per
  hour (also known as "flow")}

  \item{emissionFactor}{emissions per vehicle, in grams per
  mile (per hour)}

  \item{width}{mixing zone width, in meters}

  \item{height}{elevation above ground level (not sea
  level!)}

  \item{classification}{see \code{\link{LinkType}} for
  possible values}

  \item{...}{other arguments}
}
\value{
  a FreeFlowLinks object
}
\description{
  Construct a FreeFlowLinks object from a
  SpatialLinesDataFrame.
}
\details{
  \code{vehiclesPerHour}, \code{emissionFactor}, and other
  arguments can all be specified as expressions. Use them
  the same way you would use arguments to
  \code{\link{transform}}. A FreeFlowLinks object can be
  plotted with \code{\link{plot}} or coerced to a
  \code{data.frame} with \code{\link{as.data.frame}}.
}
\keyword{links}

