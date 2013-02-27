\name{Receptors}
\alias{Receptors}
\alias{Receptors.matrix}
\alias{Receptors.SpatialPoints}
\alias{Receptors.SpatialPointsDataFrame}
\title{Receptor locations}
\usage{
  Receptors(obj, ...)

  Receptors.SpatialPointsDataFrame(obj, z = 1.8,
    check.projection = TRUE)

  Receptors.SpatialPoints(obj, z = 1.8,
    check.projection = TRUE)

  Receptors.matrix(obj, z = 1.8)
}
\arguments{
  \item{obj}{a SpatialPoints or SpatialPointsDataFrame
  object, or a two-dimensional matrix of coordinates}

  \item{z}{elevation(s) above ground, in meters}

  \item{check.projection}{checks to see that obj has an
  associated projection}
}
\value{
  SpatialPoints* object
}
\description{
  Construct a set of receptors. All coordinates must be in
  meters!
}
\seealso{
  ReceptorGrid ReceptorRings
}
\keyword{receptors}

