\name{ReceptorGrid}
\alias{ReceptorGrid}
\alias{ReceptorRings}
\title{Receptor grids}
\usage{
  ReceptorGrid(links, z = 1.8, resolution = 1000,
    maxDistance = 1000, rgeos.scale = 1e+06)

  ReceptorRings(links, z = 1.8,
    distances = c(50, 100, 250, 500, 1000),
    spacing = identity, rgeos.scale = 1e+06)
}
\arguments{
  \item{links}{a \link{FreeFlowLinks} object}

  \item{z}{elevation in meters}

  \item{resolution}{spacing between receptors, in meters}

  \item{maxDistance}{buffer radius, in meters}

  \item{rgeos.scale}{can be adjusted if precision warnings
  occur}

  \item{links}{a \link{FreeFlowLinks} object}

  \item{z}{elevation in meters}

  \item{distances}{list of distances to the roadway
  centerline, in meters}

  \item{spacing}{TODO}

  \item{rgeos.scale}{TODO}
}
\value{
  SpatialPointsDataFrame

  SpatialPointsDataFrame
}
\description{
  Construct a set of receptors, using a
  \link{FreeFlowLinks} as the basis.

  ReceptorRings constructs concentric rings of receptors at
  specific distances from \code{links}.
}
\details{
  ReceptorGrid constructs a regular Cartesian grid of
  receptors no more than \code{maxDistance} from
  \code{links}.
}
\seealso{
  ReceptorRings
}
\keyword{receptors}

