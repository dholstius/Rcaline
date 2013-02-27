\name{Meteorology}
\alias{Meteorology}
\alias{Meteorology.character}
\alias{Meteorology.data.frame}
\alias{Meteorology.ISCFile}
\title{Meteorology}
\usage{
  Meteorology(x, ...)

  Meteorology.data.frame(x)

  Meteorology.character(x)

  Meteorology.ISCFile(x, use = c("urban", "rural"))
}
\arguments{
  \item{x}{a filename, \link{ISCFile} object, or
  data.frame}

  \item{use}{whether to use urban or rural mixing heights}
}
\value{
  a Meteorology object (essentially a \link{data.frame})
}
\description{
  Construct a Meteorology object, generally from a file
  containing ISC-formatted records.
}
\keyword{meteorology}

