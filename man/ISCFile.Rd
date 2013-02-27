\name{ISCFile}
\alias{ISCFile}
\title{ISCFile}
\usage{
  ISCFile(filename)
}
\arguments{
  \item{filename}{filename}
}
\value{
  an ISCFile object (essentially a \link{data.frame})
}
\description{
  Read records from a file in ISC format.
}
\details{
  NOTE: Rotates the "flow vector" (wind direction) column
  by 180 degrees to obtain the wind bearing.
}
\keyword{meteorology}

