\name{as.data.frame.FreeFlowLinks}
\alias{as.data.frame.FreeFlowLinks}
\title{as.data.frame}
\usage{
  as.data.frame.FreeFlowLinks(x, row.names, optional, ...)
}
\arguments{
  \item{x}{a FreeFlowLinks object}

  \item{row.names}{TODO}

  \item{optional}{TODO}

  \item{...}{other arguments}
}
\value{
  a data.frame
}
\description{
  Convert a FreeFlowLinks object to a data.frame.
}
\details{
  Each row in the data.frame corresponds to one or more
  segments from the original (polyline) geometry. The
  starting coordinates (XL1, YL1) and ending coordinates
  (XL2, YL2) are preserved as columns in the data.frame.
  Attributes are also preserved as columns.
}
\keyword{links}

