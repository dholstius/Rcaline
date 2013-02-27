\name{CALINE3}
\alias{CALINE3}
\title{CALINE3}
\usage{
  CALINE3(XR, YR, ZR, XL1, YL1, XL2, YL2, WL, HL, NTYP,
    VPHL, EFL, UM, BRGM, CLASM, MIXHM, ATIM, Z0, VS, VD)
}
\arguments{
  \item{XR}{x-coordinates of the receptors}

  \item{YR}{y-coordinates of the receptors}

  \item{ZR}{z-coordinates of the receptors (above ground
  level)}

  \item{XL1}{starting x-coordinates of the links}

  \item{YL1}{starting y-coordinates of the links}

  \item{XL2}{ending x-coordinates of the links}

  \item{YL2}{ending y-coordinates of the links}

  \item{WL}{widths of the links}

  \item{HL}{heights of the links (above ground level)}

  \item{NTYP}{link classifications}

  \item{VPHL}{link-level traffic volumes, in vehicles per
  hour}

  \item{EFL}{link-level emission factors, in grams per
  vehicle-mile [per hour]}

  \item{UM}{wind speeds, in meters per second (<1.0 results
  in NA)}

  \item{BRGM}{wind bearings, in degrees (direction wind is
  blowing from)}

  \item{CLASM}{stability classes (1, 2, 3, 4, 5, or 6)}

  \item{MIXHM}{mixing heights}

  \item{ATIM}{averaging time, in minutes}

  \item{Z0}{surface roughness, in centimeters}
}
\value{
  matrix of concentrations, in ug/m3
}
\description{
  Wrapper for single-precision Fortran routine.
}
\details{
  All coordinates are in meters unless otherwise specified.
}

