require(Rcaline)
require(testthat)
context("CALINE3-examples")

args <- list(
      XR=30.0, YR=0.0, ZR=1.8,
      XL1=0.0, YL1=-5000.0, XL2=0.0, YL2=5000.0, 
      WL=30.0, HL=0.0, NTYP=0, VPHL=7500.0, EFL=30.0,
      UM=1.0, BRGM=270.0, CLASM=6, MIXHM=1000.0,
      ATIM=60.0, Z0=10.0, VS=0.0, VD=0.0)

f <- Rcaline:::CALINE3
C.ugm3 <- do.call('f', args)
C.ppm <- C.ugm3 * 0.0245 / 28.0
expect_equal(4.6, round(C.ppm[1,1], digits=1))