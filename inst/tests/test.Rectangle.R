require(Rcaline)
data(SanFrancisco, package='Rcaline')

bb <- structure(c(19000, 4188000, 28000, 4196000), 
	.Dim = c(2L, 2L), 
	.Dimnames = list(c("x", "y"), c("min", "max")))

rec <- Rectangle(bb)

poly <- as(rec, 'SpatialPolygons')
expect_equal(bbox(poly), bb)

dat <- as(rec, 'data.frame')
expect_equal(dat,
	structure(list(xmin = 19000, ymin = 4188000, xmax = 28000, ymax = 4196000), 
		.Names = c("xmin", "ymin", "xmax", "ymax"), 
		row.names = c(NA, -1L), 
		class = "data.frame"))

rec2 <- resize(rec, 0.5)
expect_equal(attr(rec, 'proj4string'), attr(rec2, 'proj4string'))