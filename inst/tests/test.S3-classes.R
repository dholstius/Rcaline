context("S3-Classes")

data(SanFrancisco, package="Rcaline")

expect_that(STRte_BAAQMD_v2.shp, is_a('SpatialLinesDataFrame'))

lnk <- FreeFlowLinks(STRte_BAAQMD_v2.shp,
	vehiclesPerHour = TRVol2009 / 24, 
	emissionFactor = 1.0,
	width = 30.0)
expect_that(lnk, is_a('FreeFlowLinks'))
expect_that(centerlines(lnk), is_a('SpatialLines'))

# Conventional: Cartesian receptor grid
rcp <- ReceptorGrid(lnk, z=1.8, resolution=500)
expect_that(rcp, is_a('SpatialPoints'))
expect_equal(proj4string(rcp), proj4string(lnk))

# Better: construct rcp based on distance to roadway
rcp <- ReceptorRings(lnk, z=1.8, distances=c(200, 500, 1000))
expect_that(rcp, is_a('SpatialPoints'))
expect_equal(proj4string(rcp), proj4string(lnk))

# met_5801.isc is provided by package data
expect_message(met <- Meteorology(met_5801.isc, use='urban')[1:3,], '2 wind speeds less than 1.0 m/s')
	
# Semi-urban terrain
ter <- Terrain(surfaceRoughness = 80.0)

# Put them all together
mod <- Caline3Model(lnk, met, rcp, ter, pollutant = CO)
expect_equal(links(mod), lnk)
expect_equal(receptors(mod), rcp)
expect_equal(meteorology(mod), met)
expect_equal(terrain(mod), ter)
expect_equal(pollutant(mod), CO)

# # Compute predicted concentrations
pred <- predict(mod, units='ppm')