library(rgdal)
library(rgeos)

UTM10 <- CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
SF_county <- spTransform(readOGR(file.path("data-raw", "BayArea", "Counties"), layer = "SF_county"), UTM10)
BayArea_highways <- spTransform(readOGR(file.path("data-raw", "BayArea", "Roadways"), layer = "STRte_BAAQMD_v2"), UTM10)

i <- which(gIntersects(gBuffer(SF_county, width = 2000), BayArea_highways, byid = TRUE))
SF_highways <- BayArea_highways[i,]

save(SF_county, SF_highways, file = file.path("data", "SanFrancisco.rda"), compress = "xz")
