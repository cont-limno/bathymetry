library(sf)

(p1 = st_point(c(1,2)))

test <- lapply(1:40, function(x) st_buffer(p1, x))
test <- st_multipolygon(test)
test2 <- st_sfc(test)
test2 <- st_cast(test, "POLYGON")
plot(test2)

r <- raster::rasterize(as_Spatial(st_sfc(test)))

# https://stackoverflow.com/questions/11945604/3d-cone-and-cylinder-in-r
