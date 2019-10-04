library(sf)

(p1 = st_point(c(1,2)))

test <- lapply(1:40, function(x) st_buffer(p1, x))
test <- st_multipolygon(test)
plot(test)
