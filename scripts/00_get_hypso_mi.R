library(sf)

# https://gisago.mcgi.state.mi.us/arcgis/rest/services/OpenData/hydro/MapServer/4
download.file("https://opendata.arcgis.com/datasets/d49160d2e5af4123b15d48c2e9c70160_4.geojson",
              "data/mi_bathy/contours.geojson")

test <- st_read("data/mi_bathy/contours.geojson")
test2 <- test[c(1:2, 89:90),]

mapview::mapview(test2)


