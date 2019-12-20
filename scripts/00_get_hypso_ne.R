# https://maps.outdoornebraska.gov/arcgis/rest/services/Programs/LakeMapping/MapServer

if(!file.exists("data/ne_bathy/ne_bathy.gpkg")){
  base_url <- "https://maps.outdoornebraska.gov/arcgis/rest/services/Programs/LakeMapping/MapServer/"
  contours <- esri2sf(paste0(base_url, "1"))

  sf::st_write(contours, "data/ne_bathy/ne_bathy.gpkg", "contours")
}

contours <- st_read("data/ne_bathy/ne_bathy.gpkg", layer = "contours")

test <- dplyr::filter(contours, Name == "Blue Lake")

