source("scripts/99_utils.R")

# KS data comes as separate polylines for each contour (no lake-level labels)
#   higher numbers represent deeper depths

# TODO load lagosus gis
# mimick workflow of the MI dataset

# ---- get-raw-data ----
if(!file.exists("data/ks_bathy/ks_bathy.gpkg")){
  dir.create("data/ks_bathy", showWarnings = FALSE)
  library(esri2sf) # install_github("yonghah/esri2sf")
  base_url <- "http://kars.ku.edu/arcgis/rest/services/WaterResources/BathymetryContour/MapServer/"

  # contours <- st_read("data/ks_bathy/ks_bathy.gpkg", layer = "contours")
  contours <- esri2sf(paste0(base_url, "0"))
  sf::st_write(contours, "data/ks_bathy/ks_bathy.gpkg", "contours")
}
# contours <- st_read("data/ks_bathy/ks_bathy.gpkg", layer = "contours")
ps       <- st_read("data/ks_bathy/ks_bathy.gpkg",
                    layer = "contours")
