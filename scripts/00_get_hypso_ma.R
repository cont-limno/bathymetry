# https://docs.digital.mass.gov/dataset/massgis-data-inland-water-bathymetry-110000
# http://download.massgis.digital.mass.gov/shapefiles/state/dfwbathy.zip

# MA data comes as contours with lake-level labels
#   higher numbers represent deeper depths
#   units in ft

dir.create("data/ma_bathy", showWarnings = FALSE)

if(!file.exists("data/ma_bathy/ma_bathy.gpkg")){
  base_url <- "http://download.massgis.digital.mass.gov/shapefiles/state/dfwbathy.zip"
  download.file(base_url, "data/ma_bathy/dfwbathy.zip")
  unzip("data/ma_bathy/dfwbathy.zip", exdir = "data/ma_bathy/")
}

contours <- sf::st_read("data/ma_bathy/DFWBATHY_ARC.shp")

mapview::mapview(dplyr::filter(contours, NAME == "Agawam Mill Pond"))


