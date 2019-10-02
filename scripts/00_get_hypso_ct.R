source("scripts/99_utils.R")

# ---- get-raw-data ----
if(!file.exists("data/ct_bathy/ct_bathy.gpkg")){
  library(esri2sf) # install_github("yonghah/esri2sf")
  url <- "https://cteco.uconn.edu/ctmaps/rest/services/Elevation/Lake_Bathymetry/MapServer/0"
  contours <- esri2sf(url)
  sf::st_write(contours, "data/ct_bathy/ct_bathy.gpkg")
}
contours <- st_read("data/ct_bathy/ct_bathy.gpkg")

test_name <- "Colebrook River Reservoir"
dt <- dplyr::filter(contours, WBNAME == test_name)

# create points from contour lines
# use gstat to interpolate between points
# clip raster to lake boundary





