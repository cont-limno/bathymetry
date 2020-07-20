# To be called from the `data` folder
# setwd("data")

library(sf)
library(raster)
suppressMessages(library(gdalUtilities))
suppressMessages(library(dplyr))

# ---- bathymetry.zip ----
# each bathymetry surface is linked by name to a LAGOSUS ID based on NHD (USGS 2019) waterbodies.

# generate raster list
depth_dirs <- dir(include.dirs = TRUE, pattern = "^\\w{2}_bathy$",
                  full.names = TRUE)
f_list <- dir(depth_dirs, pattern = "^\\d{1,6}.tif",
              full.names = TRUE)

# turn on cog (cloud optimized geotiff)
# r_test_path <- "ct_bathy/101661.tif"
# r_test_path2 <- "ct_bathy/106820.tif"
# system("gdal_translate ct_bathy/101661.tif test.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW")
# seems like tifs are already cloud optimized?

# zip files
# zip("bathymetry.zip", c(f_list))

# ---- bathymetry_index.zip ----
# contains a vector layer with information on the filename, coverage polygon,
# projection, raw data source file, and raw data source url of each bathymetry
# surface.

# setup source data by state, url, and raw data file
f_source <- dplyr::bind_rows(
   c("state" = "ct", data = "ct_bathy/ct_bathy.gpkg", url = "https://cteco.uconn.edu/ctmaps/rest/services/Elevation/Lake_Bathymetry/MapServer/"),
   c("state" = "ia", data = "ia_bathy/lakes_bathymetry.shp", url = "http://iowageodata.s3.amazonaws.com/inlandWaters/lakes_bathymetry.zip"),
   c("state" = "ks", data = "ks_bathy/ks_bathy.gpkg", url = "http://kars.ku.edu/arcgis/rest/services/WaterResources/BathymetryContour/MapServer/"),
   c("state" = "ma", data = "ma_bathy/DFWBATHY_ARC.shp", url = "http://download.massgis.digital.mass.gov/shapefiles/state/dfwbathy.zip"),
   c("state" = "me", data = "me_bathy/lakedpth.shp", url = "https://www.maine.gov/megis/catalog/shps/state/lakedpths.zip"),
   c("state" = "mi", data = "mi_bathy/contours.geojson", url = "https://opendata.arcgis.com/datasets/d49160d2e5af4123b15d48c2e9c70160_4.geojson"),
   c("state" = "mn", data = "mn_bathy/lake_bathymetric_elevation_model.tif", url = "https://gisdata.mn.gov/dataset/water-lake-bathymetry"),
   c("state" = "ne", data = "ne_bathy/ne_bathy.gpkg", url = "https://maps.outdoornebraska.gov/arcgis/rest/services/Programs/LakeMapping/MapServer"),
   c("state" = "nh", data = "nh_bathy/Bathymetry_Lakes_lines.shp", url = "http://www.granit.unh.edu/cgi-bin/nhsearch?dset=bathymetry_lakes_polygons/nh")
)

# create geopackage index, filename, coverage polygon, and crs
ids            <- data.frame(llid = stringr::str_extract(f_list, "[0-9]+"),
                             stringsAsFactors = FALSE)
states         <- stringr::str_extract(f_list, "(?<=\\/).{2}")

get_projection <- function(x) projection(raster(x))
# get_projection(f_list[[1]])
coverage_crs   <- as.character(do.call("rbind",
           lapply(f_list, function(x) get_projection(x))
           ))

get_poly <- function(i){
   st_transform(
      st_as_sfc(st_bbox(raster(f_list[[i]]))),
      crs = coverage_crs[[i]], 5071)
}
# mapview::mapview(get_poly(1))
coverage_polys <- lapply(seq_along(coverage_crs),
                         function(i) get_poly(i)) %>%
   Reduce(c, .) %>%
   st_as_sf(ids, geometry = .) %>%
   mutate(crs = coverage_crs,
          file = f_list,
          state = states)
coverage_polys <- left_join(coverage_polys, f_source, by = "state")

# unlink("bathymetry.gpkg")
sf::st_write(coverage_polys, "bathymetry.gpkg")
# coverage_polys <- st_read("bathymetry.gpkg")

# ---- collate raw data ----
## pull paths for data in shp format
shp_files <- as.character(unlist(sapply(
   substring(f_source[grep(".shp", f_source$data),]$data, 0, 8),
       function(x) dir(x, pattern = c(".shp|.dbf|.CPG|.cpg|.prj|.sbn|.sbx|.shx"),
    include.dirs = TRUE, full.names = TRUE))))
shp_files <- shp_files[!(shp_files %in% f_source$data)]

# ---- hypsography.csv ----
file.copy("00_hypso/hypso.csv", "hypsography.csv")

# ---- zip files ----
unlink("bathymetry.zip")
zip("bathymetry.zip", c(f_list, f_source$data, shp_files, "bathymetry.gpkg", "hypsography.csv"))
unlink("bathymetry.gpkg")
# system("fsizemb bathymetry.zip")
