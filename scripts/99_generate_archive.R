library(sf)
library(raster)
library(gdalUtilities)
library(dplyr)

# ---- generate raster list ----
depth_dirs <- dir("data",
    include.dirs = TRUE, pattern = "^\\w{2}_bathy$",
    full.names = TRUE)
f_list <- dir(depth_dirs, pattern = "^\\d{1,6}.tif",
              full.names = TRUE)

# ---- setup source data by state, url, and raw data file ----
 (f_source <- dplyr::bind_rows(
   c("state" = "ct", data = "data/ct_bathy/ct_bathy.gpkg", url = "https://cteco.uconn.edu/ctmaps/rest/services/Elevation/Lake_Bathymetry/MapServer/"),
   c("state" = "ia", data = "data/ia_bathy/lakes_bathymetry.shp", url = "http://iowageodata.s3.amazonaws.com/inlandWaters/lakes_bathymetry.zip"),
   c("state" = "ks", data = "data/ks_bathy/ks_bathy.gpkg", url = "http://kars.ku.edu/arcgis/rest/services/WaterResources/BathymetryContour/MapServer/"),
   c("state" = "ma", data = "data/ma_bathy/DFWBATHY_ARC.shp", url = "http://download.massgis.digital.mass.gov/shapefiles/state/dfwbathy.zip"),
   c("state" = "me", data = "data/me_bathy/lakedpth.shp", url = "https://www.maine.gov/megis/catalog/shps/state/lakedpths.zip"),
   c("state" = "mi", data = "data/mi_bathy/contours.geojson", url = "https://opendata.arcgis.com/datasets/d49160d2e5af4123b15d48c2e9c70160_4.geojson"),
   c("state" = "mn", data = "data/mn_bathy/lake_bathymetric_elevation_model.tif", url = "https://gisdata.mn.gov/dataset/water-lake-bathymetry"),
   c("state" = "ne", data = "data/ne_bathy/ne_bathy.gpkg", url = "https://maps.outdoornebraska.gov/arcgis/rest/services/Programs/LakeMapping/MapServer"),
   c("state" = "nh", data = "data/nh_bathy/Bathymetry_Lakes_lines.shp", url = "http://www.granit.unh.edu/cgi-bin/nhsearch?dset=bathymetry_lakes_polygons/nh")
   ))

f_data <- dplyr::filter(f_source, state != "mn") %>% # mn data is too large
   pull(data)

# ---- turn on cog (cloud optimized geotiff) ----
# r_test_path <- "data/ct_bathy/101661.tif"
# r_test_path2 <- "data/ct_bathy/106820.tif"
# system("gdal_translate data/ct_bathy/101661.tif test.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW")
# seems like tifs are already cloud optimized?

# ---- create geopackage index, filename, coverage polygon, and crs ----

ids            <- data.frame(llid = stringr::str_extract(f_list, "[0-9]+"),
                             stringsAsFactors = FALSE)
states         <- stringr::str_extract(f_list, "(?<=data\\/).{2}")
coverage_crs   <- as.character(do.call("rbind",
                        lapply(f_list, function(x) st_crs(raster(x))$input)))
coverage_polys <- lapply(seq_along(coverage_crs),
       function(i) st_transform(st_as_sfc(st_bbox(raster(f_list[i])),
                                          crs = coverage_crs[i]), 5071)) %>%
  Reduce(c, .) %>%
  st_as_sf(ids, geometry = .) %>%
  mutate(crs = coverage_crs,
         file = f_list,
         state = states)

coverage_polys <- left_join(coverage_polys, f_source, by = "state")

# plot(coverage_polys$geometry)
unlink("data/bathymetry.gpkg")
sf::st_write(coverage_polys, "data/bathymetry.gpkg")
# coverage_polys <- st_read("data/bathymetry.gpkg")

# ---- zip files ----
zip("data/bathymetry.zip", c("data/README.md", "data/bathymetry.gpkg", "data/00_hypso/hypso.csv",
                             f_list, f_data))
