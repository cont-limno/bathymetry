library(sf)
library(raster)
library(gdalUtilities)
library(dplyr)

# I'd geotiff them with lzw and tiling on (cog) and ship with a Geopackage
# index, filename and coverage polygon. Store the foreign crs as string if the
# rasters use different ones. This would be a breeze in and for R

# ---- generate raster list ----
depth_dirs <- dir("data",
    include.dirs = TRUE, pattern = "^\\w{2}_bathy$",
    full.names = TRUE)
f_list <- dir(depth_dirs, pattern = "^\\d{1,6}.tif",
              full.names = TRUE)

# ---- setup source data by state, url, and raw data file ----
 (f_source <- dplyr::bind_rows(
   c("state" = "ct", data = "data/ct_bathy/ct_bathy.gpkg", url = "https://cteco.uconn.edu/ctmaps/rest/services/Elevation/Lake_Bathymetry/MapServer/"),
   c("state" = "ia"),
   c("state" = "ks"),
   c("state" = "ma"),
   c("state" = "me"),
   c("state" = "mi"),
   c("state" = "mn"),
   c("state" = "ne"),
   c("state" = "nh")
   ))

# ---- turn on cog (cloud optimized geotiff) ----
# r_test_path <- "data/ct_bathy/101661.tif"
# r_test_path2 <- "data/ct_bathy/106820.tif"
# system("gdal_translate data/ct_bathy/101661.tif test.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW")
# seems like tifs are already cloud optimized?

# ---- create geopackage index, filename, coverage polygon, and crs ----

ids            <- data.frame(llid = stringr::str_extract(f_list, "[0-9]+"),
                             stringsAsFactors = FALSE)
coverage_crs   <- as.character(do.call("rbind",
                        lapply(f_list, function(x) st_crs(raster(x))$input)))
coverage_polys <- lapply(seq_along(coverage_crs),
       function(i) st_transform(st_as_sfc(st_bbox(raster(f_list[i])),
                                          crs = coverage_crs[i]), 5071)) %>%
  Reduce(c, .) %>%
  st_as_sf(ids, geometry = .) %>%
  mutate(crs = coverage_crs,
         file = f_list)

# plot(coverage_polys$geometry)
# unlink("bathymetry.gpkg")
sf::st_write(coverage_polys, "data/bathymetry.gpkg")
# test <- st_read("data/bathymetry.gpkg")

# ---- zip files ----
zip("data/bathymetry.zip", c("data/README.md", "data/bathymetry.gpkg", f_list))
