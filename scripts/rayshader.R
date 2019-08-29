library(sf)
library(LAGOSNE)
library(LAGOSNEgis)
library(dplyr)
library(mapview)
library(raster)

# arc_raster.exe from: https://github.com/r-barnes/ArcRasterRescue
# curl::curl_download("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/water_lake_bathymetry/fgdb_water_lake_bathymetry.zip", "data/mn_lake_bathy_gdb.zip")
# unzip("data/mn_lake_bathy_gdb.zip", exdir = "data/mn_bathy")
# system(paste0("arc_raster.exe data/mn_bathy/water_lake_bathymetry.gdb/"))
# system(paste0("arc_raster.exe data/mn_bathy/water_lake_bathymetry.gdb/ 0 lake_bathymetric_elevation_model"))

# Lake Bathymetric Digital Elevation Model (DEM): A digital elevation
# model (DEM) representing lake bathymetry. Cell size is most often 5m,
# although 10m cells were used for some lakes to reduce grid file size. This grid
# contains one attribute DEPTH that represents lake depth in (negative) feet. Use
# in combination with other Lake Bathymetric GIS products. Reclassify DEM based on
# various depth intervals. Calculate zonal and neighborhood statistics. Derive slope
# surface. Model depth data with other cell-based parameters (e.g., slope, vegetation,
# substrate, chemistry) to predict habitat suitability, functional niches, etc. (Note:
# These raster analyses require Spatial Analyst or Arc Grid.)
r <- raster("data/mn_bathy/lake_bathymetric_elevation_model.tif")

lg_raw <- lagosne_load()
lg     <- left_join(lg_raw$locus, lg_raw$state) %>%
  dplyr::filter(state == "MN") %>%
  arrange(desc(lake_area_ha)) %>%
  # filter(lake_area_ha < 2000) %>%
  filter(lake_area_ha >= 4) %>%
  dplyr::select(lagoslakeid, gnis_name,
                nhd_lat, nhd_long, lake_area_ha)

res_sf <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", 996)
res_sf <- st_transform(res_sf, 26915)
box    <- raster::extent(st_sf(st_as_sfc(st_bbox(res_sf))))
rsub   <- crop(r, box)

plot(rsub)
mapview(res_sf)
