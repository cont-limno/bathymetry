library(sf)
library(LAGOSNE)
library(LAGOSNEgis)
library(dplyr)
library(mapview)
library(raster)
library(rayshader)
library(elevatr)

# Lake Bathymetric Digital Elevation Model (DEM): A digital elevation
# model (DEM) representing lake bathymetry. Cell size is most often 5m,
# although 10m cells were used for some lakes to reduce grid file size. This
# grid # contains one attribute DEPTH that represents lake depth in (negative)
# feet. Use in combination with other Lake Bathymetric GIS products. Reclassify
# DEM based on various depth intervals. Calculate zonal and neighborhood
# statistics. Derive slope surface. Model depth data with other cell-based
# parameters (e.g., slope, vegetation, substrate, chemistry) to predict habitat
# suitability, functional niches, etc. (Note: These raster analyses require
# Spatial Analyst or Arc Grid.)
r <- raster("data/mn_bathy/lake_bathymetric_elevation_model.tif")

lg_raw <- lagosne_load()
lg     <- left_join(lg_raw$locus, lg_raw$state) %>%
  dplyr::filter(state == "MN") %>%
  arrange(desc(lake_area_ha)) %>%
  filter(lake_area_ha < 1200) %>%
  filter(lake_area_ha >= 4) %>%
  dplyr::select(lagoslakeid, gnis_name,
                nhd_lat, nhd_long, lake_area_ha)

res_sf   <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", 1183)
res_sf   <- st_transform(res_sf, 26915)
box      <- raster::extent(
  st_buffer(st_sf(st_as_sfc(st_bbox(res_sf))), 50))
rsub     <- crop(r, box)
maxdepth <- abs(cellStats(rsub, "min"))

# TODO: replace NA with data from elevatr
elev <- get_elev_raster(rsub, 13)
test <- crop(elev, rsub)
test <- mask(test, test <= (cellStats(test, "min") + 1), maskvalue = TRUE)
test <- test - cellStats(test, "min")
test <- resample(test, rsub)

test2 <- merge(test, rsub)
test2[is.na(test2)] <- 0

# plot(rsub)
# mapview(res_sf)

# rayshader
rmat <- matrix(test2 + maxdepth,
               nrow = ncol(rsub), ncol = nrow(rsub))
rmat[rmat > maxdepth] <- NA
# slice half off to show profile
# rmat[1:(nrow(rmat)/2),] <- NA

tex <- create_texture("#AFEEEE", "#B0C4DE", "#B0E2FF", "#B2DFEE", "#BCD2EE")
rmat %>%
  sphere_shade(texture = tex) %>%
  plot_3d(rmat, water = TRUE, waterdepth = maxdepth,
          zscale = 0.2, solidcolor = "white", solidalpha = 0.1,
          shadow = FALSE, solidlinecolor = "white", phi = 20)
render_snapshot(clear = TRUE)


rmat %>%
  sphere_shade(texture = tex) %>%
  plot_3d(rmat, water = TRUE, waterdepth = maxdepth,
          zscale = 0.2, solidcolor = "white", solidalpha = 1,
          shadow = FALSE, solidlinecolor = "white", phi = 20)
render_snapshot(clear = TRUE)

library(magick)

image_write(image_append(c(
  image_trim(image_read("solidalpha1.0.png")),
  image_trim(image_read("soildalpha0.1.png")))), "solidalpha.png")

