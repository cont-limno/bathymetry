library(sf)
library(LAGOSNE)
library(LAGOSNEgis)
library(dplyr)
library(mapview)
library(raster)
library(rayshader)
library(elevatr)
library(ggpointdensity)
library(ggplot2)

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
# r <- raster("data/mn_bathy/lake_bathymetric_elevation_model.tif")
#
# lg_raw <- lagosne_load()
# lg     <- left_join(lg_raw$locus, lg_raw$state) %>%
#   left_join(dplyr::select(lg_raw$lakes_limno, lagoslakeid, maxdepth, maxdepthsource)) %>%
#   dplyr::filter(state == "MN") %>%
#   mutate(daratio = maxdepth/lake_area_ha) %>%
#   arrange(desc(daratio)) %>%
#   filter(lake_area_ha < 5000) %>%
#   filter(lake_area_ha >= 120) %>%
#   filter(!is.na(gnis_name)) %>%
#   dplyr::select(lagoslakeid, gnis_name,
#                 nhd_lat, nhd_long, lake_area_ha, maxdepth, maxdepthsource, daratio)
# # filter lakes where the lagosnegis points don't intersect an NA value in r
# llid_pnts   <- query_gis("LAGOS_NE_All_Lakes_4ha_POINTS",
#                          "lagoslakeid", lg$lagoslakeid)
# lg <- lg[!is.na(raster::extract(r, llid_pnts)),]
# # plot(lg$lake_area_ha, lg$maxdepth)
# quantile_sample <- function(x, y = NA, probs = c(0.05, 0.95)){
#   x_qs   <- quantile(x, probs, na.rm = TRUE)
#   x_diff <- sapply(x_qs, function(b) abs(b - x))
#
#   if(!is.na(y[1])){
#     y_qs   <- quantile(y, probs, na.rm = TRUE)
#     y_diff <- sapply(y_qs, function(b) abs(b - y))
#
#     total_diff_1 <- x_diff[,1] + y_diff[,1]
#     total_diff_2 <- x_diff[,2] + y_diff[,2]
#
#     data.frame(x = c(which.min(total_diff_1),
#                      which.min(total_diff_2)),
#                y = c(which.min(total_diff_1),
#                      which.min(total_diff_2)),
#                row.names = probs)
#
#   }else{
#     apply(x_diff, 2, which.min)
#   }
# }
#
# llids <- lg[quantile_sample(lg$lake_area_ha,lg$maxdepth, probs = c(0.96, 0.955))[,1],]
# llids <- llids$lagoslakeid
#
# get_hypso <- function(llid){
#   res_sf   <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", llid)
#   res_sf   <- st_transform(res_sf, 26915)
#   box      <- raster::extent(
#     st_buffer(st_sf(st_as_sfc(st_bbox(res_sf))), 100))
#   rsub     <- crop(r, box)
#   maxdepth <- abs(cellStats(rsub, "min"))
#   if(maxdepth == Inf){
#     stop("No depth data found in layer.")
#   }
#
#   # replace NA with data from elevatr
#   elev <- get_elev_raster(rsub, 13)
#   test <- crop(elev, rsub)
#   test <- mask(test, test <= (cellStats(test, "min") + 1), maskvalue = TRUE)
#   test <- test - cellStats(test, "min")
#   test <- resample(test, rsub)
#
#   test2 <- merge(test, rsub)
#   test2[is.na(test2)] <- 0
#
#   rmat <- matrix(test2 + maxdepth,
#                  nrow = ncol(rsub), ncol = nrow(rsub))
#   rmat[rmat > maxdepth] <- NA
#   # slice half off to show profile
#   # rmat[1:(nrow(rmat)/2),] <- NA
#   list(rmat = rmat, maxdepth = maxdepth, res_sf = res_sf, rsub = rsub)
# }
#
# hypso_1 <- get_hypso(lg$lagoslakeid[2])
#
# sphere_shade(hypso_1$rmat, texture = tex) %>%
#   plot_3d(hypso_1$rmat, water = TRUE, waterdepth = 152.997,
#           zscale = 0.4, solidcolor = "white", solidalpha = 0.4,
#           shadow = FALSE, solidlinecolor = "white", phi = 20)
#
# sphere_shade(hypso_1$rmat, texture = tex) %>%
#   plot_3d(hypso_1$rmat, water = TRUE, waterdepth = 152.997,
#           zscale = 0.4, solidcolor = "white", solidalpha = 1,
#           shadow = FALSE, solidlinecolor = "white", phi = 20)
#
# sphere_shade(hypso_1$rmat) %>%
#   plot_3d(hypso_1$rmat, water = TRUE, waterdepth = 152.997,
#           zscale = 0.4, solidalpha = 1,
#           shadow = FALSE, solidlinecolor = "white", phi = 20)
#
# write.csv(hypso_1$rmat, "rmat.csv", row.names = FALSE)
# ####

library(rayshader)

rmat <- read.csv(url("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4ePN6nkzKFkqCnlentHkKx-3Ugn7uIy486bMoqtazW9R9SIiipJ1if7-YgzIAcm1d4O1oNq_eSP8x/pub?gid=556110853&single=true&output=csv"))
rmat <- as.matrix(rmat)

tex <- create_texture("#AFEEEE", "#B0C4DE", "#B0E2FF", "#B2DFEE", "#BCD2EE")

sphere_shade(rmat, texture = tex) %>%
  plot_3d(rmat, water = TRUE, waterdepth = 152.997,
          zscale = 1, solidcolor = "white", solidalpha = 0.4,
          shadow = FALSE, solidlinecolor = "white", phi = 20)
render_snapshot(clear = TRUE)

sphere_shade(rmat, texture = tex) %>%
  plot_3d(rmat, water = TRUE, waterdepth = 152.997,
          zscale = 0.4, solidcolor = "white", solidalpha = 1,
          shadow = FALSE, solidlinecolor = "white", phi = 20)
render_snapshot(clear = TRUE)


library(magick)

image_write(image_append(c(
  image_trim(image_read("1.png")),
  image_trim(image_read("2.png")))), "solidalpha.png")
