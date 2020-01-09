source("scripts/99_utils.R")

lg <- lagosus_load("locus")

# given a raster, find:
#   the max depth
#   the deepest point
#   the point furthest from land
#   the distance from each point to land
#   the distance between these points
#   the true in-lake "slope"
get_geometry <- function(r, llid, deep_positive = TRUE, ft = 1){
  # r <- raster("data/mi_bathy/257862.tif")
  # test_poly <- LAGOSUSgis::query_gis("LAGOS_US_All_Lakes_1ha", "lagoslakeid", c(257862, 3958))
  # TODO: see if theres a way to fix or auto remove problematic lakes like 257862

  # r <- raster("data/ct_bathy/101661.tif")
  dt_poly      <- st_zm(concaveman::concaveman(
    st_sf(st_sfc(
      st_multipoint(rasterToPoints(r)), crs = st_crs(r)))
  ))

  if(!deep_positive){
    xy       <- xyFromCell(r, which.min(r[]))
    maxdepth <- abs(r[which.min(r[])][1]) / ft
  }else{
    xy       <- xyFromCell(r, which.max(r[]))
    maxdepth <- abs(r[which.max(r)][1]) / ft
  }
  pnt_deepest   <- st_sfc(st_multipoint(xy), crs = st_crs(r))
  pnt_viscenter <- as.numeric(
    polylabelr::poi(st_coordinates(dt_poly)[,1:2])[1:2])
  pnt_viscenter <- st_sfc(st_point(pnt_viscenter), crs = st_crs(r))

  dist_deepest   <- st_distance(pnt_deepest,
                              st_cast(dt_poly, "MULTILINESTRING"))
  dist_viscenter <- st_distance(pnt_viscenter,
                                st_cast(dt_poly, "MULTILINESTRING"))
  dist_between   <- st_distance(pnt_deepest, pnt_viscenter)

  inlake_slope   <- maxdepth / as.numeric(dist_deepest)

  list(pnt_deepest = pnt_deepest, pnt_viscenter = pnt_viscenter,
       dist_deepest = dist_deepest, dist_viscenter = dist_viscenter,
       dist_between = dist_between, inlake_slope = inlake_slope,
       maxdepth = maxdepth, llid = llid)
}

rm_bad_rasters <- function(rsubs){
  max_raster_size <- 4470000
  rsubs           <- rsubs[!is.na(sapply(rsubs, minValue))]
  rsubs           <- rsubs[sapply(rsubs, ncell) < max_raster_size]
  rsubs
}

loop_state <- function(fpath, outname, deep_positive, ft = 1){
  flist <- list.files(fpath, pattern = "\\d.tif",
                         full.names = TRUE, include.dirs = TRUE)
  if(!file.exists(outname)){
    rsubs <- lapply(flist, function(x) raster(x))
    rsubs <- rm_bad_rasters(rsubs)
    pb <- progress_bar$new(
      format = "llid :llid [:bar] :percent",
      total = length(rsubs),
      clear = FALSE, width = 80)

    res <- lapply(rsubs, function(x){
      # x <- rsubs[[1]]
      pb$tick(tokens = list(llid = gsub("X", "", names(x))))
      get_geometry(x, deep_positive = deep_positive,
                   llid = gsub("X", "", names(x)), ft = ft)
    })

    saveRDS(res, outname)
    return(res)
  }else{
    return(readRDS(outname))
  }
}

res_all         <- list()

# MN
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/mn_bathy/",
             "data/00_bathy_depth/00_bathy_depth_mn.rds",
             deep_positive = FALSE)
), state = "MN", source = "https://gisdata.mn.gov/dataset/water-lake-bathymetry"))
# unlink("data/00_bathy_depth/00_bathy_depth_mn.rds")

# CT
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/ct_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ct.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "CT", source = "https://cteco.uconn.edu/ctmaps/rest/services/Elevation/Lake_Bathymetry/MapServer/"))
# unlink("data/00_bathy_depth/00_bathy_depth_ct.rds")

# KS
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/ks_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ks.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "KS", source = "http://kars.ku.edu/arcgis/rest/services/WaterResources/BathymetryContour/MapServer/"))

# MA
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/ma_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ma.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "MA", source = "http://download.massgis.digital.mass.gov/shapefiles/state/dfwbathy.zip"))

# MI
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/mi_bathy/",
             "data/00_bathy_depth/00_bathy_depth_mi.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "MI", source = "https://opendata.arcgis.com/datasets/d49160d2e5af4123b15d48c2e9c70160_4"))
# unlink("data/00_bathy_depth/00_bathy_depth_mi.rds")

# NE
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/ne_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ne.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "NE", source = "https://maps.outdoornebraska.gov/arcgis/rest/services/Programs/LakeMapping/MapServer/"))

# NH
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/nh_bathy/",
             "data/00_bathy_depth/00_bathy_depth_nh.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "NH", source = "http://www.granit.unh.edu/cgi-bin/nhsearch?dset=bathymetry_lakes_polygons/nh"))

# IA
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/ia_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ia.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "IA", source = "http://iowageodata.s3.amazonaws.com/inlandWaters/lakes_bathymetry.zip"))

# write geometry to an rds file containing an sf object with two geometries
#         pnt_deepest and pnt_viscenter
saveRDS(st_as_sf(res_all),
        "data/00_bathy_depth/bathy_pnts.rds")

# write geometry stats without pnt geometry
write.csv(select(res_all, -contains("pnt")),
          "data/00_bathy_depth/bathy_geometry.csv", row.names = FALSE)
# res_all <- read.csv("data/00_bathy_depth/bathy_geometry.csv", stringsAsFactors = FALSE)

# write max depth formatted like other max depth sources
res_final <- res_all %>%
  mutate(effort = "bathymetry") %>%
  dplyr::select(llid, state, max_depth_m = maxdepth, source,
                effort, -contains("pnt"), -contains("dist")) %>%
  left_join(mutate(dplyr::select(lg$locus$locus_characteristics,
                          lagoslakeid, lake_waterarea_ha,
                          lake_connectivity_permanent),
                   lagoslakeid = as.character(lagoslakeid)),
            by = c("llid" = "lagoslakeid")) %>%
  left_join(mutate(dplyr::select(lg$locus$locus_information, lagoslakeid,
                          lake_lat_decdeg, lake_lon_decdeg),
                   lagoslakeid = as.character(lagoslakeid)),
            by = c("llid" = "lagoslakeid")) %>%
  mutate(lag = lake_lat_decdeg, long = lake_lon_decdeg) %>%
  write.csv("data/00_bathy_depth/00_bathy_depth.csv", row.names = FALSE)

# res_all <- read.csv("data/00_bathy_depth/bathy_geometry.csv",
#                     stringsAsFactors = FALSE)

# dt_raw_ne <- read.csv("data/lagosne_depth_predictors.csv",
#                       stringsAsFactors = FALSE) %>%
#   mutate(llid = as.character(lagoslakeid))
# test      <- left_join(res_all, dt_raw_ne) %>%
#   data.frame()
# test$calc_depth_lgne <- calc_depth(test$buffer100m_slope_max, test$dist_deepest,
#                                   grain = 10)
# test$calc_depth_lgus <- calc_depth(test$inlake_slope, test$dist_deepest)
# plot(test$maxdepth, test$calc_depth_lgus)
# plot(test$lake_maxdepth_m, test$calc_depth_lgus)
# abline(0, 1)
#
# ggplot(data = test) +
#   geom_point(aes(x = lake_maxdepth_m, y = calc_depth_lgus)) +
#   geom_abline(aes(slope = 1, intercept = 0)) +
#   facet_wrap(~state)

# plotly::ggplotly())

#
# hist(test$inlake_slope)
# hist(test$dist_deepest)
# hist(test$calc_depth_lgus)
#
# plot(test$calc_depth_lgne, test$calc_depth_lgus)
# plot(test$lake_maxdepth_m, test$calc_depth_lgne)

#
# # 2654
#
#
#
# arrange(res, desc(dist_between)) %>%
#   View()
#
# hist(res$dist_between)
# plot(res$dist_deepest, res$dist_viscenter)
# abline(0, 1)

# pnt_surface  <- st_point_on_surface(dt_poly)
## point furthest from shore
# test <- as_Spatial(dt_poly)
# r <- rasterize(SpatialPolygons(test@polygons), dt)
# r[is.na(r)] <- 0
# r[r == 1] <- NA
# test2 <- distance(r)
# which.max(test2)
# xy           <- xyFromCell(dt, which.max(test2[]))
# pnt_furthest  <- st_sfc(st_multipoint(xy), crs = st_crs(dt))
# pnt_centroid <- st_centroid(dt_poly)

# dt           <- raster("data/mn_bathy/2419.tif")
# compare_deepest_center(dt)
# mapview::mapview(dt_poly) +
#   mapview::mapview(pnt_viscenter) +
# mapview::mapview(r) +
#   mapview::mapview(pnt_deepest)
