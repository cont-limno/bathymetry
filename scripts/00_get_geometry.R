# setwd("../")
source("scripts/99_utils.R")

lg <- lagosus_load("locus")

# given a raster, find:
#   the max depth
#   the deepest point
#   the point furthest from land
#   the distance from each point to land
#   the distance between these points
#   the true in-lake "slope"
# get_geometry(raster(paste0("data/mi_bathy/", 2119, ".tif")), 2119, ft = 3.281,
#  dt_poly = LAGOSUSgis::query_gis("LAGOS_US_All_Lakes_1ha", "lagoslakeid", 2119))
#
# get_geometry(raster(paste0("data/ct_bathy/", 7324, ".tif")), 7324, ft = 3.281,
#  dt_poly = LAGOSUSgis::query_gis("LAGOS_US_All_Lakes_1ha", "lagoslakeid", 7324))
get_geometry <- function(r, llid, deep_positive = TRUE, ft = 1, dt_poly){
  # llid <- 1006
  # r <- raster(paste0("data/mi_bathy/", llid, ".tif"))
  # deep_positive = TRUE
  # ft <- 3.281
  # dt_poly <- LAGOSUSgis::query_gis("LAGOS_US_All_Lakes_1ha", "lagoslakeid", llid)

  proj_str_init <- st_crs(r)$proj4string
  proj_str      <- proj_str_init
  if(nrow(st_coordinates(st_transform(dt_poly, proj_str))) >= 1){
    dt_poly <- st_transform(dt_poly, proj_str)
  }else{
    proj_str <- st_crs(dt_poly)
    r <- projectRaster(r, crs = proj_str$proj4string)
  }
  if(is.na(sf::st_is_valid(dt_poly))){
    dt_poly <- sf::st_make_valid(dt_poly)
  }

  if(!st_is_simple(dt_poly) | (
     st_area(dt_poly) > units::as_units(130000, "m2") &
     nrow(st_coordinates(dt_poly)) > 32)){
    dt_poly_raw <- dt_poly
    dt_poly     <- dt_poly_raw %>%
      sf::st_make_valid() %>%
      rmapshaper::ms_simplify(0.1)
    if(st_area(dt_poly_raw) > units::as_units(2700000, "m2") &
       nrow(st_coordinates(dt_poly_raw)) > 32){
      dt_poly     <- dt_poly_raw %>%
        sf::st_make_valid() %>%
        rmapshaper::ms_simplify(0.02)
    }
  }

  dt_poly_coords <- st_coordinates(dt_poly)[,1:2]
  # dt_poly_coords <- dt_poly_coords[!(duplicated(paste(dt_poly_coords[,1], dt_poly_coords[,2])))]
  pnt_viscenter <- polylabelr::poi(dt_poly_coords)
  pnt_viscenter <- as.numeric(pnt_viscenter[1:2])
  pnt_viscenter <- st_sfc(st_point(pnt_viscenter), crs = proj_str)
  st_crs(pnt_viscenter) <- proj_str
  dist_viscenter <- st_distance(pnt_viscenter,
                                st_cast(dt_poly, "MULTILINESTRING"))
  # mapview(dt_poly) + mapview(r) + mapview(pnt_viscenter)

  if(!deep_positive){
    maxdepth  <- abs(r[which.min(r[])][1]) / ft
    meandepth <- abs(cellStats(r, mean)) / ft
    # smooth out the minima in MN raster data
    # r <- raster(paste0("data/mn_bathy/", llid, ".tif"))
    r <- reclassify(r, matrix(c(floor(min(r[], na.rm = TRUE)),
                            quantile(r[], 0.01, na.rm = TRUE),
                            quantile(r[], 0.01, na.rm = TRUE)),
                            ncol = 3, byrow = TRUE),
                       include.lowest = FALSE)
    xy <- xyFromCell(r, which(r[] == min(r[], na.rm = TRUE)))
  }else{
    xy        <- xyFromCell(r, which(r[] == max(r[], na.rm = TRUE)))
    r         <- reclassify(r, cbind(NULL, NA))
    maxdepth  <- abs(r[which.max(r)][1]) / ft
    meandepth <- cellStats(r, mean) / ft
  }
  pnts_deepest <- st_cast(
    st_sfc(st_multipoint(xy), crs = proj_str), "POINT")
  st_crs(pnts_deepest) <- proj_str
  pnts_deepest <- pnts_deepest[
    st_distance(st_cast(dt_poly, "MULTILINESTRING"), pnts_deepest) <=
      units::as_units(as.numeric(dist_viscenter), "m")]

  pnt_deepest <- pnts_deepest[
    which.max(st_distance(st_cast(dt_poly, "MULTILINESTRING"), pnts_deepest))]
  if(nrow(st_coordinates(pnt_deepest)) == 0){
    pnts_deepest <- st_cast(
      st_sfc(st_multipoint(xy), crs = proj_str), "POINT")
    st_crs(pnts_deepest) <- proj_str
    pnt_deepest <- pnts_deepest[
      which.min(st_distance(pnt_viscenter, pnts_deepest))]
  }

  # mapview(dt_poly) + mapview(r) + mapview(pnt_viscenter) + mapview(pnt_deepest, color = "red")

  # ggplot() + geom_sf(data = dt_poly) +
  #   coord_sf(datum = st_crs(r)) +
  #   theme(axis.text.x = element_text(angle = 90))

  # mapview(st_nearest_points(pnt_deepest, st_cast(dt_poly, "MULTILINESTRING")))
  # mapview(st_nearest_points(pnt_viscenter, st_cast(dt_poly, "MULTILINESTRING")))

  dist_between   <- st_distance(pnt_deepest, pnt_viscenter)
  dist_deepest   <- st_distance(pnt_deepest,
                              st_cast(dt_poly, "MULTILINESTRING"))
  dists_deepest  <- mean(
                    st_distance(pnts_deepest,
                                st_cast(dt_poly, "MULTILINESTRING"))
                    )
  # dist_viscenter > dist_deepest

  inlake_slope_pnt  <- maxdepth / as.numeric(dist_deepest)
  inlake_slope_pnts <- maxdepth / as.numeric(dists_deepest)

  pnt_shore  <- sf::st_nearest_points(pnt_deepest, st_transform(
    st_cast(dt_poly, "MULTILINESTRING"), proj_str))
  pnts_shore <- sf::st_nearest_points(pnts_deepest, st_transform(
    st_cast(dt_poly, "MULTILINESTRING"), proj_str))

  r_slope                     <- terrain(r, "slope")

  inlake_slope_mean           <- mean(r_slope@data@values, na.rm = TRUE)
  inlake_slope_median         <- median(r_slope@data@values, na.rm = TRUE)

  r_slope_pnt_values <- extract(r_slope, as_Spatial(pnt_shore))[[1]]
  # View(bench::mark(
  #   unique(as.numeric(na.omit(extract(r_slope, as_Spatial(pnt_shore))[[1]]))),
  #   unique(as.numeric(na.omit(mask(r_slope, as_Spatial(pnt_shore))@data@values))),
  #   min_iterations = 10
  # ))
  # profvis::profvis({
  #   extract(r_slope, as_Spatial(pnt_shore))
  #   mask(r_slope, as_Spatial(pnt_shore))@data@values
  #   })

  inlake_slope_online_mean    <- mean(r_slope_pnt_values, na.rm = TRUE) # * res(r)[1]
  inlake_slope_online_median  <- median(r_slope_pnt_values, na.rm = TRUE) # * res(r)[1]

  # View(bench::mark(
  #   sort(unique(as.numeric(na.omit(unlist(extract(r_slope, as_Spatial(pnts_shore))))))),
  #   sort(unique(as.numeric(na.omit(mask(r_slope, as_Spatial(pnts_shore))@data@values)))), min_iterations = 10
  # ))
  # profvis::profvis({
  #   extract(r_slope, as_Spatial(pnts_shore))
  #   mask(r_slope, as_Spatial(pnts_shore))@data@values
  #   })

  r_slope_pnts_values         <- unlist(extract(r_slope, as_Spatial(pnts_shore)))
  inlake_slopes_online_mean   <- mean(r_slope_pnts_values, na.rm = TRUE) # * res(r)[1]
  inlake_slopes_online_median <- median(r_slope_pnts_values, na.rm = TRUE) # * res(r)[1]

  # make sure pnt deepest and pnt_viscenter match
  # the projection of the on disk raster
  # attempt to project pnts to `proj_str_init`
  # if empty project raster to `proj_str`

  list(pnt_deepest = pnt_deepest, pnts_deepest = st_as_sf(st_combine(pnts_deepest)),
       pnt_viscenter = pnt_viscenter,
       dist_deepest = dist_deepest, dists_deepest = dists_deepest,
       dist_viscenter = dist_viscenter,
       dist_between = dist_between, inlake_slope_pnt = inlake_slope_pnt,
       inlake_slope_pnts = inlake_slope_pnts,
       inlake_slope_online_mean = inlake_slope_online_mean,
       inlake_slopes_online_mean = inlake_slopes_online_mean,
       inlake_slope_mean = inlake_slope_mean, inlake_slope_median =
         inlake_slope_median,
       maxdepth = maxdepth, meandepth = meandepth, llid = llid)
}

rm_bad_rasters <- function(rsubs){
  max_raster_size <- 4470000
  rsubs           <- rsubs[!is.na(sapply(rsubs, minValue))]
  rsubs           <- rsubs[sapply(rsubs, ncell) < max_raster_size]
  rsubs
}

loop_state <- function(fpath, outname, deep_positive, ft = 1){
  # fpath <- "data/mi_bathy/"
  # outname <- "data/00_bathy_depth/00_bathy_depth_mi.rds"
  # deep_positive = TRUE
  # ft = 3.281
  flist <- list.files(fpath, pattern = "\\d.tif",
                         full.names = TRUE, include.dirs = TRUE)
  # print(flist)
  if(!file.exists(outname)){
    rsubs <- lapply(flist, function(x) raster(x))
    rsubs <- rm_bad_rasters(rsubs)
    # print(gsub("X", "", unlist(lapply(rsubs, names)))[300])
    pb <- progress_bar$new(
      format = "llid :llid [:bar] :percent",
      total = length(rsubs),
      clear = FALSE, width = 80)

    dt_polys <- st_read("data/gis.gpkg", layer = "dt_polys",
        query = paste0(
          "SELECT * FROM dt_polys WHERE ",
          paste0("lagoslakeid LIKE '",
                 gsub("X", "", unlist(lapply(rsubs, function(x) names(x)))),
                 "'", collapse = " OR ")
          ))

    res <- lapply(rsubs, function(x){
      # x <- rsubs[[1]]
      pb$tick(tokens = list(llid = gsub("X", "", names(x))))
      get_geometry(x, deep_positive = deep_positive,
                   llid = gsub("X", "", names(x)), ft = ft,
                   dt_poly = dt_polys[dt_polys$lagoslakeid == gsub("X", "", names(x)),])
    })

    saveRDS(res, outname)
    return(res)
  }else{
    return(readRDS(outname))
  }
}

res_all      <- list()
fpath_stem   <- "data"
outname_stem <- "data"
# MN
# message("Calculating MN geometries...")
# res_all <- rbind(res_all, mutate(bind_rows(
#   loop_state(paste0(fpath_stem, "/mn_bathy/"),
#              paste0(outname_stem, "/00_bathy_depth/00_bathy_depth_mn.rds"),
#              deep_positive = FALSE)
# ), state = "MN", source = "https://gisdata.mn.gov/dataset/water-lake-bathymetry"))
# unlink("data/00_bathy_depth/00_bathy_depth_mn.rds")

# CT
message("Calculating CT geometries...")
res_all <- rbind(res_all, mutate(
  bind_rows(
  loop_state("data/ct_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ct.rds",
             deep_positive = TRUE,
             ft = 3.281)
  ),
  state = "CT", source = "https://cteco.uconn.edu/ctmaps/rest/services/Elevation/Lake_Bathymetry/MapServer/"))
# unlink("data/00_bathy_depth/00_bathy_depth_ct.rds")

# KS
message("Calculating KS geometries...")
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/ks_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ks.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "KS", source = "http://kars.ku.edu/arcgis/rest/services/WaterResources/BathymetryContour/MapServer/"))
# unlink("data/00_bathy_depth/00_bathy_depth_ks.rds")

# MA
message("Calculating MA geometries...")
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/ma_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ma.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "MA", source = "http://download.massgis.digital.mass.gov/shapefiles/state/dfwbathy.zip"))
# unlink("data/00_bathy_depth/00_bathy_depth_ma.rds")

# MI
message("Calculating MI geometries...")
res_all <- rbind(res_all, mutate(
  bind_rows(
  loop_state("data/mi_bathy/",
             "data/00_bathy_depth/00_bathy_depth_mi.rds",
             deep_positive = TRUE,
             ft = 3.281)), # 300 throws an error
  state = "MI", source = "https://opendata.arcgis.com/datasets/d49160d2e5af4123b15d48c2e9c70160_4"))
# unlink("data/00_bathy_depth/00_bathy_depth_mi.rds")

# NE
message("Calculating NE geometries...")
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/ne_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ne.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "NE", source = "https://maps.outdoornebraska.gov/arcgis/rest/services/Programs/LakeMapping/MapServer/"))
# unlink("data/00_bathy_depth/00_bathy_depth_ne.rds")

# NH
message("Calculating NH geometries...")
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/nh_bathy/",
             "data/00_bathy_depth/00_bathy_depth_nh.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "NH", source = "http://www.granit.unh.edu/cgi-bin/nhsearch?dset=bathymetry_lakes_polygons/nh"))
# unlink("data/00_bathy_depth/00_bathy_depth_nh.rds")

# IA
message("Calculating IA geometries...")
res_all <- rbind(res_all, mutate(bind_rows(
  loop_state("data/ia_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ia.rds",
             deep_positive = TRUE,
             ft = 3.281)
), state = "IA", source = "http://iowageodata.s3.amazonaws.com/inlandWaters/lakes_bathymetry.zip"))
# unlink("data/00_bathy_depth/00_bathy_depth_ia.rds")

# ME
message("Calculating ME geometries...")
res_me <- mutate(
  bind_rows(loop_state("data/me_bathy/",
                       "data/00_bathy_depth/00_bathy_depth_me.rds",
                       deep_positive = TRUE,
                       ft = 1)),
  state = "ME",
  source = "https://www.maine.gov/megis/catalog/shps/state/lakedpths.zip")
res_all <- rbind(res_all, res_me)
# unlink("data/00_bathy_depth/00_bathy_depth_me.rds")

# write geometry to an rds file containing an sf object with two geometries
#         pnt_deepest and pnt_viscenter
res_all <- dplyr::filter(res_all, dist_deepest > 0.1)
saveRDS(st_as_sf(res_all),
        "data/00_bathy_depth/bathy_pnts.rds")

# write geometry stats without pnt geometry
write.csv(select(res_all, -contains("pnt")),
          "data/00_bathy_depth/bathy_geometry.csv", row.names = FALSE)

# write max depth formatted like other max depth sources
res_final <- res_all %>%
  mutate(effort = "bathymetry") %>%
  dplyr::select(llid, state, max_depth_m = maxdepth,
                mean_depth_m = meandepth, source,
                effort, -contains("pnt"), -contains("dist")) %>%
  left_join(mutate(dplyr::select(lg$locus$lake_characteristics,
                          lagoslakeid, lake_waterarea_ha,
                          lake_connectivity_permanent),
                   lagoslakeid = as.character(lagoslakeid)),
            by = c("llid" = "lagoslakeid")) %>%
  left_join(mutate(dplyr::select(lg$locus$lake_information, lagoslakeid,
                          lake_lat_decdeg, lake_lon_decdeg),
                   lagoslakeid = as.character(lagoslakeid)),
            by = c("llid" = "lagoslakeid")) %>%
  mutate(lat = lake_lat_decdeg, long = lake_lon_decdeg) %>%
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
