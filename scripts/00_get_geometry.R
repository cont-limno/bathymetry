source("scripts/99_utils.R")

# given a raster, find:
#   the max depth
#   the deepest point
#   the point furthest from land
#   the distance from each point to land
#   the distance between these points
#   the true in-lake "slope"
get_geometry <- function(r, llid, deep_positive = TRUE){
  # r <- raster("data/mn_bathy/1001.tif")
  # r <- raster("data/ct_bathy/101661.tif")
  dt_poly      <- st_zm(concaveman::concaveman(
    st_sf(st_sfc(
      st_multipoint(rasterToPoints(r)), crs = st_crs(r)))
  ))

  if(!deep_positive){
    xy       <- xyFromCell(r, which.min(r[]))
    maxdepth <- abs(r[which.min(r[])][1])
  }else{
    xy       <- xyFromCell(r, which.max(r[]))
    maxdepth <- abs(r[which.max(r)][1])
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

loop_state <- function(fpath, outname, deep_positive){
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
                   llid = gsub("X", "", names(x)))
    })

    saveRDS(res, outname)
    return(res)
  }else{
    return(readRDS(outname))
  }
}

res_all         <- list()

# MN
res_all <- rbind(res_all, bind_rows(
  loop_state("data/mn_bathy/",
             "data/00_bathy_depth/00_bathy_depth_mn.rds",
             deep_positive = FALSE)
))

# CT
res_all <- rbind(res_all, bind_rows(
  loop_state("data/ct_bathy/",
             "data/00_bathy_depth/00_bathy_depth_ct.rds",
             deep_positive = TRUE)
))



# saveRDS(res, "test.rds")
res <- readRDS("test.rds")
res <- bind_rows(res)
res$llid <- gsub("X", "", unlist(lapply(rsubs, names)))

dt_raw_ne <- read.csv("data/lagosne_depth_predictors.csv",
                      stringsAsFactors = FALSE) %>%
  mutate(llid = as.character(llid))

res <- left_join(res, dt_raw_ne)

res$calc_depth <- calc_depth(res$buffer100m_slope_max, res$dist_deepest)

plot(res$max_depth_m, res$calc_depth)
abline(0, 1)


arrange(res, desc(dist_between)) %>%
  View()

hist(res$dist_between)
plot(res$dist_deepest, res$dist_viscenter)
abline(0, 1)

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
