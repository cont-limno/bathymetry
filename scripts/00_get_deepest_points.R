source("scripts/99_utils.R")

# For non-MN lakes, we probably need a more sophisticated interpolation routine
#   that "bends" below the deepest point in the raw data

compare_deepest_center <- function(r){
  dt_poly      <- st_zm(concaveman::concaveman(
    st_sf(st_sfc(
      st_multipoint(rasterToPoints(r)), crs = st_crs(r)))
  ))

  xy            <- xyFromCell(r, which.min(r[]))
  pnt_deepest   <- st_sfc(st_multipoint(xy), crs = st_crs(r))
  pnt_viscenter <- as.numeric(
    polylabelr::poi(st_coordinates(dt_poly)[,1:2])[1:2])
  pnt_viscenter <- st_sfc(st_point(pnt_viscenter), crs = st_crs(r))

  dist_deepest   <- st_distance(pnt_deepest,
                              st_cast(dt_poly, "MULTILINESTRING"))
  dist_viscenter <- st_distance(pnt_viscenter,
                                st_cast(dt_poly, "MULTILINESTRING"))
  dist_between <- st_distance(pnt_deepest, pnt_viscenter)

  list(pnt_deepest = pnt_deepest, pnt_viscenter = pnt_viscenter,
       dist_deepest = dist_deepest, dist_viscenter = dist_viscenter,
       dist_between = dist_between)
}
# dt           <- raster("data/mn_bathy/10310.tif")
# compare_deepest_center(dt)

flist        <- list.files("data/mn_bathy/", patter = "\\d.tif",
                           full.names = TRUE, include.dirs = TRUE)
rsubs <- lapply(flist, function(x) raster(x))
rsubs <- rsubs[!is.na(sapply(rsubs, minValue))]

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = length(rsubs),
  clear = FALSE, width = 80)

res <- lapply(rsubs,
              function(x){
                # x <- rsubs[[1]]
                pb$tick(tokens = list(llid = gsub("X", "", names(x))))
                compare_deepest_center(x)
                })

# mapview::mapview(dt_poly) +
#   mapview::mapview(pnt_viscenter) +
# mapview::mapview(dt) +
#   mapview::mapview(pnt_deepest)

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
