# setwd("../")
source("scripts/99_utils.R")
# lagos geo module should have terrain metrics for mean slope
# Oliver (year) found that max slope was most informative

max_buffer_dist <- 100 # to match Hollister (2011)

dt         <- read.csv("data/lagosus_depth.csv",
                    stringsAsFactors = FALSE)
bathy_pnts <- readRDS("data/00_bathy_depth/bathy_pnts.rds")
dt_pred    <- read.csv("data/lagosne_depth_predictors.csv",
                    stringsAsFactors = FALSE) %>%
  dplyr::filter(lagoslakeid %in% bathy_pnts$llid) %>%
  dplyr::filter(lagoslakeid != 4870)

# attempt at manual calculation of max buffer slope

ll_ids            <- unique(dt_pred$lagoslakeid)
flist             <- list.files("data/elevatr", pattern = "\\d*.tif",
                    full.names = TRUE, include.dirs = TRUE)
existing_surfaces <- gsub(".tif", "",
                stringr::str_extract(flist, "\\d*(!?.tif)"))
ll_ids_new <- ll_ids[!(ll_ids %in% existing_surfaces)]
# clipr::write_clip(paste0(ll_ids_new, ","))
# write.csv(ll_ids_new, "ll_ids_new.csv", row.names = FALSE)
# ll_ids <- c(existing_surfaces)
# ll_ids <- c(ll_ids_new)
ll_ids <- c(ll_ids_new, existing_surfaces)
ll_ids <- ll_ids[!duplicated(ll_ids)]

# sapply(flist[!(ll_ids %in% dt_pred$lagoslakeid)], "unlink")
# ll_ids <- ll_ids_new[1965:length(ll_ids_new)]
ll_polys <- LAGOSUSgis::query_gis("LAGOS_US_All_Lakes_1ha", "lagoslakeid", ll_ids)

get_elev <- function(ll_id){
  # ll_id <- ll_ids[1]
  # ll_id <- 7922
  fname <- paste0("data/elevatr/", ll_id, ".tif")
  if(!file.exists(fname)){
    ll_poly <- ll_polys[ll_id == ll_polys$lagoslakeid,]
    ll_poly <- sf::st_make_valid(ll_poly)

    elev    <- suppressMessages(
      get_elev_raster(as_Spatial(st_buffer(ll_poly, max_buffer_dist)),
                      12, clip = "bbox", verbose = FALSE))
    writeRaster(elev, fname)
  }else{
    elev <- raster(fname)
  }
  elev
}

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = length(ll_ids),
  clear = FALSE, width = 80)

if(!interactive()){
  res <- lapply(ll_ids, function(x) {
    pb$tick(tokens = list(llid = x))
    get_elev(x)
  })
}

get_slope <- function(ll_id){
  # ll_id <- ll_ids[1]
  # ll_id <- 100570
  res_path <- paste0("data/elevatr/", ll_id, ".csv")
  if(!file.exists(res_path)){
    ll_poly <- ll_polys[ll_id == ll_polys$lagoslakeid,]
    ll_poly <- sf::st_make_valid(ll_poly)
    ll_iws  <- LAGOSUSgis::query_gis("ws", "lagoslakeid", ll_id)
    # hack together a one-sided buffer [sf doesn't offer this :(]
    ll_buff <- st_buffer(ll_poly, max_buffer_dist)
    ll_buff <- st_difference(ll_buff, ll_poly)

    fname <- paste0("data/elevatr/", ll_id, ".tif")
    elev  <- raster(fname)

    # approximates functions in lakemorpho package
    elev      <- mask(elev, as_Spatial(ll_iws))
    elev      <- mask(elev, as_Spatial(ll_buff))
    slope     <- terrain(elev, "slope")

    nearshore_slope_mean   <- mean(slope@data@values, na.rm = TRUE) # * res(elev)[1]
    nearshore_slope_median <- median(slope@data@values, na.rm = TRUE) # * res(elev)[1]
    nearshore_slope_max    <- max(slope@data@values, na.rm = TRUE) # * res(elev)[1]
    nearshore_slope_ne     <- dplyr::filter(dt_pred, lagoslakeid == ll_id) %>%
      dplyr::select(buffer100m_slope_mean, buffer100m_slope_max) %>%
      mutate_all(function(x) x / 10) # lagosne slopes are per 10m
    nearshore_slope_ne     <- nearshore_slope_ne[1,]

    # pull buffer slope on-line from deepest point
    deepest_pnt              <- dplyr::filter(bathy_pnts, llid == ll_id)
    st_geometry(deepest_pnt) <- "pnt_deepest"
    r_crs               <- raster( # only used to set crs
      paste0("data/", tolower(deepest_pnt$state), "_bathy/", ll_id, ".tif")
    )
    st_crs(deepest_pnt) <- st_crs(r_crs)

    deepest_pnt$pnts_deepest <- list(st_as_sf(
      deepest_pnt$pnts_deepest[[1]], coords = c("X", "Y"),
      crs = st_crs(r_crs)))

    if(is.na(st_distance(st_transform(ll_poly, st_crs(r_crs)), deepest_pnt))){
      st_crs(deepest_pnt) <- st_crs(ll_poly)
      st_crs(deepest_pnt$pnts_deepest[[1]]) <- st_crs(ll_poly)
      r_crs <- projectRaster(r_crs, crs = st_crs(ll_poly)$proj4string)
    }

    ll_poly_hull <- st_zm(st_cast(
      concaveman::concaveman(st_cast(ll_poly, "MULTILINESTRING")),
      "MULTILINESTRING"))
    ll_buff_hull <- st_zm(st_cast(
      concaveman::concaveman(st_cast(ll_buff, "MULTILINESTRING")),
      "MULTILINESTRING"))
    pnt_shore <- sf::st_nearest_points(deepest_pnt,
                                    st_transform(ll_poly_hull, st_crs(deepest_pnt)))
    pnts_shore <- sf::st_nearest_points(deepest_pnt$pnts_deepest[[1]],
                                    st_transform(ll_poly_hull, st_crs(deepest_pnt)))

    buffer_line  <- st_buffer(
      sf::st_nearest_points(pnt_shore,
                            st_transform(ll_buff_hull, st_crs(deepest_pnt))),
                    20)
    buffer_lines  <- st_buffer(
      sf::st_nearest_points(pnts_shore,
                            st_transform(ll_buff_hull, st_crs(deepest_pnt))),
                    20)


    # mapview(elev) +
    # mapview(ll_poly_hull) + mapview(ll_buff_hull) + mapview(deepest_pnt) + mapview(buffer_line) + mapview(st_buffer(buffer_line, 20))

    slope_online        <- raster::extract(slope, as_Spatial(buffer_line))[[1]]
    slope_online_mean   <- mean(slope_online, na.rm = TRUE) # * res(elev)[1]
    slope_online_median <- median(slope_online, na.rm = TRUE) # * res(elev)[1]

    slopes_online <- raster::extract(slope, as_Spatial(buffer_lines))[[1]]
    nearshore_slopes_online_mean <- mean(slopes_online, na.rm = TRUE)

    res <- list(
      nearshore_slope_mean = nearshore_slope_mean,
      nearshore_slope__max = nearshore_slope_max,
      nearshore_slope_median = nearshore_slope_median, # buffer
      # slope_max_ne = slope_ne$buffer100m_slope_max, # buffer
      # slope_mean_ne = slope_ne$buffer100m_slope_mean, # buffer
      nearshore_slope_online_mean = slope_online_mean, # buffer
      slope_online_median = slope_online_median,
      nearshore_slopes_online_mean = nearshore_slopes_online_mean,
      res_elev_1 = res(elev)[1]) # buffer
    res <- bind_rows(res) %>%
      mutate(llid = as.character(ll_id))

    write.csv(res, res_path, row.names = FALSE)
    res
  }else{
    read.csv(res_path)
  }
}
# profvis::profvis({get_slope(ll_ids[1])})

# rm ll_ids not in unique(dt_pred$lagoslakeid)
ll_ids <- ll_ids[ll_ids %in% bathy_pnts$llid]

# bathy_dirs <- as.character(na.omit(unlist(
#   lapply(stringr::str_match_all(list.dirs("data"), "data/(.{2})_bathy"),
#          function(x) as.character(x[1])))))
# bathy_ids <- unlist(
#   lapply(bathy_dirs, function(x) gsub(".tif", "", list.files(x, pattern = "*.tif"))))
# all(ll_ids %in% bathy_ids)

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = length(ll_ids),
  clear = FALSE, width = 80,
  show_after = 0)

res <- lapply(ll_ids, function(x) {
  pb$tick(tokens = list(llid = x))
  get_slope(x)
  })
res <- res %>%
  bind_rows() %>%
  dplyr::mutate(llid = as.character(llid)) %>%
  left_join(mutate(
    dplyr::select(sf::st_drop_geometry(bathy_pnts),
                          llid, starts_with("inlake_slope")),
    llid = as.character(llid)))

# write to csv
write.csv(res, "data/00_geometry/nearshore.csv", row.names = FALSE)

# cor.test(test2$slope_mean, test2$inlake_slope)
# cor.test(test2$slope_online, test2$inlake_slope)
# hist(test2$inlake_slope / test2$slope_online)
