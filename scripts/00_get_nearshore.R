source("scripts/99_utils.R")
# lagos geo module should have terrain metrics for mean slope
# Oliver (year) found that max slope was most informative

dt         <- read.csv("data/lagosus_depth.csv",
                    stringsAsFactors = FALSE)
bathy_pnts <- readRDS("data/00_bathy_depth/bathy_pnts.rds")
dt_pred    <- read.csv("data/lagosne_depth_predictors.csv",
                    stringsAsFactors = FALSE) %>%
  dplyr::filter(lagoslakeid %in% bathy_pnts$llid)


# attempt at manual calculation of max buffer slope
max_buffer_dist <- 100 # to match Hollister (2011)
set.seed(55)
ll_ids <- sample(dt_pred$lagoslakeid, 10)

get_slope <- function(ll_id){
  # ll_id <- ll_ids[2]
  print(ll_id)
  ll_poly <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", ll_id)
  ll_iws <- query_gis("IWS", "lagoslakeid", ll_id)

  fname <- paste0("data/elevatr/", ll_id, ".tif")
  if(!file.exists(fname)){
    elev      <- get_elev_raster(as_Spatial(ll_iws),
                                 13, clip = "bbox")
    writeRaster(elev, fname)
  }else{
    elev <- raster(fname)
  }

  # approximates functions in lakemorpho package
  elev      <- mask(elev, as_Spatial(ll_iws))
  elev      <- mask(elev, as_Spatial(st_buffer(ll_poly, max_buffer_dist)))
  slope     <- terrain(elev, "slope")

  slope_mean <- mean(slope@data@values, na.rm = TRUE)
  slope_max <- max(slope@data@values, na.rm = TRUE) # * res(elev)[2]
  slope_ne  <- dplyr::filter(dt_pred, lagoslakeid == ll_id) %>%
    dplyr::select(buffer100m_slope_mean, buffer100m_slope_max) %>%
    mutate_all(function(x) x / 10) # lagosne slopes are per 10m
  slope_ne  <- slope_ne[1,]

  # pull slope on-line from deepest point
  deepest_pnt         <- dplyr::filter(bathy_pnts, llid == ll_id)
  r <- raster(
    paste0("data/", tolower(deepest_pnt$state), "_bathy/", ll_id, ".tif")
    )
  st_crs(deepest_pnt) <- st_crs(r)

  shore_pnt <- sf::st_nearest_points(deepest_pnt, st_transform(
    st_cast(ll_poly, "MULTILINESTRING"), st_crs(r)))
  ll_buf <- st_buffer(ll_poly, 100)

  buffer_line <- sf::st_nearest_points(shore_pnt,
                                 st_transform(st_cast(ll_buf, "MULTILINESTRING"), st_crs(r)))
  slope_online <- raster::extract(slope, as_Spatial(buffer_line))
  slope_online <- mean(slope_online[[1]], na.rm = TRUE)#  * res(slope)[2]

  res <- list(slope_mean = slope_mean, slope_max = slope_max,
       slope_max_ne = slope_ne$buffer100m_slope_max,
       slope_mean_ne = slope_ne$buffer100m_slope_mean,
       slope_online = slope_online)
  res <- bind_rows(res) %>%
    mutate(llid = ll_id)
  res
}

test <- lapply(ll_ids, get_slope)
