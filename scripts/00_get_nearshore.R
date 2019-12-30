source("scripts/99_utils.R")
# lagos geo module should have terrain metrics for mean slope
# Oliver (year) found that max slope was most informative

dt      <- read.csv("data/lagosus_depth.csv",
                    stringsAsFactors = FALSE)
dt_pred <- read.csv("data/lagosus_depth_predictors.csv",
                    stringsAsFactors = FALSE)

# attempt at manual calculation of max buffer slope
max_buffer_dist <- 100 # to match Hollister (2011)
llid            <- 4040

# pull lake outline with LAGOSNEgis
ll_poly <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", llid)
# pull lake iws
ll_iws <- query_gis("IWS", "lagoslakeid", llid)

# pull buffer with elevatr
if(!file.exists("elev.tif")){
  elev      <- get_elev_raster(as_Spatial(ll_iws),
                          13, clip = "bbox")
  writeRaster(elev, "elev.tif")
}else{
  elev <- raster("elev.tif")
}

# approximates functions in lakemorpho package
elev      <- mask(elev, as_Spatial(ll_iws))
elev      <- mask(elev, as_Spatial(st_buffer(ll_poly, max_buffer_dist)))
slope     <- terrain(elev, "slope")@data@values
slope_med <- median(slope, na.rm = TRUE)

max(slope, na.rm = TRUE) * res(elev)[2]
dplyr::filter(dt_pred, lagoslakeid == llid) %>%
  pull(buffer100m_slope_max) / 10 # lagosne slopes are per 10m

# 3.63 vs 3.29 lagosne
