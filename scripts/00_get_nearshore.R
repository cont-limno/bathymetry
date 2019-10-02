source("scripts/99_utils.R")

dt              <- read.csv("data/lagosus_depth.csv",
                            stringsAsFactors = FALSE)

# lagos geo module should have terrain metrics for mea slope
# Oliver (year) found that max slope was most informative


# attempt at manual calculation follows:
max_buffer_dist <- 100 # to match Hollister (2011)

llid <- 4040 # test on one lake for now
# lake_info(llid)

# pull lake outline with LAGOSNEgis
ll_poly <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", llid)
# pull lake iws
ll_iws <- query_gis("IWS", "lagoslakeid", llid)

# pull buffer with elevatr
elev      <- get_elev_raster(as_Spatial(ll_iws),
                        13, clip = "bbox")

# TODO: cache get_elev_raster output because FedData results have terribly low res

# approximates functions in lakemorpho package
elev      <- mask(elev, as_Spatial(ll_iws))
elev      <- mask(elev, as_Spatial(st_buffer(ll_poly, max_buffer_dist)))
slope     <- terrain(elev, "slope")@data@values
slope_med <- median(slope, na.rm = TRUE)


library(lakemorpho)
test <- lakeSurroundTopo(as_Spatial(ll_poly), elev)


#   test <- mask(test, test <= (cellStats(test, "min") + 1), maskvalue = TRUE)
#   test <- test - cellStats(test, "min")
#   test <- resample(test, rsub)
#
#   test2 <- merge(test, rsub)
#   test2[is.na(test2)] <- 0


