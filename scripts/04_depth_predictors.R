source("scripts/99_utils.R")

# unlink("data/lagosus_depth_predictors.csv")
lg        <- lagosus_load(modules = "locus")
lg_ne     <- lagosne_load()
# need x_walk to join lagosne slope data
lg_x_walk <- lagosus_load(modules = "locus")$locus$locus_link %>%
  dplyr::select(lagosne_lagoslakeid, lagoslakeid, lagosus_centroidstate) %>%
  dplyr::rename(lagosus_lagoslakeid = lagoslakeid) %>%
  distinct(lagosus_lagoslakeid, .keep_all = TRUE)
hypso_classes <- read.csv(
  "data/00_hypso/hypso_classes.csv", stringsAsFactors = FALSE)

dt_raw <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  dplyr::left_join(hypso_classes, by = c("lagoslakeid" = "llid")) %>%
  dplyr::select(-contains("waterarea")) %>%
  dplyr::filter(!is.na(lake_maxdepth_m)) %>%
  left_join(lg$locus$locus_characteristics,
            by = "lagoslakeid") %>%
  left_join(dplyr::select(lg$locus$locus_information,
                          -matches("\\b(?!hu4)\\w*zoneid\\b", perl = TRUE),
                          # -contains("name"),
                          -contains("reachcode"),
                          -lake_states, -contains("decdeg"),
                          -contains("namegnis")),
                          by = "lagoslakeid") %>%
  left_join(dplyr::select(lg$locus$locus_nws, contains("area"),
                          contains("perimeter"), contains("width"),
                          contains("length"), contains("orientation"),
                          "lagoslakeid"),
                          by = c("lagoslakeid")) %>%
  left_join(dplyr::select(lg$locus$locus_ws, contains("area"),
                          contains("perimeter"), contains("width"),
                          contains("length"), contains("orientation"),
                          "lagoslakeid"),
            by = c("lagoslakeid")) %>%
  left_join(lg_x_walk,
            by = c("lagoslakeid" = "lagosus_lagoslakeid")) %>%
  left_join(dplyr::select(lg_ne$buffer100m.lulc, lagoslakeid,
                          buffer100m_slope_max, buffer100m_slope_mean),
            by = c("lagosne_lagoslakeid" = "lagoslakeid")) %>%
  # remove duplicate columns in original depth product
  dplyr::select(-contains("_lat_"), -contains("_lon_"),
                -contains("program"), -contains("effort"),
                -contains("gnis"), -contains("states"),
                -contains("border"), -contains("namelagos"),
                -contains("predicted"))
# fill missing nws data with ws data?

# add reservoir class labels
dt_raw <- dt_raw %>%
  left_join(dplyr::select(read.csv(
  "data/00_reservoir_classification/reservoir_classes_clean.csv",
  stringsAsFactors = FALSE), lagoslakeid, reservoir_class), by = "lagoslakeid")

# add geometry data
bathy_pnts <- read.csv("data/00_bathy_depth/bathy_geometry.csv",
                       stringsAsFactors = FALSE)
dt_raw <- dt_raw %>%
  left_join(dplyr::select(bathy_pnts, dist_deepest:inlake_slope, llid),
            by = c("lagoslakeid" = "llid"))

write.csv(dt_raw, "data/lagosus_depth_predictors.csv", row.names = FALSE)
# dt_raw <- read.csv("data/lagosus_depth_predictors.csv", stringsAsFactors = FALSE)

dt_raw_ne <- dplyr::filter(dt_raw, !is.na(buffer100m_slope_max))
write.csv(dt_raw_ne, "data/lagosne_depth_predictors.csv", row.names = FALSE)

if(interactive()){

# ---- jsta-random-shape-class-slopes ----

  # depth ~ max_slope | shape_class
  # or
  # # after getting distance from the deepest point to the shore
  # inlake_slope ~ max_slope | shape_class
  # depth = distance * tan(inlake_slope)


}
