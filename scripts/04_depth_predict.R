source("scripts/99_utils.R")

# unlink("data/lagosus_depth_predictors.csv")
if(!file.exists("data/lagosus_depth_predictors.csv")){
  lg        <- lagosus_load(modules = "locus")
  lg_ne     <- lagosne_load()
  lg_x_walk <- read.csv(
    "data/00_lagosus_locus/LAGOS_Lake_Link_v2_20191017.csv",
    stringsAsFactors = FALSE) %>%
    dplyr::select(lagosne_lagoslakeid, lagoslakeid, lagosus_centroidstate) %>%
    dplyr::rename(lagosus_lagoslakeid = lagoslakeid) %>%
    distinct(lagosus_lagoslakeid, .keep_all = TRUE)

  dt_raw <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
    dplyr::select(-contains("waterarea")) %>%
    dplyr::filter(!is.na(max_depth_m)) %>%
    left_join(lg$locus$locus_characteristics,
              by = c("llid" = "lagoslakeid")) %>%
    left_join(dplyr::select(lg$locus$locus_information,
                            -contains("zoneid"),
                            # -contains("name"),
                            -contains("reachcode")),
                            by = c("llid" = "lagoslakeid")) %>%
    left_join(dplyr::select(lg$locus$locus_nws, contains("area"),
                            contains("perimeter"), contains("width"),
                            contains("length"), contains("orientation"),
                            "lagoslakeid"),
                            by = c("llid" = "lagoslakeid")) %>%
    left_join(dplyr::select(lg$locus$locus_ws, contains("area"),
                            contains("perimeter"), contains("width"),
                            contains("length"), contains("orientation"),
                            "lagoslakeid"),
              by = c("llid" = "lagoslakeid")) %>%
    left_join(lg_x_walk,
              by = c("llid" = "lagosus_lagoslakeid")) %>%
    left_join(dplyr::select(lg_ne$buffer100m.lulc, lagoslakeid, buffer100m_slope_max),
              by = c("llid" = "lagoslakeid")) %>%
    # remove duplicate columns in original depth product
    dplyr::select(-lat, -long, -effort, -source_type, -source, -name,
                  -legacy_name, -state, -lake_states, -contains("border"),
                  -contains("namelagos"), -lagosus_centroidstate)
  # fill missing nws data with ws data?

  dt_raw_ne <- dplyr::filter(dt_raw, !is.na(buffer100m_slope_max))

  write.csv(dt_raw, "data/lagosus_depth_predictors.csv", row.names = FALSE)
  write.csv(dt_raw_ne, "data/lagosne_depth_predictors.csv", row.names = FALSE)
}else{
  dt_raw <- read.csv("data/lagosus_depth_predictors.csv",
                     stringsAsFactors = FALSE)
  dt_raw_ne <- read.csv("data/lagosne_depth_predictors.csv",
                     stringsAsFactors = FALSE)
}


if(interactive()){
  library(corrr)
  library(pheatmap)

  pretty_names <- gsub("mbgconhull_", "", names(dt_raw_ne))
  pretty_names <- gsub("lake_", "", pretty_names)
  pretty_names <- gsub("_nounits", "", pretty_names)
  pretty_names <- gsub("focallake", "", pretty_names)

  test <- dt_raw_ne %>%
    setNames(pretty_names) %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select(-llid, -contains("decdeg"),
                  -contains("nhd"),
                  -contains("lagoslakeid"),
                  -huc12) %>%
    correlate() %>%
    data.frame()
  test_rowname    <- test$rowname
  test            <- dplyr::select(test, -rowname)
  row.names(test) <- test_rowname

  pheatmap(t(test),
          color = colorRampPalette(
            RColorBrewer::brewer.pal(n = 7, name = "Reds"))(100))
}
