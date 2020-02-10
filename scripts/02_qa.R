# setwd("../")
source("scripts/99_utils.R")

lg        <- lagosus_load(module = "locus")

# ---- remove obs that seem wrong in lakes with repeated measures ----
diff_threshold    <- 8
percent_threshold <- 0.3

dt_raw        <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE)
has_limno_ids <- dt_raw %>%
  dplyr::filter(has_limno == 1) %>%
  distinct(lagoslakeid) %>%
  pull(lagoslakeid)
dt_raw        <- dt_raw %>%
  dplyr::filter(!is.na(lake_maxdepth_m))

calc_diff_metrics <- function(dt, field = "lake_maxdepth_m"){
  dt %>%
  group_by(lagoslakeid) %>%
    add_tally() %>%
    mutate(diff = (max(UQ(rlang::sym(field))) - min(UQ(rlang::sym(field)))),
           percent = diff / max(UQ(rlang::sym(field)))) %>%
    mutate(diff = case_when(n < 2 ~ 0,
                            TRUE ~ diff),
           percent = case_when(n < 2 ~ 0,
                               TRUE ~ percent)) %>%
    arrange(desc(diff))
}

dt     <- dt_raw %>%
  calc_diff_metrics()

# remove NLA first because we're calling it the most unreliable
dt     <- dt %>%
  ungroup(dt) %>%
  dplyr::filter(!(lagos_effort == "NLA" & percent > percent_threshold)) %>%
  dplyr::filter(!(lagos_effort == "NLA" & diff > diff_threshold)) %>%
  dplyr::select(-n, -diff, -percent) %>%
  calc_diff_metrics()

# remove LAGOSNE second because we're calling it the second most unreliable
dt     <- dt %>%
  ungroup(dt) %>%
  dplyr::filter(!(lagos_effort == "LAGOSNE" & percent > percent_threshold)) %>%
  dplyr::filter(!(lagos_effort == "LAGOSNE" & diff > diff_threshold)) %>%
  dplyr::select(-n, -diff, -percent) %>%
  calc_diff_metrics()

# remove manual third because we assume that NHD hi-res polygons are correct
dt     <- dt %>%
  ungroup(dt) %>%
  dplyr::filter(!(lagos_effort == "manual" & percent > percent_threshold)) %>%
  dplyr::filter(!(lagos_effort == "manual" & diff > diff_threshold)) %>%
  dplyr::select(-n, -diff, -percent) %>%
  calc_diff_metrics()

# TODO: make sure max depth is at least equal to the maximum sample depth from limno

# ---- add back in lakes with missing depth from locus
lg_missing <- lg$locus$locus_information %>%
  dplyr::select(lagoslakeid, lake_namegnis, lake_states,
                lake_state = lake_centroidstate,
                lake_lat_decdeg, lake_lon_decdeg) %>%
  dplyr::left_join(dplyr::select(lg$locus$locus_characteristics,
                                 lagoslakeid, lake_waterarea_ha)) %>%
  dplyr::filter(!(lagoslakeid %in% dt$lagoslakeid))

res <- bind_rows(dt, lg_missing) %>%
  dplyr::select(-n, -diff, -percent) %>%
  mutate(has_limno = case_when(
    lagoslakeid %in% has_limno_ids ~ 1,
    TRUE ~ 0))

# any(!is.na(res$lake_maxdepth_m) & res$has_limno == 1) # TRUE

write.csv(res, "data/lagosus_depth.csv", row.names = FALSE)
# res <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE)

## lakes depth to water area ratio test
# mutate(dt, da_ratio = lake_maxdepth_m / lake_waterarea_ha) %>%
#   arrange(da_ratio) %>%
#   # dplyr::filter(lagos_effort == "bathymetry") %>%
#   dplyr::filter(lake_waterarea_ha >= 20) %>%
#   View()
# plot(dt$lake_maxdepth_m, dt$lake_waterarea_ha)
#
# lg_ne <- LAGOSNE::lagosne_load()
# dt <- left_join(lg_ne$locus, lg_ne$iws, by = "lagoslakeid")
# dt %>%
#   mutate(a_ratio = iws_ha / lake_area_ha) %>%
#   arrange(desc(a_ratio)) %>%
#   # dplyr::filter(str_detect(gnis_name, "Truman")) %>%
#   View()
#
# lg <- LAGOSUS::lagosus_load("locus")
# left_join(lg$locus$locus_characteristics, lg$locus$locus_nws) %>%
#   left_join(lg$locus$locus_information) %>%
#   mutate(a_ratio = nws_area_ha / lake_waterarea_ha) %>%
#   arrange(desc(a_ratio)) %>%
#   dplyr::filter(lake_waterarea_ha > 20 & !is.na(lake_centroidstate)) %>%
#   dplyr::select(-contains("zoneid")) %>%
#   # dplyr::filter(lake_centroidstate == "MO") %>%
#   arrange(desc(nws_area_ha)) %>%
#   View()
