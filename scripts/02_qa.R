# setwd("../")
source("scripts/99_utils.R")

lg        <- lagosus_load(module = "locus")

# ---- remove obs that seem wrong in lakes with repeated measures ----
threshold <- 0.4

dt_raw <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  dplyr::filter(!is.na(lake_maxdepth_m))
dt     <- dt_raw %>%
  group_by(lagoslakeid) %>%
  add_tally() %>%
  mutate(diff = (max(lake_maxdepth_m) - min(lake_maxdepth_m)) /
           max(lake_maxdepth_m)) %>%
  mutate(diff = case_when(n < 2 ~ 0,
                          TRUE ~ diff)) %>%
  arrange(desc(diff))

# remove NLA first because we're calling it the most unreliable
dt     <- dt %>%
  ungroup(dt) %>%
  dplyr::filter(!(lagos_effort == "NLA" & diff > threshold)) %>%
  dplyr::select(-n, -diff) %>%
  group_by(lagoslakeid) %>%
  add_tally() %>%
  mutate(diff = (max(lake_maxdepth_m) - min(lake_maxdepth_m)) /
           max(lake_maxdepth_m)) %>%
  mutate(diff = case_when(n < 2 ~ 0,
                          TRUE ~ diff)) %>%
  arrange(desc(diff))

# remove LAGOSNE second because we're calling it the second most unreliable
dt     <- dt %>%
  ungroup(dt) %>%
  dplyr::filter(!(lagos_effort == "LAGOSNE" & diff > threshold)) %>%
  dplyr::select(-n, -diff) %>%
  group_by(lagoslakeid) %>%
  add_tally() %>%
  mutate(diff = (max(lake_maxdepth_m) - min(lake_maxdepth_m)) /
           max(lake_maxdepth_m)) %>%
  mutate(diff = case_when(n < 2 ~ 0,
                          TRUE ~ diff)) %>%
  arrange(desc(diff))

# remove bathymetry third because this is likely due to NHD hi-res polygons
# being mismatched to gnis names?
# dt <- dt %>%
#   ungroup(dt) %>%
#   dplyr::filter(!(lagos_effort == "bathymetry" & diff > threshold)) %>%
#   dplyr::select(-n, -diff) %>%
#   group_by(lagoslakeid) %>%
#   add_tally() %>%
#   mutate(diff = (max(lake_maxdepth_m) - min(lake_maxdepth_m)) /
#            max(lake_maxdepth_m)) %>%
#   mutate(diff = case_when(n < 2 ~ 0,
#                           TRUE ~ diff)) %>%
#   arrange(desc(diff))

# hist(dt$diff[dt$diff != 0])

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
  dplyr::select(-n, -diff)

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
