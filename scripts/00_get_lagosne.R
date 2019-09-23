source("scripts/99_utils.R")

lg <- lagosne_load("1.087.3")

# anticipated columns:
# llid, name, legacy_name, state, max_depth_m, mean_depth_m, source, lat, long
res <- mutate(lg$lakes_limno,
       legacy_name = NA) %>%
  left_join(dplyr::select(lg$locus, lagoslakeid, gnis_name, state_zoneid)) %>%
  left_join(dplyr::select(lg$state, state, state_zoneid)) %>%
dplyr::select(lagoslakeid, name = gnis_name, state,
              max_depth_m = maxdepth, mean_depth_m = meandepth,
              source = maxdepthsource, lat = nhd_lat, long = nhd_long) %>%
  # TODO: eventually cross-walk lagosne llids with lagosus llids
  dplyr::mutate(lagoslakeid = NA) %>%
  dplyr::filter(!is.na(max_depth_m) | !is.na(mean_depth_m))

write.csv(res, "data/00_lagosne/00_lagosne.csv", row.names = FALSE)
