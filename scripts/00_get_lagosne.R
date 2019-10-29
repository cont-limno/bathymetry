source("scripts/99_utils.R")

lg <- lagosne_load("1.087.3")

# anticipated columns:
# llid, name, legacy_name, state, max_depth_m, mean_depth_m, source, source_type, lat, long
res <- mutate(lg$lakes_limno,
       legacy_name = NA) %>%
  dplyr::filter(!is.na(maxdepth) | !is.na(meandepth)) %>%
  left_join(dplyr::select(lg$locus, lagoslakeid, gnis_name, state_zoneid,
                          lake_area_ha)) %>%
  left_join(dplyr::select(lg$state, state, state_zoneid)) %>%
  left_join(dplyr::select(lg$lakes.geo, lagoslakeid, lakeconnection)) %>%
  left_join(dplyr::select(lg$buffer100m.lulc,
                          buffer100m_slope_mean, lagoslakeid)) %>%
  # rename to lagosus conny codes
  mutate(lakeconnection = case_when(lakeconnection == "DR_Stream" ~ "Drainage",
                                    lakeconnection == "DR_LakeStream" ~ "DrainageLk",
                                    TRUE ~ lakeconnection)) %>%
dplyr::select(llid = lagoslakeid, name = gnis_name, state,
              max_depth_m = maxdepth, mean_depth_m = meandepth,
              source = maxdepthsource, lake_waterarea_ha = lake_area_ha,
              buffer100m_slope_mean,
              lake_connectivity_permanent = lakeconnection,
              lat = nhd_lat, long = nhd_long) %>%
  left_join(dplyr::select(lg$lagos_source_program, programname, programtype),
                  by = c("source" = "programname")) %>%
  rename(source_type = programtype) %>%
  # TODO: eventually cross-walk lagosne llids with lagosus llids
  # for now: limit results to those with WQ data in epi_nutr
  # dplyr::filter(llid %in% lg$epi_nutr$lagoslakeid) %>%
  dplyr::mutate(# llid = NA,
                effort = "LAGOSNE") # %>%
  #dplyr::filter(!is.na(max_depth_m) | !is.na(mean_depth_m))

write.csv(res, "data/00_lagosne/00_lagosne.csv", row.names = FALSE)
