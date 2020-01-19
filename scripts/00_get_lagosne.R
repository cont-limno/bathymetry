source("scripts/99_utils.R")

lg_ne        <- lagosne_load("1.087.3")
lg           <- lagosus_load(modules = "locus")
lg_xwalk <- read.csv("data/00_lagosne/00_lagosne_xwalk.csv",
                     stringsAsFactors = FALSE) %>%
  dplyr::select(lagosne_lagoslakeid, lagoslakeid,
                lagosus_centroidstate = lake_centroidstate) %>%
  dplyr::rename(lagosus_lagoslakeid = lagoslakeid) %>%
  distinct(lagosne_lagoslakeid, .keep_all = TRUE)

# anticipated columns:
# llid, name, legacy_name, state, max_depth_m, mean_depth_m, source, source_type, lat, long
res <- mutate(lg_ne$lakes_limno,
       legacy_name = NA) %>%
  # dplyr::filter(!is.na(maxdepth) | !is.na(meandepth)) %>%
  left_join(dplyr::select(lg_ne$locus, lagoslakeid, gnis_name, state_zoneid,
                          lake_area_ha)) %>%
  left_join(dplyr::select(lg_ne$state, state, state_zoneid)) %>%
  left_join(dplyr::select(lg_ne$lakes.geo, lagoslakeid, lakeconnection)) %>%
  left_join(dplyr::select(lg_ne$buffer100m.lulc,
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
  left_join(dplyr::select(lg_ne$lagos_source_program, programname, programtype),
                  by = c("source" = "programname")) %>%
  rename(source_type = programtype,
         lagosne_lagoslakeid = llid) %>%
  left_join(lg_xwalk, by = "lagosne_lagoslakeid") %>%
  rename(llid = lagosus_lagoslakeid) %>%
  dplyr::filter(!is.na(llid)) %>%
  # join replace state and coords with lagosus values
  left_join(dplyr::select(lg$locus$locus_information,
                    llid = lagoslakeid, lake_lat_decdeg, lake_lon_decdeg)) %>%
  dplyr::mutate(state = lagosus_centroidstate,
                lat = lake_lat_decdeg,
                long = lake_lon_decdeg) %>%
  dplyr::select(-lake_lat_decdeg, -lake_lon_decdeg) %>%
  mutate(has_limno = case_when(
     lagosne_lagoslakeid %in% unique(lg_ne$epi_nutr$lagoslakeid) ~ 1,
     TRUE ~ 0))

res2 <- rm_dups(res) %>%
    dplyr::mutate(effort = "LAGOSNE")

write.csv(res2, "data/00_lagosne/00_lagosne.csv", row.names = FALSE)
# res2 <- read.csv("data/00_lagosne/00_lagosne.csv", stringsAsFactors = FALSE)
