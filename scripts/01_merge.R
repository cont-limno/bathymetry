source("scripts/99_utils.R")
# merge manual, nla, and lagosne data

lg               <- lagosus_load(module = "locus")
manual_raw       <- read.csv(
  "data/00_manual/00_manual.csv", stringsAsFactors = FALSE)
manual_extra_raw <- read.csv(
  "data/00_manual_extra/00_manual_extra.csv", stringsAsFactors = FALSE)
nla_raw          <- read.csv(
  "data/00_nla/00_nla.csv", stringsAsFactors = FALSE)
lagosne_raw      <- read.csv(
  "data/00_lagosne/00_lagosne.csv", stringsAsFactors = FALSE)

# quality assurance
res_raw <- dplyr::bind_rows(manual_raw, manual_extra_raw,
                            nla_raw, lagosne_raw) %>%
  dplyr::filter(is.na(max_depth_m) | is.na(mean_depth_m) |
                  max_depth_m != mean_depth_m) %>%
  dplyr::filter(is.na(max_depth_m) | is.na(mean_depth_m) |
                  max_depth_m > mean_depth_m) %>%
  dplyr::filter(max_depth_m >= 0.3048 | is.na(max_depth_m))

# source classification
res_raw <- res_raw %>%
  mutate(source = urltools::domain(source)) %>%
  mutate(source_type = case_when(
    is.na(source_type) & grepl("\\.gov|\\.us$|dnr|dwq|dep", source) ~ "Government",
    is.na(source_type) & grepl("\\.edu", source) ~ "University",
    !is.na(source_type) ~ source_type,
    TRUE ~ NA_character_)) %>%
  mutate(source_type = case_when(
    grepl("Mix", source_type) | is.na(source_type) ~ "Misc",
    grepl("Agency", source_type) | is.na(source_type) ~ "Government",
    !is.na(source_type) ~ source_type))

# column naming and arrangement
res <- res_raw %>%
  dplyr::select(llid, name, legacy_name, state, max_depth_m,
                mean_depth_m, source, source_type, effort, lat, long, lake_waterarea_ha) %>%
  left_join(lg$locus$locus_information, by = c("llid" = "lagoslakeid"))

res <- res %>%
  dplyr::select(lagoslakeid = llid, lake_namegnis, lake_states,
                lake_state = state,
                lake_lat_decdeg, lake_lon_decdeg,
                lake_maxdepth_m = max_depth_m,
                lake_meandepth_m = mean_depth_m,
                lake_waterarea_ha, programtype_depth = source_type,
                programlink_depth = source, lagos_effort = effort) %>%
  mutate(predicted_maxdepth_m = NA)

# keep duplicates with unique max_depth_m values
res       <- res %>%
  data.frame() %>% group_by(lagoslakeid) %>% add_count() %>%
  arrange(lagoslakeid, desc(n)) %>%
  ungroup()
res$n_na  <- rowSums(is.na(res))
res_final <- res %>%
  group_by(lagoslakeid) %>%
  arrange(desc(n), lagoslakeid, n_na) %>%
  distinct(lagoslakeid, lake_maxdepth_m, .keep_all = TRUE) %>%
  dplyr::select(-n, -n_na) %>%
  arrange(lagoslakeid)

write.csv(res_final, "data/lagosus_depth.csv", row.names = FALSE)
# res <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE)

# write to GDrive
# googledrive::drive_upload(media = "data/lagosus_depth.csv",
# path = "Continental-limnology/HUBS/LIMNO-Hub/LAGOS-lake-depth/LAGOSUS-LakeDepth/depth_merged.csv")
