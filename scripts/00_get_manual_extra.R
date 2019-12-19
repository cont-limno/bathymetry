source("scripts/99_utils.R")
# merge data in Depth_Collection_Extra

locus <- lagosus_load("locus")$locus$locus_characteristics

# files <- c("co_cpw", "ct_healy", "ks_befs", "mt_fwp", "nc_deq", "nd_gfapps",
#            "nm_env", "ut_deq")
# lapply(files, function(x) drive_download(file = x,
#                path = paste0("data/00_manual_extra/", x, ".csv"),
#                overwrite = TRUE))

flist <- list.files("data/00_manual_extra/", pattern = ".csv",
                    full.names = TRUE, include.dirs = TRUE)
dt_raw <- lapply(flist, function(x) read.csv(x, stringsAsFactors = FALSE))

dt  <- dplyr::bind_rows(dt_raw) %>%
  dplyr::filter(!is.na(max_depth_m) | !is.na(mean_depth_m) |
                !is.na(max_depth_ft) | !is.na(mean_depth_ft))
dt  <- convert_ft_m(dt)
dt  <- left_join(dt, dplyr::select(locus, lagoslakeid, lake_waterarea_ha,
                                    lake_connectivity_permanent),
                 by = c("llid" = "lagoslakeid"))
dt <- dt[,!duplicated(names(dt))] %>%
  mutate(legacy_name = NA) %>%
  mutate(effort = "manual") %>%
  rename(source = url) %>%
  select(llid, name, legacy_name, state,
         max_depth_m, mean_depth_m, source,
         lake_waterarea_ha, lake_connectivity_permanent, effort,
         lat, long = lon)
dt  <- rm_dups(dt)

write.csv(dt, "data/00_manual_extra/00_manual_extra.csv", row.names = FALSE)
