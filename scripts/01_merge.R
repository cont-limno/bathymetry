source("scripts/99_utils.R")

# merge manual, nla, and lagosne data

# anticipated columns:
# llid, name, legacy_name, state, max_depth_m, mean_depth_m, source, lat, long

manual_raw  <- read.csv("data/00_manual/00_manual.csv", stringsAsFactors = FALSE)
nla_raw     <- read.csv("data/00_nla/00_nla.csv", stringsAsFactors = FALSE)
lagosne_raw <- read.csv("data/00_lagosne/00_lagosne.csv", stringsAsFactors = FALSE)

res <- dplyr::bind_rows(manual_raw,
                        nla_raw,
                        lagosne_raw) %>%
  dplyr::filter(!is.na(max_depth_m) | !is.na(mean_depth_m))

write.csv(res, "data/lagosus_depth.csv", row.names = FALSE)

# write to GDrive
# googledrive::drive_upload(media = "data/lagosus_depth.csv",
# path = "Continental-limnology/HUBS/LIMNO-Hub/LAGOS-lake-depth/LAGOSUS-LakeDepth/depth_merged.csv")
