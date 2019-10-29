## create logs for individual state for lakes without wq data
library(dplyr)

target_state <- "NC"
file_name    <- "nc_deq.csv"

# pull locus
ls_raw <- read.csv("data/00_lagosus_locus/lake_information_20190913.csv",
               stringsAsFactors = FALSE)

# pull manual log
log <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE)

# remove dups
ls <- dplyr::filter(ls_raw, !(lagoslakeid %in% log$llid)) %>%
  dplyr::filter(lake_centroidstate == target_state) %>%
  # order by lake name
  arrange(desc(lake_namegnis)) %>%
  # add fields
  mutate(rowid = seq_len(nrow(.)),
         max_depth_ft = NA, mean_depth_ft = NA,
         lakename_google = NA, max_depth_m = NA, mean_depth_m = NA,
         url = NA) %>%
  dplyr::select(llid = lagoslakeid, rowid, lat = lake_lat_decdeg,
                lon = lake_lon_decdeg, name = lake_namegnis,
                state = lake_centroidstate, max_depth_ft, mean_depth_ft,
                lakename_google, max_depth_m, mean_depth_m,
                url) %>%
  # TODO distinct !is.na(name) but keep all blanks
  distinct(lat, .keep_all = TRUE)


# export to csv
write.csv(ls, paste0("data/99_depth_collection_extra/", file_name),
          row.names = FALSE)

# write to GDrive
googledrive::drive_upload(media = paste0("data/99_depth_collection_extra/", file_name),
    path = paste0("Continental-limnology/HUBS/LIMNO-Hub/LAGOS-lake-depth/",
    "LAGOSUS-LakeDepth/Depth_Collection_Extra/", file_name))
