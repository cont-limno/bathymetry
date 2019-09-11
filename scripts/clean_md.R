library(googledrive)

drive_download("DEDUPED_MD_MDE_sites_v1_linked_R", type = "csv",
               path = "data/md.csv", overwrite = TRUE)

dt <- read.csv("data/md.csv", stringsAsFactors = FALSE) %>%
  dplyr::filter(!duplicated(Linked_lagoslakeid)) %>%
  dplyr::filter(!is.na(Linked_lagoslakeid)) %>%
  dplyr::select(Linked_lagoslakeid, lat = LatitudeMeasure,
                lon = LongitudeMeasure, name = MonitoringLocationName) %>%
  mutate(filename = "DEDUPED_MD_MDE_sites_v1_linked_R",
         state = "MD") %>%
  write.csv("test.csv", row.names = FALSE)


