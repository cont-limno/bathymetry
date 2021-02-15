# setwd("../")
# From Dropbox: contlimno2 > manuscripts > RSVRs (or Reservoirs)

library(readxl)
library(dplyr)

lg <- LAGOSUS::lagosus_load("locus")

manual_class_raw <- readxl::read_excel(
  "data/00_reservoir_classification/LAGOSNE_MANUAL_CLASSIFICATIONS_V1.0.xlsx") %>%
  janitor::clean_names() %>%
  rename(reservoir_class = lake_type) %>%
  mutate(reservoir_class = case_when(
    reservoir_class == "RSVR" ~ "Res",
    TRUE ~ reservoir_class))

pred_class_raw <- readxl::read_excel(
  "data/00_reservoir_classification/LAGOSNE_PREDICTION_V1.0.xlsx") %>%
  janitor::clean_names() %>%
  dplyr::filter(confidence >= 0.75) %>%
  rename(reservoir_class = prediction)

dt <- dplyr::bind_rows(manual_class_raw, pred_class_raw) %>%
  arrange(lagoslakeid) %>%
  rename(lagosne_lagoslakeid = lagoslakeid) %>%
  left_join(dplyr::select(lg$locus$lake_link, lagoslakeid, lagosne_lagoslakeid),
                          by = "lagosne_lagoslakeid") %>%
  dplyr::filter(!is.na(lagoslakeid)) %>%
  distinct(lagoslakeid, .keep_all = TRUE) %>%
  dplyr::select(lagoslakeid, reservoir_class)

write.csv(dt,
          "data/00_reservoir_classification/reservoir_classes_clean.csv",
          row.names = FALSE)
