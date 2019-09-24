source("scripts/99_utils.R")

# TODO: make sure max depth is at least equal to the maximum sample depth from limno

dt <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  # ## max_depth_m > mean_depth_m
  dplyr::filter(max_depth_m != mean_depth_m) %>%
  dplyr::filter(!is.na(max_depth_m) & !is.na(mean_depth_m)) %>%
  dplyr::filter(max_depth_m > mean_depth_m)
