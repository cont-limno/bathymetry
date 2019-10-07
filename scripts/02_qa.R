source("scripts/99_utils.R")

# TODO: make sure max depth is at least equal to the maximum sample depth from limno

dt <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  # ## max_depth_m > mean_depth_m
  dplyr::filter(max_depth_m != mean_depth_m) %>%
  dplyr::filter(!is.na(max_depth_m) & !is.na(mean_depth_m)) %>%
  dplyr::filter(max_depth_m > mean_depth_m)

dt_bad <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  dplyr::filter(max_depth_m == mean_depth_m) %>%
  mutate(test = urltools::domain(source))

table(dt_bad$test)
jsta::pdf_table(knitr::kable(table(dt_bad$test)))
jsta::pdf_table(knitr::kable(t(table(dt_bad$state))))

