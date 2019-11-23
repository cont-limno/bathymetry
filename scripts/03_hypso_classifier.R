source("scripts/99_utils.R")

# load merged hypso data
dt <- read.csv("data/00_hypso/hypso.csv", stringsAsFactors = FALSE)

# round the x value and test if y is greater or less than a straight-line curve
hypso_classes <- dt %>%
  group_by(llid) %>%
  summarize_all(median) %>%
  mutate(area_percent_rounded = round(area_percent)) %>%
  left_join(data.frame(y = rev(seq(0, 100, by = 1)), x = seq(0, 100, by = 1)),
            by = c("area_percent_rounded" = "x")) %>%
  data.frame() %>%
  mutate(shape_class = case_when(depth_percent < y ~ "convex",
                                 depth_percent > y ~ "concave",
                                 TRUE ~ "neither")) %>%
  ungroup() %>%   dplyr::filter(!is.na(shape_class)) %>%
  dplyr::select(llid, shape_class)

# save csv with llid, bowl_shaped_dummy_variable
