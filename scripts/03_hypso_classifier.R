source("scripts/99_utils.R")

# load merged hypso data
dt_raw <- read.csv("data/00_hypso/hypso.csv", stringsAsFactors = FALSE)

# round the x value and test if y is greater or less than a straight-line curve
hypso_classes <- dt %>%
  dplyr::select(-state) %>%
  group_by(llid) %>%
  summarize_all(median) %>%
  left_join(dplyr::select(dt_raw, llid, state)) %>%
  mutate(area_percent_rounded = round(area_percent)) %>%
  left_join(data.frame(y = rev(seq(0, 100, by = 1)), x = seq(0, 100, by = 1)),
            by = c("area_percent_rounded" = "x")) %>%
  data.frame() %>%
  mutate(offset = depth_percent - y,
         shape_class = case_when(depth_percent < y ~ "convex",
                                 depth_percent > y ~ "concave",
                                 TRUE ~ "neither")) %>%
  ungroup() %>%  dplyr::filter(!is.na(shape_class))

# qa outliers
if(interactive()){
  distinct(hypso_classes, llid, offset, .keep_all = TRUE) %>%
  dplyr::filter(state == "MI") %>%
    arrange(offset) %>%
    head()
  plot(raster("data/mi_bathy/1558.tif"))
  # convex

  distinct(hypso_classes, llid, offset, .keep_all = TRUE) %>%
    dplyr::filter(state == "MI") %>%
    arrange(desc(offset)) %>%
    head()
  mapview::mapview(raster("data/mi_bathy/4225.tif"))
  # concave
}

res <- hypso_classes %>%
  dplyr::select(llid, shape_class)

# save csv with llid, bowl_shaped_dummy_variable
write.csv(res, "data/00_hypso/hypso_classes.csv", row.names = FALSE)

