# setwd("../")
source("scripts/99_utils.R")

# load merged hypso data
dt_raw <- read.csv("data/00_hypso/hypso.csv", stringsAsFactors = FALSE)

# round the x value and test if y is greater or less than a straight-line curve
hypso_classes <- dt_raw %>%
  dplyr::select(-state) %>%
  group_by(llid) %>%
  summarize_all(median) %>%
  mutate(area_percent_rounded = round(area_percent)) %>%
  left_join(data.frame(y = rev(seq(0, 100, by = 1)), x = seq(0, 100, by = 1)),
            by = c("area_percent_rounded" = "x")) %>%
  data.frame() %>%
  mutate(offset = depth_percent - y,
         shape_class = case_when(depth_percent < y ~ "convex",
                                 depth_percent > y ~ "concave",
                                 TRUE ~ "neither")) %>%
  ungroup() %>%  dplyr::filter(!is.na(shape_class)) %>%
  left_join(dplyr::distinct(
    dplyr::select(dt_raw, llid, state), llid, .keep_all = TRUE), by = "llid")

# qa outliers
if (interactive()) {
  distinct(hypso_classes, llid, offset, .keep_all = TRUE) %>%
  dplyr::filter(state == "MN") %>%
    dplyr::filter(shape_class == "neither") %>%
    # arrange(offset) %>%
    # dplyr::filter(llid %in% c(2063, 3082))
    head()
    # View()
  plot(raster("data/mn_bathy/2063.tif")) # convex
  plot(raster("data/mn_bathy/3082.tif")) # concave
  plot(raster("data/ma_bathy/28.tif")) # neither

  # verify hypsography
  dplyr::filter(dt_raw, llid %in% c(2063, 3082, 28)) %>%
    dplyr::mutate(llid = factor(llid)) %>%
    ggplot() +
    geom_point(aes(x = area_percent, y = depth_percent, color = llid))

  # closer look at the neither shape_class
  group_by(dt_raw, llid) %>%
    add_count() %>%
    distinct(llid, n) %>%
    left_join(hypso_classes) %>%
    dplyr::filter(shape_class == "neither") %>%
    View()
}

res <- hypso_classes %>%
  dplyr::select(llid, shape_class, shape_offset = offset)

# save csv with llid, bowl_shaped_dummy_variable
write.csv(res, "data/00_hypso/hypso_classes.csv", row.names = FALSE)
# res  <- read.csv("data/00_hypso/hypso_classes.csv", stringsAsFactors = FALSE)
