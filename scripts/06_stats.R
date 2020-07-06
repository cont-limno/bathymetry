source("scripts/99_utils.R")

# ---- What percentage of lakes are in each shape class? ----
dt <- read.csv("data/00_hypso/hypso_classes.csv", stringsAsFactors = FALSE)
table(dt$shape_class)[1] / sum(table(dt$shape_class)) * 100
table(dt$shape_class)[2] / sum(table(dt$shape_class)) * 100

# ---- What percentage of lakes are in each reservoir class? ----
dt <- read.csv("data/00_reservoir_classification/reservoir_classes_clean.csv",
               stringsAsFactors = FALSE)
table(dt$reservoir_class)[1] / sum(table(dt$reservoir_class))
table(dt$reservoir_class)[2] / sum(table(dt$reservoir_class))

# ---- What are the summary stats of the papers looked at by Hakanson (1977)? ----
hak_areas <- as.numeric(c("821000", "598000", "578000", "25700", "19000", "5650",
                          "3580", "2070", "1910", "1140", "484", "128",
                          "94.7", "92.0", "63.8", "54.2", "23.8", "18.6",
                          "16.0", "14.9", "10.0", "7.10", "5.15", "4.96",
                          "3.88", "3.28", "2.80", "2.66", "2.00", "1.90",
                          "1.80", "1.60", "1.59", "1.47", "1.13", "1.1",
                          "1.02", "0.91", "0.87", "0.84", "0.57", "0.56", "
             0.36", "0.32", "0.31", "0.17", "0.097", "0.029")) *
  100 # convert from km2 to ha
quantile(hak_areas,
         probs = c(0.25, 0.5, 0.75))
hist(hak_areas, n = 80)

# ---- How related are true and proxy geometry metrics? ----
bp <- readRDS("data/00_bathy_depth/bathy_pnts.rds")
dt <- read.csv("data/00_geometry/nearshore.csv", stringsAsFactors = FALSE) %>%
  mutate(llid = as.character(llid))
dt <- left_join(dt, st_drop_geometry(bp),
                by = "llid") %>%
  dplyr::filter(!is.na(inlake_slope_mean))
lgus_pred <- read.csv("data/lagosus_depth_predictors.csv",
                      stringsAsFactors = FALSE) %>%
  mutate(lagoslakeid = as.character(lagoslakeid)) %>%
  dplyr::select(-contains("dist"))
dt <- left_join(dt, lgus_pred, by = c("llid" = "lagoslakeid")) %>%
  dplyr::filter(!is.na(shape_class)) %>%
  dplyr::filter(inlake_slope < 0.4)

broom::glance(lm(dist_viscenter~dist_deepest, data = dt))$r.squared # 0.80
broom::glance(lm(inlake_slope ~ slope_mean, data = dt))$r.squared # 0.17

# percent obs where nearshore land slope > in-lake slope
sum(dt$slope_mean > dt$inlake_slope, na.rm = TRUE) /
  nrow(dt[complete.cases(dt[,c("inlake_slope", "slope_mean")]),]) * 100

# ---- the percentage of lakes in the study footprint with bathy maps ----
# pull bathy llids + states
bd <- read.csv("data/00_bathy_depth/00_bathy_depth.csv",
               stringsAsFactors = FALSE)

# pull list of llids > 4 ha in footprint
lg_raw <- lagosus_load("locus")
lg <- lg_raw$locus$locus_information %>%
  dplyr::filter(lake_centroidstate %in% unique(bd$state)) %>%
  left_join(lg_raw$locus$locus_characteristics) %>%
  dplyr::filter(lake_waterarea_ha >= 4)

(nrow(bd) / nrow(lg)) * 100 # 15%

# ---- the percentage of lakes in the study footprint with depth data ----
# pull lagosus depth
lg_depth <- lagosus_load(modules = "depth", versions = 0)$depth$depth %>%
  dplyr::filter(lake_state %in% unique(bd$state))

# pull list of llids > 4 ha in footprint
lg_raw <- lagosus_load("locus")
lg <- lg_raw$locus$locus_information %>%
  dplyr::filter(lake_centroidstate %in% unique(bd$state)) %>%
  left_join(lg_raw$locus$locus_characteristics) %>%
  dplyr::filter(lake_waterarea_ha >= 4)

(nrow(lg_depth) / nrow(lg)) * 100 # 25%
