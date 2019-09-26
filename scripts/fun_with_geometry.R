library(LAGOSNE)
library(dplyr)

lg <- lagosne_load("1.087.3")
dt <- dplyr::filter(lg$lakes_limno,
                    !is.na(maxdepth) & !is.na(meandepth)) %>%
  dplyr::select(lagoslakeid, nhd_lat, nhd_long, maxdepth, meandepth) %>%
  dplyr::left_join(dplyr::select(lg$locus, lagoslakeid, lake_area_ha, gnis_name, state_zoneid)) %>%
  dplyr::left_join(dplyr::select(lg$state, state_zoneid, state)) %>%
  dplyr::filter(lake_area_ha >= 4 & lake_area_ha <= 400) %>%
  dplyr::filter(state == "MN") %>%
  dplyr::filter(meandepth != maxdepth) %>%
  dplyr::filter(meandepth > 1)

# using lake areas:
# calculate a vector of lake radii
dt <- mutate(dt, radius = sqrt(lake_area_ha / pi))
# TODO: instead of straight-up radii use half the fetch or half the minimum bounding width
# Kath did geoprocessing using ArcGIS v 10.5 with tool Minimum Bounding Geometry in Features in Data Management Tools (System Toolbox)

# calculate "slopes" as max_depth / r
dt <- mutate(dt, slope = maxdepth / radius)

# calculate "half-slopes" as mean_depth / (r/2)
# how do they compare with full "slopes"?
dt <- mutate(dt, half_slope = meandepth / (radius / 2))

plot(dt$slope, dt$half_slope)
abline(0, 1) # shallow slopes are shallower than the overall (deep) slope

# regress half-slopes against max depth
dt <- mutate(dt, maxdepth_pred = half_slope * radius)
plot(dt$maxdepth_pred, dt$maxdepth)
abline(0, 1) # using shallow slopes underestimates maxdepth

# wetzel (2001) says that mean:max ratio is usually > 0.33 which is the value for a perfect cone
dt <- mutate(dt, depth_ratio = meandepth / maxdepth) %>%
  arrange(desc(depth_ratio))
hist(dt$depth_ratio)
abline(v = 0.33)
