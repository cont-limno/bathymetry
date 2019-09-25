library(LAGOSNE)
library(dplyr)

lg <- lagosne_load("1.087.3")
dt <- dplyr::filter(lg$lakes_limno,
                    !is.na(maxdepth) & !is.na(meandepth)) %>%
  dplyr::select(lagoslakeid, nhd_lat, nhd_long, maxdepth, meandepth) %>%
  dplyr::left_join(dplyr::select(lg$locus, lagoslakeid, lake_area_ha, gnis_name))

# using lake areas:
# calculate a vector of lake radii
dt <- mutate(dt, radius = sqrt(lake_area_ha / pi))

# calculate "slopes" as max_depth / r
dt <- mutate(dt, slope = maxdepth / radius)

# calculate "half-slopes" as mean_depth / (r/2)
# how do they compare with full "slopes"?
dt <- mutate(dt, half_slope = meandepth / (radius / 2))

plot(dt$slope, dt$half_slope)
abline(0, 1)

# regress half-slopes against max depth
dt <- mutate(dt, maxdepth_pred = half_slope * radius)
plot(dt$maxdepth_pred, dt$maxdepth)
abline(0, 1)



