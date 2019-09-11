library(nlaR)
library(dplyr)
library(tidyr)
library(LAGOSNE)
library(ggplot2)

nl2012 <- nla_load(2012)
dt     <- dplyr::select(nl2012$wide_siteinfo,
                    SITE_ID, LAT_DD83, LON_DD83) %>%
  distinct(SITE_ID, .keep_all = TRUE) %>%
  left_join(dplyr::select(nl2012$wide_profile,
                          SITE_ID, INDEX_SITE_DEPTH)) %>%
  tidyr::drop_na(INDEX_SITE_DEPTH) %>%
  group_by(SITE_ID) %>%
  slice(which.max(INDEX_SITE_DEPTH)) %>%
  LAGOSNE::coordinatize("LAT_DD83", "LON_DD83")

ggplot() +
  geom_sf(data = dt, aes(color = INDEX_SITE_DEPTH))

