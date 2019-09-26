library(raster)
library(LAGOSNE)
library(dplyr)
library(LAGOSNEgis)
library(sf)

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

r         <- raster("data/mn_bathy/lake_bathymetric_elevation_model.tif")
llid_pnts <- query_gis("LAGOS_NE_All_Lakes_4ha_POINTS", "lagoslakeid", dt$lagoslakeid)
llid_pnts <- st_transform(llid_pnts, st_crs(r))
dt        <- dt[!is.na(raster::extract(r, llid_pnts)),]
dt        <- arrange(dt, desc(maxdepth))

llid <- dt$lagoslakeid[1]

res_sf   <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", llid) %>%
  st_transform(st_crs(r))
box      <- raster::extent(st_buffer(st_sf(st_as_sfc(st_bbox(res_sf))), 100))
rsub     <- crop(r, box) / 3.281
maxdepth <-  abs(cellStats(rsub, "min"))

# define depth intervals by raster resolution
min_res   <- res(rsub)[1]
depth_int <- -1 * seq(0, round(maxdepth/min_res) * min_res, by = min_res)

# calculate area of raster between depth intervals
  # reclassify raster based on depth intervals
  # calculate area of each class
rc <- cut(rsub, breaks = depth_int) %>%
  as.data.frame() %>% tidyr::drop_na(layer) %>%
  group_by(layer) %>% tally() %>% cumsum() %>%
  mutate(area_m2 = n * 5 *5) %>%
  mutate(depth_int = rev(# add interval midpoints
    as.numeric(na.omit((depth_int + lag(depth_int))/2)) * -1)) %>%
  mutate(area_percent = (area_m2 / max(area_m2)) * 100) %>%
  mutate(depth_percent = (depth_int/ max(depth_int)) * 100)


plot(rc$area_percent, rc$depth_percent)

rc$area_m2[nrow(rc)]
st_area(res_sf)

30000 - 90425

