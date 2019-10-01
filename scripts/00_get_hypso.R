source("scripts/99_utils.R")

r  <- raster("data/mn_bathy/lake_bathymetric_elevation_model.tif")

dt <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  dplyr::filter(effort == "LAGOSNE") %>%
  dplyr::filter(lake_waterarea_ha >= 4 & lake_waterarea_ha <= 400) %>%
  dplyr::filter(state == "MN") %>%
  dplyr::filter(mean_depth_m != max_depth_m) %>%
  dplyr::filter(mean_depth_m > 1) %>%
  dplyr::filter(max_depth_m >= 5)

llid_pnts <- query_gis("LAGOS_NE_All_Lakes_4ha_POINTS", "lagoslakeid", dt$llid)
llid_pnts <- st_transform(llid_pnts, st_crs(r))
# limit to those intersecting topobathy values
dt        <- dt[!is.na(raster::extract(r, llid_pnts)),]
dt        <- arrange(dt, desc(max_depth_m))
llid_poly <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", dt$llid) %>%
  st_transform(st_crs(r))
rsubs     <- lapply(st_geometry(llid_poly), function(x){
  raster::crop(r,
       raster::extent(
         st_buffer(st_sf(st_as_sfc(st_bbox(x))), 100))) / 3.281 # ft to m
})
names(rsubs) <- llid_poly$lagoslakeid

# ---- function_definitions ----
get_hypso <- function(rsub){
  # rsub <- rsubs[[46]]
  maxdepth <-  abs(cellStats(rsub, "min"))

  # define depth intervals by raster resolution
  # min_res   <- res(rsub)[1]
  min_res   <- 0.5
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

  rc
}

hypso <- lapply(seq_len(length(rsubs)),
               function(x){
                 # print(x)
                 dplyr::mutate(get_hypso(rsubs[[x]]),
                               llid = dt$llid[x])
                 })
hypso <- dplyr::bind_rows(hypso)

if(interactive()){
  library(ggplot2)
  ggplot(data = hypso) +
    geom_line(aes(x = area_percent, y = depth_percent, group = llid))
}

write.csv(hypso, "data/mn_hypso.csv", row.names = FALSE)

