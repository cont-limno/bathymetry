source("scripts/99_utils.R")

r  <- raster("data/mn_bathy/lake_bathymetric_elevation_model.tif")

lg_x_walk <- read.csv(
  "data/00_lagosus_locus/LAGOS_Lake_Link_v2_20191017.csv",
  stringsAsFactors = FALSE) %>%
  dplyr::select(lagosne_lagoslakeid, lagoslakeid, lagosus_centroidstate) %>%
  dplyr::rename(lagosus_lagoslakeid = lagoslakeid) %>%
  distinct(lagosne_lagoslakeid, .keep_all = TRUE)

dt <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  # dplyr::filter(effort == "LAGOSNE") %>%
  # dplyr::filter(lake_waterarea_ha >= 4 & lake_waterarea_ha <= 400) %>%
  dplyr::filter(state == "MN") %>%
  dplyr::filter(!is.na(max_depth_m)) %>%
  dplyr::filter(mean_depth_m != max_depth_m | is.na(mean_depth_m)) %>%
  dplyr::filter(mean_depth_m > 1 | is.na(mean_depth_m)) %>%
  # dplyr::filter(max_depth_m < 20) %>%
  dplyr::filter(max_depth_m >= 2) %>%
  dplyr::filter(!duplicated(llid)) %>%
  left_join(lg_x_walk, by = c("llid" = "lagosus_lagoslakeid")) %>%
  dplyr::filter(!duplicated(lagosne_lagoslakeid))

# send lagosne ids to query_gis instead of lagosus ids
llid_pnts <- query_gis("LAGOS_NE_All_Lakes_4ha_POINTS", "lagoslakeid",
                       dt$lagosne_lagoslakeid)
llid_pnts <- st_transform(llid_pnts, st_crs(r))
llid_pnts <- llid_pnts[!sf::st_is_empty(llid_pnts),]

dt <- dplyr::filter(dt,
                    lagosne_lagoslakeid %in% unique(llid_pnts$lagoslakeid))
# limit to those intersecting topobathy values
dt        <- dt[!is.na(raster::extract(r, llid_pnts)),]
dt        <- arrange(dt, desc(max_depth_m))
llid_pnts <- dplyr::filter(llid_pnts, lagoslakeid %in% dt$lagosne_lagoslakeid)

llid_poly <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", dt$llid) %>%
  st_transform(st_crs(r))

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = nrow(llid_poly),
  clear = FALSE, width = 80)

rsubs     <- lapply(seq_len(nrow(llid_poly)), function(x){
  # x <- 1
  # x <- which(2109 == llid_poly$lagoslakeid)
  pb$tick(tokens = list(llid = llid_poly[x,]$lagoslakeid))

  fname <- paste0("data/mn_bathy/", llid_poly[x,]$lagoslakeid, ".tif")
  # TODO if file !exists write to raster
  if(!file.exists(fname)){
    res <- raster::crop(r,
       raster::extent(
         st_buffer(st_sf(st_as_sfc(st_bbox(
           st_geometry(llid_poly)[x]))), 100)
         )) / 3.281  # ft to m
    writeRaster(res, fname)
  }
})
flist <- list.files("data/mn_bathy/", patter = "\\d.tif",
                    full.names = TRUE, include.dirs = TRUE)
rsubs <- lapply(flist, raster)
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
    mutate(area_m2 = n * 5 * 5) %>%
    mutate(depth_int = rev(# add interval midpoints
      as.numeric(na.omit((depth_int + lag(depth_int))/2)) * -1)) %>%
    mutate(area_percent = scales::rescale(area_m2, to = c(0, 100))) %>%
    mutate(depth_percent = scales::rescale(depth_int, to = c(0, 100)))

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
  ggplot(data = hypso_mn) +
    geom_line(aes(x = area_percent, y = depth_percent, group = llid))
  # TODO: plot the line of an ideal cone shape
}

hypso <- dplyr::select(hypso, llid, area_percent, depth_percent)

write.csv(hypso, "data/mn_hypso.csv", row.names = FALSE)
# hypso_mn <- read.csv("data/mn_hypso.csv", stringsAsFactors = FALSE)
