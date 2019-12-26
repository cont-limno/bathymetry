source("scripts/99_utils.R")

# MN data comes as a topobathymetric raster
#   lower (more negative) numbers represent deeper depths

r  <- raster("data/mn_bathy/lake_bathymetric_elevation_model.tif")

lg_x_walk <- lagosus_load(modules = "locus")$locus$locus_link %>%
  dplyr::select(lagosne_lagoslakeid, lagoslakeid, lagosus_centroidstate) %>%
  dplyr::rename(lagosus_lagoslakeid = lagoslakeid) %>%
  distinct(lagosne_lagoslakeid, .keep_all = TRUE)

dt <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  dplyr::filter(lake_state == "MN") %>%
  dplyr::filter(!is.na(lake_maxdepth_m)) %>%
  dplyr::filter(lake_meandepth_m != lake_maxdepth_m | is.na(lake_meandepth_m)) %>%
  dplyr::filter(lake_meandepth_m > 1 | is.na(lake_meandepth_m)) %>%
  # dplyr::filter(lake_maxdepth_m < 20) %>%
  dplyr::filter(lake_maxdepth_m >= 2) %>%
  dplyr::filter(!duplicated(lagoslakeid)) %>%
  left_join(lg_x_walk, by = c("lagoslakeid" = "lagosus_lagoslakeid")) %>%
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
dt        <- arrange(dt, desc(lake_maxdepth_m))
llid_pnts <- dplyr::filter(llid_pnts, lagoslakeid %in% dt$lagosne_lagoslakeid)

llid_poly <- query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", dt$lagoslakeid) %>%
  st_transform(st_crs(r))
llid_poly <- llid_poly[as.numeric(st_area(llid_poly)) != 0,]

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = nrow(llid_poly),
  clear = FALSE, width = 80)

rsubs     <- lapply(seq_len(nrow(llid_poly)), function(x){
  # x <- 1
  # x <- which(140544 == llid_poly$lagoslakeid)
  pb$tick(tokens = list(llid = llid_poly[x,]$lagoslakeid))

  fname <- paste0("data/mn_bathy/", llid_poly[x,]$lagoslakeid, ".tif")
  if(!file.exists(fname)){
    res <- raster::crop(r,
       raster::extent(
         st_buffer(st_sf(st_as_sfc(st_bbox(
           st_geometry(llid_poly)[x]))), 100)
         )) / 3.281  # ft to m
    writeRaster(res, fname)
  }
})
flist        <- list.files("data/mn_bathy/", patter = "\\d.tif",
                    full.names = TRUE, include.dirs = TRUE)
flist <- flist[
  gsub(".tif", "", basename(flist)) %in% unique(llid_poly$lagoslakeid)]

rsubs        <- lapply(flist, raster)
rsubs <- rsubs[!is.na(sapply(rsubs, minValue))]
rsubs <- rsubs[sapply(rsubs, minValue) < -0.2]
names(rsubs) <- gsub("X", "", unlist(lapply(rsubs, names)))

# ---- function_definitions ----
get_hypso <- function(rsub){
  # rsub <- rsubs[[82]]
  maxdepth <-  abs(cellStats(rsub, "min")) # MN depths are negative

  # define depth intervals by raster resolution
  # min_res   <- res(rsub)[1]
  min_res   <- 0.5
  depth_int <- -1 * seq(0, round(maxdepth/min_res) * min_res, by = min_res)

  # calculate area of raster between depth intervals
  # reclassify raster based on depth intervals
  # calculate area of each class
  rc <- cut(rsub, breaks = depth_int) %>%
    as.data.frame()
  # drop depth_int entries for missing layers
  depth_int <- depth_int[1:length(depth_int) %in% unique(rc$layer)]

  rc <- rc %>%
    tidyr::drop_na(layer) %>%
    group_by(layer) %>% tally() %>% cumsum() %>%
    mutate(area_m2 = n * 5 * 5) %>%
    mutate(depth_int = c(rev(# add interval midpoints
      as.numeric(na.omit((depth_int + lag(depth_int))/2)) * -1), 0)) %>%
    mutate(area_percent = scales::rescale(area_m2, to = c(0, 100))) %>%
    mutate(depth_percent = scales::rescale(depth_int, to = c(0, 100)))

  rc
}

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = length(rsubs),
  clear = FALSE, width = 80)

hypso <- lapply(seq_len(length(rsubs)),
               function(x){
                 pb$tick(tokens = list(llid = names(rsubs)[x]))
                 # print(x)
                 # x <- which(names(rsubs) == 2005)
                 dplyr::mutate(get_hypso(rsubs[[x]]), llid = names(rsubs)[x])
                 })
hypso <- dplyr::bind_rows(hypso)

if(interactive()){
  ggplot(data = hypso) +
    geom_line(aes(x = area_percent, y = depth_percent, group = llid)) +
    geom_abline(slope = 1, intercept = -100, color = "red") +
    ylim(c(100, 0))
  # TODO: plot the line of an ideal cone shape
}

hypso <- hypso %>%
  group_by(llid) %>%
  mutate(maxdepth = max(depth_int)) %>%
  ungroup() %>%
  dplyr::select(llid, area_percent, depth_percent, maxdepth)

write.csv(hypso, "data/mn_hypso.csv", row.names = FALSE)
# hypso_mn <- read.csv("data/mn_hypso.csv", stringsAsFactors = FALSE)
