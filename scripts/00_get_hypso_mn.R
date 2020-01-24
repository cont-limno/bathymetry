source("scripts/99_utils.R")

# MN data comes as a topobathymetric raster
#   lower (more negative) numbers represent deeper depths
# https://gisdata.mn.gov/dataset/water-lake-bathymetry

r  <- raster("data/mn_bathy/lake_bathymetric_elevation_model.tif")

lg_pnts <- LAGOSUSgis::query_gis_(query =
            "SELECT * FROM LAGOS_US_All_Lakes_1ha_points WHERE lake_centroidstate LIKE 'MN' AND lake_totalarea_ha > 15")
lg_pnts <- st_transform(lg_pnts, st_crs(r))

dt <- st_drop_geometry(lg_pnts)
# limit to those intersecting topobathy values
dt <- dt[!is.na(raster::extract(r, lg_pnts)),]
llid_poly <- LAGOSUSgis::query_gis("LAGOS_US_All_Lakes_1ha",
                                   "lagoslakeid",
                                   dt$lagoslakeid) %>%
  st_transform(st_crs(r))
llid_poly <- llid_poly[as.numeric(st_area(llid_poly)) != 0,]

# remove lakes not in lagosne xwalk table as they likely have a
# non-functional basin split issue
lg_xwalk <- read.csv("data/00_lagosne/00_lagosne_xwalk.csv",
                     stringsAsFactors = FALSE)
llid_poly <- dplyr::filter(llid_poly,
                           lagoslakeid %in% unique(lg_xwalk$lagoslakeid))

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = nrow(llid_poly),
  clear = FALSE, width = 80)

rsubs     <- lapply(seq_len(nrow(llid_poly)), function(x){
  # x <- 1
  # x <- which(25419 == llid_poly$lagoslakeid)
  pb$tick(tokens = list(llid = llid_poly[x,]$lagoslakeid))

  fname <- paste0("data/mn_bathy/", llid_poly[x,]$lagoslakeid, ".tif")
  if(!file.exists(fname)){
    res <- raster::crop(r,
       raster::extent(
         st_buffer(st_sf(st_as_sfc(st_bbox(
           st_geometry(llid_poly)[x]))), 100)
         )) / 3.281  # ft to m

    res <- mask(res, llid_poly[x,])
    writeRaster(res, fname)
  }
})
flist        <- list.files("data/mn_bathy/", pattern = "\\d.tif",
                    full.names = TRUE, include.dirs = TRUE)
## rm flist not in list
# flist_rm <- flist[!
#   gsub(".tif", "", basename(flist)) %in% unique(llid_poly$lagoslakeid)]
# sapply(flist_rm, unlink)
flist <- flist[
  gsub(".tif", "", basename(flist)) %in% unique(llid_poly$lagoslakeid)]

rsubs       <- lapply(flist, raster)
bad_rasters <- sapply(rsubs, minValue)
bad_rasters <- bad_rasters > -0.2 | is.na(bad_rasters)
# delete bad_rasters files that were excluded above
# sapply(flist[bad_rasters], unlink)
rsubs <- rsubs[!bad_rasters]

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
