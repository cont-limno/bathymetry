source("scripts/99_utils.R")

# MI data comes as contours with no lake-level labels
#   higher numbers represent deeper depths

if(!file.exists("data/mi_bathy/contours.geojson")){
# https://gisago.mcgi.state.mi.us/arcgis/rest/services/OpenData/hydro/MapServer/4
download.file("https://opendata.arcgis.com/datasets/d49160d2e5af4123b15d48c2e9c70160_4.geojson",
              "data/mi_bathy/contours.geojson")
}

lg_poly <- LAGOSUSgis::query_gis_(query =
                                    "SELECT * FROM LAGOS_US_All_Lakes_1ha WHERE lake_centroidstate LIKE 'MI' AND lake_totalarea_ha > 4")
lg_poly <- lwgeom::st_make_valid(lg_poly)

# remove problematic llids
bad_llids <- c(107971, 4101, 1141, 2723)
lg_poly   <- dplyr::filter(lg_poly, !(lagoslakeid %in% bad_llids))

mi      <- st_read("data/mi_bathy/contours.geojson")
mi      <- st_transform(mi, st_crs(lg_poly))
mi      <- st_join(mi, lg_poly) %>%
  dplyr::filter(!is.na(lagoslakeid))

# ## remove st_join errors
# # find mi that st_cross lg_poly
# # buffer these lines
# # compute overlap
# # remove those that have overlap less than some threshold
# bad_contours <- unlist(lapply(
#   sf::st_crosses(mi, lg_poly), function(x) length(x) > 0))
# bad_contours <- mi[bad_contours,]
# bad_contours <- st_buffer(bad_contours, 30)
# bad_contours <- st_intersection(bad_contours, lg_poly)
# test <- st_area(bad_contours) < units::as_units(1800, "m2")
# test2 <- bad_contours[test,]
# # mapview(bad_contours[which(test)[1],])

# remove lakes not in lagosne xwalk table as they likely have a
# non-functional basin split issue
lg_xwalk <- read.csv("data/00_lagosne/00_lagosne_xwalk.csv",
                     stringsAsFactors = FALSE)
mi <- dplyr::filter(mi, lagoslakeid %in% unique(lg_xwalk$lagoslakeid))

lg_mi <- dplyr::filter(lg_poly, lagoslakeid %in% mi$lagoslakeid)

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = length(unique(mi$lagoslakeid)),
  clear = FALSE, width = 80)

rsubs <- lapply(seq_along(unique(mi$lagoslakeid)),
                function(i){
  pb$tick(tokens = list(llid = unique(mi$lagoslakeid)[i]))

  # i <- 1
  # i <- which(unique(mi$lagoslakeid) == 107971)
  fname <- paste0("data/mi_bathy/", unique(mi$lagoslakeid)[i], ".tif")
  if(!file.exists(fname)){
    dt <- dplyr::filter(mi, lagoslakeid == unique(mi$lagoslakeid)[i])
    dt <- suppressWarnings(st_cast(dt, "POINT"))
    # ggplot() + geom_sf(data = dt, aes(color = DEPTH)) +
    #   geom_sf(
    #     data = dplyr::filter(lg_poly, lagoslakeid == unique(mi$lagoslakeid)[i]))

    res <- poly_to_filled_raster(dt, "DEPTH", 27, proj = 32616)
    if(cellStats(res$r, max) != 0){
      writeRaster(res$r, fname)
    }
  }else{
    res    <- list()
    res$r  <- raster(fname)
    res$wh <- NA
  }

  list(r = res$r, width = res$wh)
  })
# whs          <- unlist(lapply(rsubs, function(x) x$width))
flist        <- list.files("data/mi_bathy/", patter = "\\d.tif",
                           full.names = TRUE, include.dirs = TRUE)
## rm flist not in list
# flist_rm <- flist[!
#   gsub(".tif", "", basename(flist)) %in% unique(lg_mi$lagoslakeid)]
# sapply(flist_rm, unlink)
flist <- flist[
  gsub(".tif", "", basename(flist)) %in% unique(lg_poly$lagoslakeid)]
rsubs        <- lapply(flist, raster)
names(rsubs) <- gsub(".tif", "", basename(flist))

## only select files that match unique(mi$lagoslakeid)?
# any(!(names(rsubs) %in% unique(mi$lagoslakeid)))

rsubs <- rsubs[sapply(rsubs, maxValue) > 0.2]

# remove rsubs with no data
# rsubs_good <- unlist(lapply(rsubs, function(x) cellStats(x, max) != 0))
# names(rsubs)[which(!rsubs_good)]
# rsubs <- rsubs[rsubs_good]

# get hypsography csv
get_hypso <- function(rsub){
  # rsub <- rsubs[[1]]
  # rsub <- rsubs[[which(names(rsubs) == 89659)]]
  maxdepth <-  abs(cellStats(rsub, "max"))

  # define depth intervals by raster resolution
  # min_res   <- res(rsub)[1]
  min_res   <- 0.5
  # 0 to max
  depth_int <- seq(0, round(maxdepth/min_res) * min_res, by = min_res)

  # calculate area of raster between depth intervals
  # reclassify raster based on depth intervals
  # calculate area of each class
  rc <- raster::cut(rsub, breaks = depth_int) %>%
    as.data.frame()
  # drop depth_int entries for missing layers
  depth_int <- depth_int[1:length(depth_int) %in% unique(rc$layer)]
  depth_mid <- # add interval midpoints
    c(0, as.numeric(na.omit((depth_int + lag(depth_int))/2)))
  # cbind(depth_int, depth_mid)
  # length(depth_int) == length(unique(tidyr::drop_na(rc, layer)$layer))

  rc <- rc %>% tidyr::drop_na(layer) %>%
    group_by(layer) %>% tally() %>% arrange(desc(layer)) %>%
    cumsum() %>%
    mutate(area_m2 = n * res(rsub)[1] * res(rsub)[2]) %>%
    mutate(depth_int = rev(depth_mid)) %>%
    mutate(area_percent = scales::rescale(area_m2, to = c(0, 100))) %>%
    mutate(depth_percent = scales::rescale(depth_int, to = c(0, 100)))

  # plot(rc$area_percent, rc$depth_percent, ylim = c(100, 0))
  rc
}

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = length(rsubs),
  clear = FALSE, width = 80)

hypso <- lapply(seq_len(length(rsubs)), function(x){
  pb$tick(tokens = list(llid = names(rsubs)[x]))
  # which(lg_mi$lagoslakeid == 3791)
  # which(names(rsubs) == 1293)
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
  mutate(maxdepth = max(depth_int) / 3.281) %>% # ft to meters
  ungroup() %>%
  dplyr::select(llid, area_percent, depth_percent, maxdepth)

write.csv(hypso, "data/mi_hypso.csv", row.names = FALSE)
# hypso_mi <- read.csv("data/mi_hypso.csv", stringsAsFactors = FALSE)
