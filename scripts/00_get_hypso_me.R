# setwd("../")
source("scripts/99_utils.R")

# https://www.maine.gov/megis/catalog/shps/state/lakedpths.zip

# ME data comes as point depth soundings
# depth measurements don't go to zero
# no lake-level labels
# higher numbers represent deeper depths
# units in m

dir.create("data/me_bathy", showWarnings = FALSE)

if(!file.exists("data/me_bathy/lakedpths.zip")){
  base_url <- "https://www.maine.gov/megis/catalog/shps/state/lakedpths.zip"
  download.file(base_url, "data/me_bathy/lakedpths.zip")
  unzip("data/me_bathy/lakedpths.zip", exdir = "data/me_bathy/")
}

# pull lagosus points
lg_poly <- LAGOSUSgis::query_gis_(query =
                                    "SELECT * FROM LAGOS_US_All_Lakes_1ha WHERE lake_centroidstate LIKE 'ME' AND lake_totalarea_ha > 4")
pnts <- sf::st_read("data/me_bathy/lakedpth.shp")
pnts <- st_transform(pnts, st_crs(lg_poly))
pnts <- st_join(pnts, lg_poly) %>%
  dplyr::filter(!is.na(lagoslakeid))

# remove lakes not in lagosne xwalk table as they likely have a
# non-functional basin split issue
lg_xwalk <- read.csv("data/00_lagosne/00_lagosne_xwalk.csv",
                     stringsAsFactors = FALSE)
pnts <- dplyr::filter(pnts,
                           lagoslakeid %in% unique(lg_xwalk$lagoslakeid))
few_soundings <- pnts %>%
  group_by(lagoslakeid) %>%
  add_tally() %>%
  dplyr::filter(n < 3) %>%
  pull(lagoslakeid)
pnts <- dplyr::filter(pnts, !(lagoslakeid %in% few_soundings))

lg_ps <- dplyr::filter(lg_poly, lagoslakeid %in% pnts$lagoslakeid)

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = length(unique(pnts$lagoslakeid)),
  clear = FALSE, width = 80)

rsubs <- lapply(seq_along(unique(pnts$lagoslakeid)),
                function(i){
                  pb$tick(tokens = list(llid = unique(pnts$lagoslakeid)[i]))

                  # i <- which(unique(pnts$lagoslakeid) == 8511)
                  fname <- paste0("data/me_bathy/", unique(pnts$lagoslakeid)[i], ".tif")
                  if(!file.exists(fname)){
                    dt <- dplyr::filter(pnts, lagoslakeid == unique(pnts$lagoslakeid)[i])
                    dt <- dplyr::filter(dt, !is.na(DEPTHM))
                    # depth measurements don't go to zero
                    dt_outline <- dplyr::filter(lg_ps,
                                          lagoslakeid == unique(pnts$lagoslakeid)[i])
                    dt_outline <- st_cast(dt_outline, "MULTIPOINT")
                    dt_outline <- st_cast(dt_outline, "POINT")
                    in_lake    <- unlist(lapply(
                      st_intersects(dt_outline, concaveman::concaveman(dt)),
                      function(x) length(x) > 0))
                    dt_outline <- dt_outline[!in_lake,]
                    dt_outline <- mutate(dt_outline, DEPTHM = 0)
                    dt         <- dt[,names(dt_outline)]
                    dt         <- rbind(dt, dt_outline)

                    res <- poly_to_filled_raster(dt, "DEPTHM", 15, proj = 32619)

                    poly_mask <- st_transform(
                      dplyr::filter(lg_ps,
                                    lagoslakeid == unique(pnts$lagoslakeid)[i]),
                      st_crs(res$r))
                    poly_mask <- st_cast(poly_mask, "POLYGON")
                    res$r     <- raster::mask(res$r, poly_mask)

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
# flist        <- list.files("data/me_bathy/", pattern = "\\d.tif",
#                            full.names = TRUE, include.dirs = TRUE)
## rm flist not in list
# (flist_rm <- flist[!
#   gsub(".tif", "", basename(flist)) %in% unique(pnts$lagoslakeid)])
# sapply(flist_rm, unlink)
fnames <- list.files("data/me_bathy/", pattern = "tif",
                     include.dirs = TRUE, full.names = TRUE)
rsubs <- lapply(fnames,
                function(x) raster(x))
names(rsubs) <- gsub(".tif", "", basename(fnames))

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
  # which(names(rsubs) == 3791)
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

write.csv(hypso, "data/me_hypso.csv", row.names = FALSE)
# hypso_me <- read.csv("data/me_hypso.csv", stringsAsFactors = FALSE)
