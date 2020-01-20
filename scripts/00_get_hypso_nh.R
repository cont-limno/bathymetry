source("scripts/99_utils.R")

# original data from:
# http://www.granit.unh.edu/cgi-bin/nhsearch?dset=bathymetry_lakes_polygons/nh

# NH data comes as either contour lines or poylgons (has lake-level labels)
#   higher numbers represent deeper depths

nh_raw <- st_read("data/nh_bathy/Bathymetry_Lakes_lines.shp")

lg_poly <- LAGOSUSgis::query_gis_(query =
                                    "SELECT * FROM LAGOS_US_All_Lakes_1ha WHERE lake_centroidstate LIKE 'NH' AND lake_totalarea_ha > 4")
nh      <- st_transform(nh_raw, st_crs(lg_poly))
nh      <- st_join(nh, lg_poly) %>%
  dplyr::filter(!is.na(lagoslakeid))

# remove lakes not in lagosne xwalk table as they likely have a
# non-functional basin split issue
lg_xwalk <- read.csv("data/00_lagosne/00_lagosne_xwalk.csv",
                     stringsAsFactors = FALSE)
nh <- dplyr::filter(nh, lagoslakeid %in% unique(lg_xwalk$lagoslakeid))
# remove other problematic lakes
nh <- dplyr::filter(nh, !(lagoslakeid %in% c(5636))) # too complex for cnvx hull

lg_nh <- dplyr::filter(lg_poly, lagoslakeid %in% nh$lagoslakeid)

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = length(unique(nh$lagoslakeid)),
  clear = FALSE, width = 80)

rsubs <- lapply(seq_along(unique(nh$lagoslakeid)),
                function(i){
                  pb$tick(tokens = list(llid = unique(nh$lagoslakeid)[i]))

                  # i <- 1
                  # i <- which(unique(nh$lagoslakeid) == 5636)
                  fname <- paste0("data/nh_bathy/", unique(nh$lagoslakeid)[i], ".tif")
                  if(!file.exists(fname)){
                    dt_lines <- dplyr::filter(nh, lagoslakeid == unique(nh$lagoslakeid)[i])
                    dt <- suppressWarnings(st_cast(dt_lines, "POINT"))
                    # TODO: check if NA depths are present in the polygon product
                    dt <- dplyr::filter(dt, !is.na(DEPTH))

                    res <- poly_to_filled_raster(dt, "DEPTH", 27, proj = 32619)

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

flist        <- list.files("data/nh_bathy/", patter = "\\d.tif",
                           full.names = TRUE, include.dirs = TRUE)
flist <- flist[
  gsub(".tif", "", basename(flist)) %in% unique(lg_nh$lagoslakeid)]
rsubs        <- lapply(flist, raster)
names(rsubs) <- gsub(".tif", "", basename(flist))

rsubs <- rsubs[sapply(rsubs, maxValue) > 0.2]

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

write.csv(hypso, "data/nh_hypso.csv", row.names = FALSE)
# hypso_nh <- read.csv("data/nh_hypso.csv", stringsAsFactors = FALSE)
