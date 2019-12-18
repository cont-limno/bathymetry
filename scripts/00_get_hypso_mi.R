source("scripts/99_utils.R")

# MI data comes as contours with no lake-level labels
#   higher numbers represent deeper depths

if(!file.exists("data/mi_bathy/contours.geojson")){
# https://gisago.mcgi.state.mi.us/arcgis/rest/services/OpenData/hydro/MapServer/4
download.file("https://opendata.arcgis.com/datasets/d49160d2e5af4123b15d48c2e9c70160_4.geojson",
              "data/mi_bathy/contours.geojson")
}

lg_poly <- query_gis_(query = paste0("SELECT * FROM LAGOS_NE_All_Lakes_4ha WHERE ",
                                   paste0("State_Name LIKE '", "Michigan'", collapse = " OR ")))
mi      <- st_read("data/mi_bathy/contours.geojson")
mi      <- st_transform(mi, st_crs(lg_poly))
mi      <- st_join(mi, lg_poly) %>%
  dplyr::filter(!is.na(lagoslakeid))

lg_mi <- dplyr::filter(lg_poly, lagoslakeid %in% mi$lagoslakeid)

pb <- progress_bar$new(
  format = "llid :llid [:bar] :percent",
  total = length(unique(mi$lagoslakeid)),
  clear = FALSE, width = 80)

rsubs <- lapply(seq_along(unique(mi$lagoslakeid)),
                function(i){
  pb$tick(tokens = list(llid = unique(mi$lagoslakeid)[i]))

                  # i <- 1
  fname <- paste0("data/mi_bathy/", unique(mi$lagoslakeid)[i], ".tif")
  if(!file.exists(fname)){
    dt <- dplyr::filter(mi, lagoslakeid == unique(mi$lagoslakeid)[i])
    dt <- suppressWarnings(st_cast(dt, "POINT"))

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
fnames <- list.files("data/mi_bathy/", pattern = "tif",
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

write.csv(hypso, "data/mi_hypso.csv", row.names = FALSE)
# hypso_mi <- read.csv("data/mi_hypso.csv", stringsAsFactors = FALSE)
