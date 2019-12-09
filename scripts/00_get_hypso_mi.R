source("scripts/99_utils.R")

# MI data come in

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

  fname <- paste0("data/mi_bathy/", unique(mi$lagoslakeid)[i], ".tif")
  if(!file.exists(fname)){
    dt <- dplyr::filter(mi, lagoslakeid == unique(mi$lagoslakeid)[i])
    dt <- suppressWarnings(st_cast(dt, "POINT"))

    res <- poly_to_filled_raster(dt, "DEPTH", 27, proj = 32616)
    writeRaster(res$r, fname)
  }else{
    res    <- list()
    res    <- raster(fname)
    res$wh <- NA
  }

  list(r = res$r, width = res$wh)
  })
whs          <- unlist(lapply(rsubs, function(x) x$width))
rsubs        <- lapply(rsubs, function(x) x$r)
names(rsubs) <- unique(mi$lagoslakeid)[1:10]


# TODO get hypsography csv
get_hypso <- function(rsub){
  # rsub <- rsubs[[1]]
  maxdepth <-  abs(cellStats(rsub, "max"))

  # define depth intervals by raster resolution
  # min_res   <- res(rsub)[1]
  min_res   <- 0.5
  depth_int <- seq(0, round(maxdepth/min_res) * min_res, by = min_res)

  # calculate area of raster between depth intervals
  # reclassify raster based on depth intervals
  # calculate area of each class
  rc <- cut(rsub, breaks = depth_int) %>%
    as.data.frame() %>% tidyr::drop_na(layer) %>%
    group_by(layer) %>% tally() %>% cumsum() %>%
    mutate(area_m2 = n * 5 * 5) %>%

    mutate(depth_int = rev(# add interval midpoints
      as.numeric(na.omit((depth_int + lag(depth_int))/2))
      )) %>%
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
