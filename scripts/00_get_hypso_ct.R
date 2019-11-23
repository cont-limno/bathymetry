source("scripts/99_utils.R")

# ---- get-raw-data ----
if(!file.exists("data/ct_bathy/ct_bathy.gpkg")){
  library(esri2sf) # install_github("yonghah/esri2sf")
  base_url <- "https://cteco.uconn.edu/ctmaps/rest/services/Elevation/Lake_Bathymetry/MapServer/"

  # contours <- st_read("data/ct_bathy/ct_bathy.gpkg", layer = "contours")
  contours <- esri2sf(paste0(base_url, "0"))

  # loop through lake names to find malformed records
  ps <- list()
  for(i in seq_along(unique(contours$WBNAME))){
    where <- paste0("WBNAME = '", unique(contours$WBNAME)[i], "'")
    ps[[i]] <- esri2sf(paste0(base_url, "1"), where = where)
  }
  names(ps) <- unique(contours$WBNAME)
  ps <- ps[which(unlist(lapply(ps, nrow)) > 1)]
  ps <- dplyr::rbind_list(ps)
  st_crs(ps) <- st_crs(4326)

  sf::st_write(contours, "data/ct_bathy/ct_bathy.gpkg", "contours")
  sf::st_write(ps, "data/ct_bathy/ct_bathy.gpkg", "ps",
               update = TRUE, delete_layer = TRUE)
}
# contours <- st_read("data/ct_bathy/ct_bathy.gpkg", layer = "contours")
ps       <- st_read("data/ct_bathy/ct_bathy.gpkg", layer = "ps")

# map ps polygons to llids
## pull the largest polygon associated with each lake
ps_large <- ps %>%
  mutate(area = st_area(.)) %>%
  group_by(WBNAME) %>%
  slice(which.max(area)) %>%
  ungroup()
# fill-in hollow contours
ps_large_filled       <- lapply(seq_len(nrow(ps_large)), function(x)
      concaveman::concaveman(st_cast(ps_large[x,], "POINT"))
  )
ps_large_filled       <- do.call(rbind, ps_large_filled)
st_geometry(ps_large) <- ps_large_filled$polygons

lg_pnts <- query_gis_(query = paste0(
  "SELECT * FROM LAGOS_NE_All_Lakes_4ha_POINTS WHERE ",
  paste0("State_Name LIKE '", "Connecticut", "'", collapse = " OR ")))
lg_pnts <- sf::st_join(lg_pnts, st_transform(ps_large, st_crs(lg_pnts))) %>%
  dplyr::filter(!is.na(WBNAME))

ps_large <- ps_large[ps_large$WBNAME %in% lg_pnts$WBNAME,]
ps       <- ps[ps$WBNAME %in% ps_large$WBNAME,]
ps       <- left_join(ps,
                  dplyr::select(st_drop_geometry(lg_pnts), lagoslakeid, WBNAME),
                  by = "WBNAME")

# length(unique(ps$lagoslakeid))
# length(unique(ps$WBNAME)); nrow(ps_large)
# View(unique(paste0(ps$WBNAME, ps$lagoslakeid))[order(unique(paste0(ps$WBNAME, ps$lagoslakeid)))])
# lg <- lagosne_load()
# lake_info(name = "Black Pond", state = "Connecticut", dt = lg)

# polygons to raster
get_rsub <- function(dt){
  dt <- st_transform(dt, 6433) # ct state plane
  dt <- dplyr::select(dt, DEPTH_FT)

  r             <- raster(xmn = st_bbox(dt)[1], ymn = st_bbox(dt)[2],
                          xmx = st_bbox(dt)[3], ymx = st_bbox(dt)[4])
  r[]           <- NA
  r             <- rasterize(as_Spatial(dt), r, field = "DEPTH_FT")
  projection(r) <- as.character(st_crs(dt))[2]

  # https://stackoverflow.com/a/45658609/3362993
  fill.na <- function(x) {
    center = 0.5 + (width*width/2)
    if( is.na(x)[center] ) {
      return( round(mean(x, na.rm=TRUE),0) )
    } else {
      return( round(x[center],0) )
    }
  }
  width <- 17
  r2 <- focal(r, w = matrix(1, width, width), fun = fill.na,
              pad = TRUE, na.rm = FALSE)
  # clip to r2
  r2 <- mask(r2, dt)
  r2
}

# raster to hypso
get_hypso <- function(rsub, id){
  # rsub <- rsubs[[46]]

  maxdepth <- abs(cellStats(rsub, "max"))

  # calculate area of each class
  rc <- rsub %>%
    as.data.frame() %>%
    group_by(layer) %>% tally() %>%
    tidyr::drop_na(layer) %>%
    arrange(desc(layer)) %>%
    mutate(n_cs = cumsum(n)) %>%
    rename(depth = layer) %>%
    mutate(area_m2 = n_cs * res(rsub)[1] * res(rsub)[2]) %>%
    mutate(area_percent = scales::rescale(area_m2, to = c(0, 100))) %>%
    mutate(depth_percent = scales::rescale(depth, to = c(0, 100)))

  rc
}

rsubs <- lapply(unique(ps$lagoslakeid), function(x){
  message(x)
  dt <- dplyr::filter(ps, lagoslakeid == x)
  get_rsub(dt)
})
names(rsubs) <- unique(ps$lagoslakeid)

if(!interactive()){
  for(i in 1:length(rsubs)){
    x <- names(rsubs[i])
    fname <- paste0("data/ct_bathy/",
                    snakecase::to_snake_case(x))
    if(!file.exists(paste0(fname, ".tif"))){
      writeRaster(rsubs[[i]], fname, format='GTiff')
    }
  }
}

hypso <- lapply(seq_len(length(rsubs)), function(x){
  dplyr::mutate(get_hypso(rsubs[[x]]),
                llid = names(rsubs)[x])
})
hypso <- dplyr::bind_rows(hypso)

name_key <- distinct(st_drop_geometry(
  dplyr::mutate(
    dplyr::select(ps, lagoslakeid, WBNAME),
    lagoslakeid = as.character(lagoslakeid))))
hypso <- left_join(hypso, name_key,
                  by = c("llid" = "lagoslakeid"))

if(interactive()){
  ggplot(data = left_join(hypso,
                          dplyr::mutate(
                            dplyr::select(lg_pnts, lagoslakeid, Lake_Area_Ha),
                            lagoslakeid = as.character(lagoslakeid)),
                          by = c("llid" = "lagoslakeid"))) +
    geom_line(aes(x = area_percent, y = depth_percent, group = llid,
                  color = log(Lake_Area_Ha))) +
    ylim(100, 0)
  # TODO: plot the line of an ideal cone shape
}

hypso <- hypso %>%
select(-n) %>% group_by(llid, WBNAME) %>%
  add_tally() %>% ungroup() %>%
  dplyr::filter(n > 3) %>%
  filter(!str_detect(WBNAME, "Cove"))

hypso <- dplyr::select(hypso, llid, area_percent, depth_percent)

write.csv(hypso, "data/ct_hypso.csv", row.names = FALSE)
# hypso1 <- read.csv("data/ct_hypso.csv", stringsAsFactors = FALSE)
