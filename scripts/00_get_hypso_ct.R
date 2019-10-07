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
contours <- st_read("data/ct_bathy/ct_bathy.gpkg", layer = "contours")
ps       <- st_read("data/ct_bathy/ct_bathy.gpkg", layer = "ps")

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

rsubs <- lapply(unique(ps$WBNAME), function(x){
  message(x)
  dt <- dplyr::filter(ps, WBNAME == x)
  get_rsub(dt)
})
names(rsubs) <- unique(ps$WBNAME)

for (i in 1:length(rsubs)) {
  writeRaster(rsubs[[i]],
              paste0("data/ct_bathy/", snakecase::to_snake_case(names(rsubs[i]))),
              format='GTiff')
}

hypso <- lapply(seq_len(length(rsubs)), function(x){
  dplyr::mutate(get_hypso(rsubs[[x]]),
                id = names(rsubs)[x])
})
hypso <- dplyr::bind_rows(hypso)

if(interactive()){
  ggplot(data = hypso) +
    geom_line(aes(x = area_percent, y = depth_percent, group = id))
  # TODO: plot the line of an ideal cone shape
}

write.csv(hypso, "data/ct_hypso.csv", row.names = FALSE)
# hypso <- read.csv("data/ct_hypso.csv", stringsAsFactors = FALSE)
