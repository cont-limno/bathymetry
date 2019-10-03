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

# test_name <- "Colebrook River Reservoir"
# dt <- dplyr::filter(contours, WBNAME == test_name)
dt <- dplyr::filter(ps, WBNAME == "Colebrook River Reservoir")
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


