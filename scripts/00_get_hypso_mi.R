source("scripts/99_utils.R")

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

# rsubs <- lapply(unique(mi$lagoslakeid), function(x){
  message(x)
  x <- unique(mi$lagoslakeid)[1]
  dt <- dplyr::filter(mi, lagoslakeid == x)
  test <- poly_to_filled_raster(dt, proj = 3079, depth_attr = "DEPTH")
# })
# names(rsubs) <- unique(mi$lagoslakeid)


# mi %>%
# group_by(lagoslakeid) %>%
# tally()

mi_large <- mi %>%
  mutate(area = st_area(.)) %>%
  group_by(lagoslakeid) %>%
  slice(which.max(area)) %>%
  ungroup()

mi_large_filled       <- lapply(seq_len(nrow(mi_large)), function(x)
  concaveman::concaveman(st_cast(mi_large[x,], "POINT"))
)

mi_large_filled       <- do.call(rbind, mi_large_filled)

st_geometry(mi_large) <- mi_large_filled$polygons
mi_large              <- mutate(mi_large,
                                legacy_id = 1:nrow(mi_large))
mi_large <- mi_large[st_is_valid(mi_large),]

# for each mi_large find intersecting mi

# TODO: maybe polylines need to be joined in some fashion?
# try mapviewing in a buffer
i <- 1
buf <- st_as_sfc(st_bbox(
  st_buffer(mi_large[i,], 100)
  ))
test <- jsta::get_intersects(mi, buf)

# assign corresponding llid
test <- lapply(seq_len(2), function(i){
  # i <- 1
  jsta::get_intersects(mi, mi_large[i,])
})
lapply(test, nrow)
