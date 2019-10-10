source("scripts/99_utils.R")

if(!file.exists("data/mi_bathy/contours.geojson")){
# https://gisago.mcgi.state.mi.us/arcgis/rest/services/OpenData/hydro/MapServer/4
download.file("https://opendata.arcgis.com/datasets/d49160d2e5af4123b15d48c2e9c70160_4.geojson",
              "data/mi_bathy/contours.geojson")
}

lg_poly <- query_gis_(query = paste0("SELECT * FROM LAGOS_NE_All_Lakes_4ha WHERE ",
                                   paste0("State_Name LIKE '", "Michigan'", collapse = " OR ")))
ct      <- st_read("data/mi_bathy/contours.geojson")
ct      <- st_transform(ct, st_crs(lg_poly))

ct_large <- st_join(ct, lg_poly) %>%
  dplyr::filter(!is.na(lagoslakeid)) %>%
  mutate(area = st_area(.)) %>%
  group_by(lagoslakeid) %>%
  slice(which.max(area)) %>%
  ungroup()




