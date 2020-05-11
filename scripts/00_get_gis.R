# setwd("../")
source("scripts/99_utils.R")

lg <- LAGOSUS::lagosus_load("locus")

# ---- size_comparison ----
gpkg_path  <- "data/gis.gpkg"
dt         <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  dplyr::filter(!is.na(lake_maxdepth_m)) %>%
  st_as_sf(coords = c("lake_lon_decdeg", "lake_lat_decdeg"), crs = 4326)

states_all <- ne_states(country = c("united states of america", "canada", "mexico"),
                        returnclass = "sf")
state_codes <- unique(dt$lake_state)
states_focal <- states_all %>%
  dplyr::filter(., postal %in% state_codes & iso_a2 == "US")
study_bbox   <- st_as_sfc(st_bbox(states_focal))
states_all   <- states_all %>%
  st_crop(study_bbox)
states_focal <- dplyr::filter(states_all, iso_a2 == "US") %>%
  dplyr::filter(postal %in%
                  pull(distinct(
                    dplyr::filter(st_drop_geometry(dt),
                                  lagos_effort == "bathymetry"),
                    lake_state), lake_state))

# add zoneids

# use LAGOSUS to pull hu ids that correspond to states
# lg <- lagosne_load()
# pad states with NA to 10 characters
hu4_zones <- distinct(lg$locus$locus_information,
                      lake_centroidstate, hu4_zoneid, lagoslakeid) %>%
  distinct(hu4_zoneid) %>% arrange(hu4_zoneid)
hu4_focal <- left_join(st_drop_geometry(dt),
                       dplyr::select(lg$locus$locus_information,
                                     lagoslakeid, hu4_zoneid)) %>%
  distinct(hu4_zoneid)

# hu4s       <- LAGOSUSgis::query_gis("hu4", "hu4_zoneid", hu4_zones$hu4_zoneid)
hu4s_focal <- LAGOSUSgis::query_gis("hu4", "hu4_zoneid", hu4_focal$hu4_zoneid)

study_bbox   <- st_as_sfc(st_bbox(states_all))

hu4s_focal_simple <- rmapshaper::ms_simplify(
  st_crop(sf::st_make_valid(hu4s_focal),
          st_transform(study_bbox, st_crs(hu4s_focal)))
)

states_all   <- rmapshaper::ms_simplify(states_all)

# study_bbox   <- concaveman::concaveman(st_cast(hu4s_focal, "POINT"))

# unlink("data/gis.gpkg")
# st_layers("data/gis.gpkg")
st_write(states_focal, gpkg_path, layer = "states_focal", delete_layer = TRUE,
         layer_options = c("OVERWRITE=yes"))
st_write(states_all, gpkg_path, layer = "states_all", delete_layer = TRUE,
         layer_options = c("OVERWRITE=yes"))
# st_write(hu4s, gpkg_path, layer = "hu4s", update = TRUE,
#          layer_options = c("OVERWRITE=yes"))
st_write(hu4s_focal, gpkg_path, layer = "hu4s_focal", update = TRUE,
         delete_layer = TRUE,
         layer_options = c("OVERWRITE=yes"))
st_write(hu4s_focal_simple, gpkg_path, layer = "hu4s_focal_simple", update = TRUE,
         delete_layer = TRUE,
         layer_options = c("OVERWRITE=yes"))
# st_write(hu8s, gpkg_path, layer = "hu8s", update = TRUE,
#          layer_options = c("OVERWRITE=yes"))
# st_write(counties, gpkg_path, layer = "counties", update = TRUE,
#          layer_options = c("OVERWRITE=yes"))
# st_write(iws, gpkg_path, layer = "iws", update = TRUE,
#          layer_options = c("OVERWRITE=yes"))
st_write(study_bbox, gpkg_path, layer = "study_bbox", update = TRUE,
         delete_layer = TRUE,
         layer_options = c("OVERWRITE=yes"))
st_write(dt, gpkg_path, layer = "dt", update = TRUE, delete_layer = TRUE,
         layer_options = c("OVERWRITE=yes"))
# test <- st_read("data/gis.gpkg", "dt", quiet = TRUE)

# library(RSQLite)
# db = SQLite()
# con = dbConnect(db, "data/gis.gpkg")
# dbRemoveTable(con, "dt")
