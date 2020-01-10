# Make taxonomy table by referencing `limno_taxonomy` file**

# names(read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE))

taxonomy_key <- data.frame(
  col_name = c("lagoslakeid", "lake_namegnis", "lake_states", "lake_state",
               "lake_lat_decdeg", "lake_lon_decdeg", "lake_maxdepth_m",
               "lake_meandepth_m", "lake_waterarea_ha", "programtype_depth",
               "programlink_depth", "lagos_effort", "predicted_maxdepth_m"),
                           description = c(
                  "unique lake identifier developed for LAGOS-US",
                  "lake name from the gnis database",
                  "abbreviation(s) of state(s) intersecting the lake polygon",
                  "abbreviation of the state used to search for a lake's depth",
                  "the latitude of the lake center point (NAD83).",
                  "the longitude of the lake center point (NAD83).",
                  "lake maximum depth in meters",
                  "lake mean depth in meters",
                  "surface area of lake waterbody polygon from NHD (excludes islands)",
                  "type of program used as the source of depth data",
                  "link or name of the program used as the source of depth data",
                  "name of depth searching effort used for internal tracking",
                  "lake maximum depth in meters predicted from Stachelek et. al"),
  stringsAsFactors = FALSE)

write.csv(taxonomy_key, "data/lagosus_depth_taxonomy.csv", row.names = FALSE)

