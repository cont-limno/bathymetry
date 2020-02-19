# Make taxonomy table by referencing `limno_taxonomy` file**

# names(read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE))

taxonomy_key <- data.frame(
  col_name = c("lagoslakeid", "lake_namegnis", "lake_states", "lake_state",
               "lake_lat_decdeg", "lake_lon_decdeg", "lake_maxdepth_m",
               "lake_meandepth_m", "lake_waterarea_ha",
               "sourcename_depth", "sourceurl_depth", "sourcetype_depth",
               "lagos_effort", "lagos_effort_reliablility"),
                           description = c(
                  "unique lake identifier developed for LAGOS-US",
                  "lake name from the gnis database by way of the LAGOS-US Locus module",
                  "abbreviation(s) of state(s) intersecting the lake polygon from the LAGOS-US Locus module",
                  "abbreviation of the state used to search for a lake's depth",
                  "the latitude of the lake center point (NAD83) from the LAGOS-US Locus module",
                  "the longitude of the lake center point (NAD83) from the LAGOS-US Locus module",
                  "lake maximum depth in meters",
                  "lake mean depth in meters",
                  "surface area of lake waterbody polygon from NHD (excludes islands) by way of the LAGOS-US Locus module",
                  "name of the source of depth data",
                  "url link of the source of depth data",
                  "type of the source of depth data; one of Citizen Monitoring, Government, University, Commercial",
                  "name of depth searching effort used for internal tracking; one of LAGOSNE, LAGOSUS, bathymetry, NLA",
                  "reliability of depth searching effort used for quality assurance and de-duplication; values in ascending order with 1 being the most reliable"),
  stringsAsFactors = FALSE)

write.csv(taxonomy_key, "data/lagosus_depth_taxonomy.csv", row.names = FALSE)
