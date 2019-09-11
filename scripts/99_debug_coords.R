library(dplyr)
library(googledrive)
library(sf)

# drive_find(type = "spreadsheet")
if(!file.exists("data/backups/test.csv")){
  drive_download("WA_SC_SWM_siteinfo_v3_linked_R", type = "csv",
                 path = "data/backups/test.csv", overwrite = TRUE)
}

dt <- read.csv("data/backups/test.csv", stringsAsFactors = FALSE) %>%
# "NAD_1983_StatePlane_Washington_North_FIPS_4601_Feet" == "+proj=lcc +lat_1=47.5 +lat_2=48.73333333333333 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000.0000000002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 no_defs"
st_as_sf(coords = c("X", "Y"), crs = "+proj=lcc +lat_1=47.5 +lat_2=48.73333333333333 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000.0000000002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 no_defs") %>%
  st_transform(crs = 4326)

st_coordinates(dt)

