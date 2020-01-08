
library(glwdr) # devtools::install_github("jsta/glwdr")
library(dplyr)
library(sf)

dt_raw <- glwd_load(level = 1)
names(dt_raw) <- tolower(names(dt_raw))
dt_raw <- st_as_sf(dt_raw, crs = 4326)
st_crs(dt_raw) <- 4326

us <- jsta::usa_sf()
dt <- dplyr::filter(dt_raw, country == "United States")
dt <- jsta::get_intersects(dt, st_transform(us, st_crs(dt)))

plot(dt$geometry)
range(dt$area_skm)
# 11 sq km lower limit == 1100 ha


