library(lakemorpho)
library(sf)

# LAGOSNE::lake_info(name = "Sunappee", state = "New Hampshire")
# lagosne says: 33.7 maxdepth, 11.6 mean depth

# wikilake::lake_wiki("Lake Sunapee")
# Wikipedia says: 32 maxdepth

plot(st_as_sf(inputLM$lake)$geometry)
title("Lake Sunapee")

lakeMaxDepth(inputLM)
