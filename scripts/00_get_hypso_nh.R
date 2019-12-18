source("scripts/99_utils.R")

# original data from:
# http://www.granit.unh.edu/cgi-bin/nhsearch?dset=bathymetry_lakes_polygons/nh

# NH data comes as either contour lines or poylgons (has lake-level labels)
#   higher numbers represent deeper depths

nh_raw <-
