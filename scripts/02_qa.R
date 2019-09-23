source("scripts/99_utils.R")

# look at distributions, outliers, and area(etc) relationships -
#     broken down by data source

# make sure max depth is at least equal to the maximum sample depth from limno

dt <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE)


