source("scripts/99_utils.R")

# lg <- LAGOSUS::lagosus_load("depth")
# lg <- lg$depth$depth

# use all depths product
lg <- read.csv("../lagos_depth/data/lagosus_depth_all.csv", stringsAsFactors = FALSE)

write.csv(lg, "data/lagosus_depth.csv", row.names = FALSE)
