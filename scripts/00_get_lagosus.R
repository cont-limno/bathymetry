source("scripts/99_utils.R")

lg <- LAGOSUS::lagosus_load("depth")
lg <- lg$depth$depth

write.csv(lg, "data/lagosus_depth.csv", row.names = FALSE)
