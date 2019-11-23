source("scripts/99_utils.R")

hypso_ct <- read.csv("data/ct_hypso.csv", stringsAsFactors = FALSE)
hypso_mn <- read.csv("data/mn_hypso.csv", stringsAsFactors = FALSE)

# hypso_mi <- read.csv("data/mi_hypso.csv", stringsAsFactors = FALSE)
# hypso_ks <- read.csv("data/ks_hypso.csv", stringsAsFactors = FALSE)
# hypso_nh <- read.csv("data/nh_hypso.csv", stringsAsFactors = FALSE)

# merge csv's and save
res <- dplyr::bind_rows(hypso_mn, hypso_ct)
write.csv(res, "data/00_hypso/hypso.csv", row.names = FALSE)
