source("scripts/99_utils.R")

fname <- "data/00_hypso/hypso_predictors.csv"
# res <- read.csv(fname, stringsAsFactors = FALSE)
# unlink("data/00_hypso/hypso_predictors.csv")

res <- read.csv("data/lagosus_depth_predictors.csv",
                   stringsAsFactors = FALSE) %>%
  distinct(lagoslakeid, .keep_all = TRUE)
res$shape_class <- factor(res$shape_class)

write.csv(res, fname, row.names = FALSE)
# res <- read.csv(fname, stringsAsFactors = FALSE)
