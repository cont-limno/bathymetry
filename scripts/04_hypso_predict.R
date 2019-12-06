source("scripts/99_utils.R")

fname <- "data/00_hypso/hypso_predictors.csv"
if(!file.exists(fname)){
  hypso_classes <- read.csv("data/00_hypso/hypso_classes.csv",
                            stringsAsFactors = FALSE)
  lg <- LAGOSNE::lagosne_load("1.087.3")

  lg_x_walk_raw <- read.csv(
    "data/00_lagosus_locus/LAGOS_Lake_Link_v2_20191017.csv",
    stringsAsFactors = FALSE)
  lg_x_walk <- lg_x_walk_raw %>%
    dplyr::select(lagosne_lagoslakeid, lagoslakeid) %>%
    dplyr::rename(lagosus_lagoslakeid = lagoslakeid) %>%
    distinct(lagosne_lagoslakeid, .keep_all = TRUE)

  # read lagosus predictors
  lg_characteristics <- read.csv(
    "data/00_lagosus_locus/lake_characteristics_20190913.csv",
    stringsAsFactors = FALSE)
  lg_geom <- read.csv("data/00_lagosus_locus/locus_geom_export_5AUG19.csv",
                      stringsAsFactors = FALSE)
  lg_nws <- read.csv("data/00_lagosus_locus/lake_watersheds_20190829_nws.csv",
                    stringsAsFactors = FALSE)
  lg_ws <- read.csv("data/00_lagosus_locus/lake_watersheds_ws_20190829.csv",
                    stringsAsFactors = FALSE)

  # join lagosne predictors
  res <- hypso_classes %>%
    left_join(dplyr::select(lg$locus,
                            lake_area_ha, elevation_m, lagoslakeid),
              by = c("llid" = "lagoslakeid")) %>%
    left_join(dplyr::select(lg$iws, iws_ha, lagoslakeid),
              by = c("llid" = "lagoslakeid")) %>%
    left_join(dplyr::select(lg$buffer100m.lulc, lagoslakeid, buffer100m_slope_max),
              by = c("llid" = "lagoslakeid"))

  # join lagosus predictors
  res <- res %>%
    left_join(lg_x_walk, by = c("llid" = "lagosne_lagoslakeid"))
  # join shape metrics + other predictors
  res <- res %>%
    left_join(lg_characteristics, by = c("lagosus_lagoslakeid" = "lagoslakeid")) %>%
    left_join(lg_geom, by = c("lagosus_lagoslakeid" = "lagoslakeid")) %>%
    left_join(lg_nws, by = c("lagosus_lagoslakeid" = "lagoslakeid")) %>%
    left_join(lg_ws, by = c("lagosus_lagoslakeid" = "lagoslakeid")) %>%
    dplyr::select(-matches("shapeflag"), -matches("zoneid"), -matches("state"),
                  -matches("oncoast"), -matches("onlandborder"),
                  -matches("inusa"), -matches("hu4inlet"))

  write.csv(res, fname, row.names = FALSE)
}else{
  res <- read.csv(fname, stringsAsFactors = FALSE)
}

res$shape_class <- factor(res$shape_class)
# logistic model yes/no bowl-shaped lake
test <- glm(shape_class ~ lake_area_ha, data = res, family = "binomial")

library(mlr3)

class_task <- TaskClassif$new(
  backend = dplyr::select(res, lake_area_ha, shape_class,
                          ws_mbgconhull_length_m, ws_area_ha,
                          buffer100m_slope_max,
                          -llid),
  target = "shape_class", id = "shape")
learner    <- lrn("classif.rpart")
train_set  <- sample(class_task$nrow, 0.9 * class_task$nrow)
test_set   <- setdiff(seq_len(class_task$nrow), train_set)

learner$train(class_task, row_ids = train_set)

learner$importance()

prediction <- learner$predict(class_task, row_ids = test_set)
prediction$confusion

measure <- msr("classif.acc")
prediction$score(measure)

prediction$score()
