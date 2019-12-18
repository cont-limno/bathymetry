source("scripts/99_utils.R")

fname <- "data/00_hypso/hypso_predictors.csv"
# unlink("data/00_hypso/hypso_predictors.csv")
if(!file.exists(fname)){
  hypso_classes <- read.csv("data/00_hypso/hypso_classes.csv",
                            stringsAsFactors = FALSE)
  dt_raw <- read.csv("data/lagosus_depth_predictors.csv",
                     stringsAsFactors = FALSE)

  res <- left_join(hypso_classes, dt_raw,
                   by = c("llid"))

  write.csv(res, fname, row.names = FALSE)
}else{
  res <- read.csv(fname, stringsAsFactors = FALSE)
}
res$shape_class <- factor(res$shape_class)

if(interactive()){
  ggplot(data = dplyr::filter(res,
                              !is.na(shape_class) &
                              shape_class != "neither")) +
    geom_boxplot(aes(x = shape_class, y = lake_waterarea_ha), outlier.shape = NA) +
    ylim(c(0, 300)) +
    facet_wrap(~lake_centroidstate)

  # logistic model yes/no bowl-shaped lake
  test <- glm(shape_class ~ lake_area_ha, data = res, family = "binomial")

  library(mlr3)

  dt_sub <- res %>%
    dplyr::filter(lake_centroidstate %in% c("MN", "MI", "CT")) %>%
    dplyr::select(# & !is.na(shape_class)
    lake_waterarea_ha, shape_class, ws_mbgconhull_length_m, ws_area_ha,
    lake_islandarea_ha, lake_elevation_m, lake_connectivity_class,
    ws_lake_arearatio,
    -llid) %>%
    dplyr::filter(!is.na(shape_class)) %>%
    drop_na()

  class_task <- TaskClassif$new(
    id = "dt_sub",
    backend = dt_sub,
    target = "shape_class")
  learner    <- lrn("classif.rpart", cp = 0)
  train_set  <- sample(class_task$nrow, 0.9 * class_task$nrow)
  test_set   <- setdiff(seq_len(class_task$nrow), train_set)

  # table(dt_sub$shape_class[test_set])
  # table(dt_sub$shape_class[train_set])

  learner$train(class_task, row_ids = train_set)
  learner$importance()

  prediction <- learner$predict(class_task, row_ids = test_set)
  prediction$confusion

  measure <- msr("classif.acc")
  prediction$score(measure)

  resampling = rsmp("cv", folds = 10L)
  rr = resample(class_task, learner, resampling, store_models = TRUE)
  pred = rr$prediction()
  pred$confusion
  pred$score(measure)
  agr = rr$aggregate(measure)

  # TODO can I get resampling predictor importance summaries?
}
