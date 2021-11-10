source("scripts/99_utils.R")
# setwd("../")

library(mlr3)
library(mlr3learners)
library(ranger)

dt <- read.csv("data/00_hypso/hypso_predictors.csv", stringsAsFactors = FALSE)
dt$shape_class <- factor(dt$shape_class)

res <- list()

# ggplot(data = dplyr::filter(dt,
#                             !is.na(shape_class) &
#                               shape_class != "neither")) +
#   geom_boxplot(aes(x = shape_class, y = lake_waterarea_ha), outlier.shape = NA) +
#   ylim(c(0, 300)) +
#   facet_wrap(~lake_centroidstate)

# ---- model setup ----
dt_sub       <- dt %>%
  # dplyr::filter(lake_centroidstate %in% c("MN", "MI", "CT")) %>%
  dplyr::filter(!is.na(shape_class) & shape_class != "neither") %>%
  mutate(shape_class = droplevels(shape_class)) %>%
  dplyr::select( # buffer100m_slope_max,
    shape_class, lake_elevation_m, lake_waterarea_ha, lake_islandarea_ha,
    lake_shorelinedevfactor_nounits, lake_connectivity_class,
    lake_glaciatedlatewisc, ws_area_ha, ws_lake_arearatio,
    ws_mbgconhull_length_m,
    -lagoslakeid) %>%
  dplyr::filter(!is.na(shape_class)) %>%
  drop_na()

class_task <- TaskClassif$new(
  id = "dt_sub",
  backend = dt_sub,
  target = "shape_class")
train_set  <- sample(class_task$nrow, 0.9 * class_task$nrow)
test_set   <- setdiff(seq_len(class_task$nrow), train_set)
measure    <- msr("classif.acc")

# ---- fit rpart model ----
learner    <- lrn("classif.rpart", cp = 0)
learner$train(class_task, row_ids = train_set)
learner$importance()
learner$predict(class_task, row_ids = test_set)$confusion
learner$predict(class_task, row_ids = test_set)$score(measure)
resampling <- rsmp("cv", folds = 10L)
rr <- resample(class_task, learner, resampling, store_models = TRUE)
pred <- rr$prediction()
pred$confusion
pred$score(measure)

fit_rpart <- list(
  importance = learner$importance(),
  confusion = pred$confusion,
  score = pred$score(measure))

res[["rpart"]] <- fit_rpart

# ---- fit ranger model ----

learner    <- lrn("classif.ranger")
learner$train(class_task, row_ids = train_set)
learner$predict(class_task, row_ids = test_set)$confusion
learner$predict(class_task, row_ids = test_set)$score(measure)
resampling <- rsmp("cv", folds = 10L)
rr <- resample(class_task, learner, resampling, store_models = TRUE)
pred <- rr$prediction()
pred$confusion
pred$score(measure)

fit_ranger <- list(
  # importance = learner$importance(),
  confusion = pred$confusion,
  score = pred$score(measure))

res[["ranger"]] <- fit_ranger

saveRDS(res, "data/01_hypso_model/hypso_model.rds")