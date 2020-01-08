source("scripts/99_utils.R")
# setwd("../")

dt <- read.csv("data/00_hypso/hypso_predictors.csv", stringsAsFactors = FALSE)
dt$shape_class <- factor(dt$shape_class)

ggplot(data = dplyr::filter(dt,
                            !is.na(shape_class) &
                              shape_class != "neither")) +
  geom_boxplot(aes(x = shape_class, y = lake_waterarea_ha), outlier.shape = NA) +
  ylim(c(0, 300)) +
  facet_wrap(~lake_centroidstate)

# logistic model yes/no bowl-shaped lake
test <- glm(shape_class ~ lake_waterarea_ha + ws_mgbconhull_length_m,
            data = dplyr::filter(dt, shape_class != "neither"), family = "binomial")
hist(test$fitted.values)

library(mlr3)
library(mlr3learners)
library(ranger)

dt_sub       <- dt %>%
  # dplyr::filter(lake_centroidstate %in% c("MN", "MI", "CT")) %>%
  dplyr::filter(!is.na(shape_class) & shape_class != "neither") %>%
  mutate(shape_class = droplevels(shape_class)) %>%
  dplyr::select(# buffer100m_slope_max,
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

learner    <- lrn("classif.rpart", cp = 0)
learner$train(class_task, row_ids = train_set)
learner$importance()
learner$predict(class_task, row_ids = test_set)$confusion
learner$predict(class_task, row_ids = test_set)$score(measure)
resampling = rsmp("cv", folds = 10L)
rr = resample(class_task, learner, resampling, store_models = TRUE)
pred = rr$prediction()
pred$confusion
pred$score(measure)

# ----

learner    <- lrn("classif.ranger")
learner$train(class_task, row_ids = train_set)
learner$predict(class_task, row_ids = test_set)$confusion
learner$predict(class_task, row_ids = test_set)$score(measure)
resampling = rsmp("cv", folds = 10L)
rr = resample(class_task, learner, resampling, store_models = TRUE)
pred = rr$prediction()
pred$confusion
pred$score(measure)

## make shape classes even
# small_n      <- dt_sub %>% group_by(shape_class) %>% tally() %>% ungroup() %>%
#   slice(which.min(n)) %>% pull(n)
# dt_sub_final <- dplyr::bind_rows(
#   dplyr::filter(dt_sub, shape_class == "concave"),
#   sample_n(dplyr::filter(dt_sub, shape_class == "convex"), small_n)
#   )

## check class balance
# resampling = rsmp("subsampling")
# resampling$instantiate(class_task)
# prop.table(table(class_task$truth(resampling$train_set(1)))) # roughly same proportion
# sum(dt_sub$shape_class == "convex", na.rm = TRUE) / nrow(dt_sub)
# table(dt_sub$shape_class[test_set])
# table(dt_sub$shape_class[train_set])

## plot rpart objects
# library(dendextend)
# labels(learner$model)
