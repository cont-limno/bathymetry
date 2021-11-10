# setwd("../")
source("scripts/99_utils.R")
set.seed(1234)

dt_raw <- read.csv("data/lagosne_depth_predictors.csv",
  stringsAsFactors = FALSE) %>%
  # test3 <- test2 %>%
  dplyr::filter(lagos_effort == "bathymetry") %>%
  dplyr::filter(!is.na(shape_class)) %>%
  dplyr::filter(!(shape_class %in% c("neither"))) %>%
  dplyr::filter(inlake_slope_pnt < 0.25) %>%
  dplyr::filter(!is.na(reservoir_class))
# TODO: assert - any(is.na(dt_raw$ws_lake_arearatio))

data_prep <- function(dt_raw,
                      inlake_slope_var = "inlake_slope_pnt",
                      nearshore_slope_var = "nearshore_slope_mean",
                      inlake_dist_var = "dist_deepest",
                      out_path = "data/depth_predictors.csv") {

  nearshore <- read.csv("data/00_geometry/nearshore.csv",
    stringsAsFactors = FALSE) %>%
    dplyr::select(lagoslakeid = llid, slope_mean = all_of(nearshore_slope_var),
      inlake_slope = all_of(inlake_slope_var))
  dt_raw <- left_join(dt_raw, nearshore, by = "lagoslakeid") %>%
    dplyr::filter(!is.na(slope_mean))

  # calculate the trig products as covariates
  # pred_grid <- expand.grid(
  #   slope = c("inlake_slope", "slope_mean"),
  #   dist = c("dist_deepest", "dist_viscenter"))
  dt_raw$slope_mean_norm <- scales::rescale(dt_raw$slope_mean,
    c(min(dt_raw$inlake_slope, na.rm = TRUE),
      max(dt_raw$inlake_slope, na.rm = TRUE)))
  # plot(dt_raw$slope_mean_norm, dt_raw$inlake_slope)
  # abline(0, 1)
  # fit <- lm(inlake_slope ~ slope_mean_norm, data = dt_raw)
  # dt_raw$slope_mean_fitted <- dt_raw$slope_mean_norm *
  #   broom::tidy(fit)$estimate[2] + broom::tidy(fit)$estimate[1]
  # dt_raw$slope_mean_fitted <- scales::rescale(dt_raw$slope_mean_fitted,
  #                                           c(min(dt_raw$inlake_slope),
  #                                             max(dt_raw$inlake_slope)))
  # plot(dt_raw$slope_mean_norm, dt_raw$inlake_slope)
  # lines(dt_raw$slope_mean_norm, dt_raw$slope_mean_fitted)
  # plot(dt_raw$slope_mean_fitted, dt_raw$inlake_slope)
  dt_raw$dist_viscenter_norm <- scales::rescale(dt_raw$dist_viscenter,
    c(min(dt_raw$dist_deepest),
      max(dt_raw$dist_deepest)))

  # plot(dt_raw$dist_viscenter_norm, dt_raw$dist_deepest)
  # abline(0, 1)

  dt_raw$maxdepth_true_true   <- calc_depth(
    dt_raw$inlake_slope, dt_raw$dist_deepest)
  dt_raw$maxdepth_true_false  <- calc_depth(
    dt_raw$inlake_slope, dt_raw$dist_viscenter_norm)
  dt_raw$maxdepth_false_true  <- calc_depth(
    dt_raw$slope_mean_norm, dt_raw$dist_deepest)
  dt_raw$maxdepth_false_false <- calc_depth(
    dt_raw$slope_mean_norm, dt_raw$dist_viscenter_norm)

  # plot(dt_raw$maxdepth_true_true, dt_raw$lake_maxdepth_m)
  # plot(dt_raw$maxdepth_true_false, dt_raw$lake_maxdepth_m)
  # plot(dt_raw$maxdepth_false_true, dt_raw$lake_maxdepth_m)
  # plot(dt_raw$maxdepth_false_false, dt_raw$lake_maxdepth_m)
  # abline(0, 1)

  dt <- dt_raw %>%
    dplyr::select(lagoslakeid, lake_maxdepth_m,
      maxdepth_true_true, maxdepth_true_false,
      maxdepth_false_true, maxdepth_false_false,
      inlake_slope, slope_mean,
      dist_deepest, dist_viscenter,
      reservoir_class, shape_class,
      lake_perimeter_m, lake_islandarea_ha,
      lake_elevation_m,
      lake_waterarea_ha,
      lake_shorelinedevfactor,
      ws_lake_arearatio,
      hu4_zoneid) %>%
    mutate(lagoslakeid = factor(lagoslakeid)) %>%
    mutate_if(is.character, factor) %>%
    dplyr::filter(!is.na(maxdepth_true_true))

  write.csv(dt, "data/depth_predictors.csv",
    row.names = FALSE)

  dt
}

fit_model <- function(maxdepth, dt_train, dt_test, maxdepth_vec) {
  # maxdepth <- maxdepth_vec[3]
  # dt_train <- data_train
  # dt_test  <- data_test

  pred_exclude <- c(maxdepth_vec[!(maxdepth_vec %in% maxdepth)],
    "inlake_slope", "dist_deepest", "slope_mean", "dist_viscenter",
    "reservoir_class", "shape_class")

  dt_rec <- recipe(lake_maxdepth_m ~ ., data = dt_train) %>%
    step_normalize(all_numeric(), -all_outcomes(), -maxdepth) %>%
    step_log(all_outcomes(), maxdepth) %>%
    prep()
  # dt_rec
  test_proc <- bake(dt_rec, new_data = dt_test)
  dt_jc     <- juice(dt_rec)

  preds    <- names(dt_jc)[
    !(names(dt_jc) %in% c("lagoslakeid", "lake_maxdepth_m",
      pred_exclude))]
  dt_jc <- dplyr::select(dt_jc,
    "lake_maxdepth_m", c(all_of(maxdepth), all_of(preds)))

  # fit1 <- linear_reg(penalty = 0.001) %>%
  #   set_engine("glmnet") %>%
  #   fit(lake_maxdepth_m ~ ., data = dt_jc)

  fit1 <- rand_forest(mode = "regression") %>%
    set_engine("ranger", verbose = TRUE, importance = "permutation",
      scale.permutation.importance = TRUE) %>%
    fit(lake_maxdepth_m ~ ., data = dt_jc)

  res          <- dt_test %>%
    select(lake_maxdepth_m) %>%
    mutate_all(log) %>%
    bind_cols(
      predict(fit1, new_data = test_proc[, preds])
    ) %>%
    bind_cols(test_proc[, "lagoslakeid"]) %>%
    mutate_at(vars(lake_maxdepth_m, .pred), exp)
  res$resid    <- res$lake_maxdepth_m - res$.pred
  res$maxdepth <- maxdepth

  list(res = res, fit = fit1)
}

get_metrics <- function(x) {
  # x <- dt_fits[[1]]
  fit_metrics <- yardstick::metric_set(
    yardstick::rmse, yardstick::rsq, yardstick::mape)
  res <- fit_metrics(x$res, truth = lake_maxdepth_m, estimate = .pred)
  res <- tidyr::pivot_wider(res, names_from = .metric, values_from = .estimate)
  res
}

# define data_prep alternatives
inlake_slope_alternatives     <- c("inlake_slope_pnt", "inlake_slope_pnts",
  "inlake_slope_mean",
  "inlake_slope_online_mean",
  "inlake_slopes_online_mean")
nearshore_slope_alternatives  <- c("nearshore_slope_mean",
  "nearshore_slope_online_mean",
  "nearshore_slopes_online_mean")
deepest_distance_alternatives <- c("dist_deepest", "dists_deepest")
slope_distance_alternatives   <- setNames(data.frame(expand.grid(
  inlake_slope_alternatives,
  nearshore_slope_alternatives,
  deepest_distance_alternatives,
  stringsAsFactors = FALSE
)), c("inlake_slope", "nearshore_slope", "inlake_dist"))

data_splitting <- function(dt,
                           strata_on = c("reservoir_class",
                             "shape_class")) {
  strata <- tidyr::unite(dplyr::select(dt, strata_on), "strata")$strata
  dt$strata <- strata
  data_split <- rsample::initial_split(dt, strata = "strata")
  data_train <- rsample::training(data_split)
  data_test  <- rsample::testing(data_split)

  list(data_train = dplyr::select(data_train, -strata),
    data_test = dplyr::select(data_test, -strata))
}

fit_alternative <- function(i, nl_res = c("Res", "NL"), concave_convex = c("convex", "concave")) {
  # i <- 1
  print(paste0("Fitting randomforest model: ", i))

  dt <- data_prep(dt_raw,
    slope_distance_alternatives$inlake_slope[i],
    slope_distance_alternatives$nearshore_slope[i],
    slope_distance_alternatives$inlake_dist[i])

  # select attributes
  dt <- dt %>%
    dplyr::filter(reservoir_class %in% nl_res) %>%
    dplyr::filter(shape_class %in% concave_convex)
  # unique(test$reservoir_class)
  # unique(test$shape_class)

  any(is.na(dt$maxdepth_true_true))

  if (all(c(length(nl_res), length(concave_convex)) == 2)) {
    dt_train_test <- data_splitting(dt,
      strata_on = c("reservoir_class", "shape_class"))
  } else {
    if (length(nl_res) < 2) {
      dt_train_test <- data_splitting(dt, strata_on = "shape_class")
    } else {
      dt_train_test <- data_splitting(dt, strata_on = "reservoir_class")
    }
  }
  data_train    <- dt_train_test[["data_train"]]
  data_test     <- dt_train_test[["data_test"]]

  maxdepth_vec <-  c("maxdepth_true_true", "maxdepth_true_false",
    "maxdepth_false_true", "maxdepth_false_false")

  dt_fits <- lapply(1:4, function(k)
    fit_model(maxdepth = maxdepth_vec[k],
      data_train, data_test, maxdepth_vec))

  # importance(dt_fits[[2]]$fit$fit)[
  #   rev(order(importance(dt_fits[[2]]$fit$fit)))]
  # importance(dt_fits[[3]]$fit$fit)[
  #   rev(order(importance(dt_fits[[3]]$fit$fit)))]
  # importance(dt_fits[[4]]$fit$fit)[
  #   rev(order(importance(dt_fits[[4]]$fit$fit)))]
  #
  # plot(dt_fits[[1]]$res$lake_maxdepth_m, dt_fits[[1]]$res$.pred)
  # abline(0, 1)

  dt_grid <- bind_rows(lapply(dt_fits, function(x) x$res))

  dt_metrics <-
    lapply(dt_fits, function(x) get_metrics(x)) %>%
    bind_rows() %>%
    bind_cols(data.frame(model = maxdepth_vec))

  list(dt_fits = dt_fits, dt_grid = dt_grid,
    dt_metrics = dt_metrics)
}
# fit_alternative(1)

res <- lapply(seq_len(nrow(slope_distance_alternatives)),
  function(i) {
    # all-predictors model using grid of real and proxy predictors
    fit_alternative(i)
  })

# add shape_class and reservoir_class sensitivity for Table 1
# test <- res
shape_reservoir_sensitivity <- c(
  res[1],
  list(fit_alternative(1, nl_res = "Res")),
  list(fit_alternative(1, nl_res = "NL")),
  list(fit_alternative(1, concave_convex = "convex")),
  list(fit_alternative(1, concave_convex = "concave"))
)
depth_grid_metrics <- lapply(shape_reservoir_sensitivity,
  function(x) x$dt_metrics)

# lapply res extract proxy_proxy stats and add to slope_distance_alternatives
slope_distance_alternatives$proxy_proxy_rmse <- unlist(
  lapply(res, function(x) x$dt_metrics$rmse[4]))
slope_distance_alternatives$proxy_proxy_rsq <- unlist(
  lapply(res, function(x) x$dt_metrics$rsq[4]))
slope_distance_alternatives$proxy_proxy_mape <- unlist(
  lapply(res, function(x) x$dt_metrics$mape[4]))

more_sensitive_to_slope <- function(x) {
  # x <- res[[1]]$dt_metrics
  dplyr::filter(x, model == "maxdepth_true_false")$rmse <
    dplyr::filter(x, model == "maxdepth_false_true")$rmse &
    dplyr::filter(x, model == "maxdepth_true_false")$rmse <
      dplyr::filter(x, model == "maxdepth_false_false")$rmse
}
slope_distance_alternatives$sensitive_to_slope <- unlist(
  lapply(res, function(x) more_sensitive_to_slope(x$dt_metrics))
)

# View(slope_distance_alternatives)
write.csv(slope_distance_alternatives,
  "data/01_depth_model/alternatives_metrics.csv", row.names = FALSE)

## saveRDS(dt_train, "data/01_depth_model/depth_training.rds")
saveRDS(res[[1]]$dt_fits, "data/01_depth_model/depth_fits.rds")
saveRDS(res[[1]]$dt_grid,
  "data/01_depth_model/depth_grid.rds")
saveRDS(depth_grid_metrics,
  "data/01_depth_model/depth_grid_metrics.rds")
saveRDS(res[[17]]$dt_metrics,
  "data/01_depth_model/runner_up_metrics.rds")
