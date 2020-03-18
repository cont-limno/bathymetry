# setwd("../")
source("scripts/99_utils.R")

dt_raw <- read.csv("data/lagosne_depth_predictors.csv",
               stringsAsFactors = FALSE) %>%
  dplyr::filter(lagos_effort == "bathymetry") %>%
  dplyr::filter(!is.na(shape_class)) %>%
  dplyr::filter(!(shape_class %in% c("neither"))) %>%
  dplyr::filter(inlake_slope < 1) %>%
  dplyr::filter(!is.na(reservoir_class))
nearshore <- read.csv("data/00_geometry/nearshore.csv",
                      stringsAsFactors = FALSE) %>%
  dplyr::select(lagoslakeid = llid, slope_mean)
dt_raw <- left_join(dt_raw, nearshore) %>%
  dplyr::filter(!is.na(slope_mean))

# data prep
library(recipes)
library(rsample)
library(parsnip)
library(yardstick)
set.seed(1234)

dt <- dt_raw %>%
  dplyr::select(lagoslakeid, lake_maxdepth_m, inlake_slope, slope_mean,
                dist_deepest, dist_viscenter,
                reservoir_class, shape_class,
                lake_perimeter_m, lake_islandarea_ha,
                lake_elevation_m,
                lake_waterarea_ha, # oliver covariates
                lake_shorelinedevfactor_nounits,
                ws_lake_arearatio,
                hu4_zoneid) %>%
  mutate(lagoslakeid = factor(lagoslakeid)) %>%
  mutate_if(is.character, factor)

dt_split <- initial_split(dt)
dt_train <- training(dt_split)
dt_test  <- testing(dt_split)

dt_rec <- recipe(lake_maxdepth_m ~ ., data = dt_train) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_log(all_outcomes()) %>%
  prep()
# dt_rec
test_proc <- bake(dt_rec, new_data = dt_test)
dt_jc     <- juice(dt_rec)

# ---- all-predictors model using grid of real and proxy predictors ----
pred_grid <- expand.grid(
  slope = c("inlake_slope", "slope_mean"),
  dist = c("dist_deepest", "dist_viscenter"))

fit_model <- function(x, exclude){
  # x <- dt_jc
  # exclude <- as.character(unlist(pred_grid[1,]))
  preds <- names(x)[!(names(x) %in% c("lagoslakeid", "lake_maxdepth_m",
                                      exclude))]
  x     <- dplyr::select(x, c("lake_maxdepth_m", preds))

  fit1 <- linear_reg(penalty = 0.001) %>%
    set_engine("glmnet") %>%
    fit(lake_maxdepth_m ~ ., data = x)

  dt_test %>%
    select(lake_maxdepth_m) %>%
    mutate(lake_maxdepth_m = log(lake_maxdepth_m)) %>%
    bind_cols(
      predict(fit1, new_data = test_proc[, preds])
    ) %>%
    bind_cols(test_proc[,"lagoslakeid"]) %>%
    mutate(resid = lake_maxdepth_m - .pred,
           model = paste0(exclude, collapse = "-"))
}

dt_fits <- lapply(1:4, function(i)
  fit_model(x = dt_jc, exclude = as.character(unlist(pred_grid[i,]))))

plot(dt_fits[[1]]$lake_maxdepth_m, dt_fits[[1]]$.pred)
abline(0, 1)
dt_fits[[1]][dt_fits[[1]]$lake_maxdepth_m > 4,]

dt_metrics <- lapply(dt_fits, function(x)
  tidyr::pivot_wider(
    metrics(x, truth = lake_maxdepth_m, estimate = .pred),
    names_from = .metric, values_from = .estimate)) %>%
  bind_rows() %>%
  bind_cols(pred_grid[4:1,])

saveRDS(dt_train, "data/01_depth_model/depth_training.rds")
saveRDS(bind_rows(dt_fits),
        "data/01_depth_model/depth_grid.rds")
saveRDS(dt_metrics,
        "data/01_depth_model/depth_grid_metrics.rds")

if(interactive()){

# fit a model for inlake_slope and dist_deepest
lgus_predictors <- read.csv("data/lagosne_depth_predictors.csv",
                            stringsAsFactors = FALSE) %>%
  select_if(is.numeric) %>%
  select(-contains("code"), -contains("ftype"), -contains("huc12"),
         -contains("lagoslakeid"),
         -has_limno, -contains("depth_m"),
         # -contains("focallakewater"),
         -contains("width"), -contains("length"),
         -contains("totalarea"),
         -contains("nws"), -contains("orientation"),
         -contains("ws_"), -contains("max"),
         -dist_between, -shape_offset
         )

####
inlake_slope_rec <- recipe(inlake_slope ~ ., data = lgus_predictors) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_log(all_outcomes()) %>%
  prep()
dt_inlake_slope <- juice(inlake_slope_rec) %>%
  dplyr::select(-dist_deepest)

fit <- lm(inlake_slope ~ ., data = dt_inlake_slope)
# hist(residuals(fit))
fit <- broom::tidy(fit) %>%
  dplyr::arrange(desc(abs(estimate)))
jsta::pdf_table(fit, "slope.pdf")
head(fit)

# slope is dependent on the arrangement of hydro features

####

dist_deepest_rec <- recipe(dist_deepest ~ ., data = lgus_predictors) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_log(all_outcomes()) %>%
  prep()
dt_dist_deepest <- juice(dist_deepest_rec) %>%
  dplyr::select(-inlake_slope)

fit <- lm(dist_deepest ~ ., data = dt_dist_deepest)
# hist(residuals(fit))
fit <- broom::tidy(fit) %>%
  dplyr::arrange(desc(abs(estimate)))
jsta::pdf_table(fit, "dist.pdf")
head(fit)

# dist is dependent on the size of the hydro system


# ---- joint prediction of slope and distance using sam's covariates
library(brms)

fit1 <- brm(
  mvbind(inlake_slope, dist_deepest) ~ lake_waterarea_ha +
    lake_shorelinedevfactor_nounits + ws_lake_arearatio,
  data = dt_jc, chains = 2
)

# pp_check(fit1, resp = "distdeepest")
# pp_check(fit1, resp = "inlakeslope")
# bayes_R2(fit1)

# 2nd stage model of max depth using posterior slope and distance as covariates
# use tidybayes package

fit2 <- brm(
  lake_maxdepth_m ~ dist_deepest + inlake_slope, data = dt_jc_2nd,
  chains = 2
)

bayes_R2(fit2)

# plot(calc_depth(dt$inlake_slope, dt$dist_deepest), dt$lake_maxdepth_m)
# hist(dt$inlake_slope)
# hist(dt$dist_deepest)

max_depth_s    <- dt$lake_maxdepth_m
inlake_slope_s <- log(dt$inlake_slope)
dist_deepest_s <- log(dt$dist_deepest)

dt <- dt %>%
  dplyr::mutate_if(is.numeric, scale) %>%
  mutate(inlake_slope = inlake_slope_s,
         dist_deepest = dist_deepest_s,
         lake_maxdepth_m = max_depth_s)

  library(brms)

# ---- focus on modelling inlake_slope ----

# breakpoint model before and after shoreline

# gaussian processes

# kriging


# ---- more complicated nonlinear ----

  prior1 <- prior(normal(0, 100), nlpar = "p") +
    prior(normal(1, 0.5), nlpar = "inlakeslope") +
    prior(lognormal(1, 1.9), nlpar = "distdeepest")

  # prior1 <- prior(normal(0, 100), nlpar = "p") +
  #   prior(normal(0, 100), nlpar = "inlakeslope") +
  #   prior(normal(0, 100), nlpar = "distdeepest")

  inits <- list(p = 0,
                inlakeslope = 0.1,
                distdeepest = median(exp(dt$dist_deepest)))
  inits_list <- list(inits, inits, inits, inits)
  fit <- brm(
    bf(lake_maxdepth_m ~ p * tan(inlakeslope) * distdeepest,
    inlakeslope ~ buffer100m_slope_mean +
      shape_class + ws_lake_arearatio + reservoir_class,
    distdeepest ~ dist_viscenter + lake_waterarea_ha + lake_shorelinedevfactor_nounits, p ~ 1, nl = TRUE),
    data = dt, prior = prior1, inits = inits_list)

# ---- hu4 random effect ----
  fit1 <- brm(
    bf(inlake_slope ~ buffer100m_slope_mean +
         shape_class + ws_lake_arearatio + reservoir_class + (1 + buffer100m_slope_mean | hu4_zoneid)), data = dt)

# ---- modeling maxdepth as a calculated quantity ----
  # model inlake_slope ~ buffer_slope + covariates
  # model deepest_dist ~ viscenter_dist + covariates
  # compute depth using mcmc chains
  # https://discourse.mc-stan.org/t/using-brms-to-create-generated-quantities-stan-code-block/11766/4
  if(!file.exists("fit1.rds")){
    # unlink("fit1.rds")
    fit1 <- brm(
      bf(inlake_slope ~ buffer100m_slope_mean +
           shape_class + ws_lake_arearatio + reservoir_class + (1 + buffer100m_slope_mean | hu4_zoneid)), data = dt)
    saveRDS(fit1, "fit1.rds")
  }else{
    fit1 <- readRDS("fit1.rds")
  }

  if(!file.exists("fit2.rds")){
    # unlink("fit2.rds")
    fit2 <- brm(
      bf(dist_deepest ~ dist_viscenter + lake_waterarea_ha + lake_shorelinedevfactor_nounits), data = dt)
    saveRDS(fit2, "fit2.rds")
  }else{
    fit2 <- readRDS("fit2.rds")
  }

  inlake_slope_pred <- fitted(fit1)
  dist_deepest_pred <- fitted(fit2)

  fit <- lm(dt$lake_maxdepth_m ~ tan(exp(inlake_slope_pred[,1])) + exp(dist_deepest_pred[,1]))

  plot(fitted(fit), dt$lake_maxdepth_m)
  abline(0, 1)

  yardstick::rmse(data.frame(measured = dt$lake_maxdepth_m,
                             predicted = fitted(fit)),
                  measured, predicted)

  plot(exp(dt$inlake_slope), exp(inlake_slope_pred[,1]))
  abline(0, 1)

  test2 <- fitted(fit2)
  hist(test[,1])
  plot(exp(dt$dist_deepest), exp(test[,1]))
  abline(0, 1)

  res <- calc_depth(exp(test1[,1]), exp(test2[,1]))
  hist(res, xlim = c(0, 150), n = 100)
  hist(dt$lake_maxdepth_m)
  plot(dt$lake_maxdepth_m, res, ylim = c(0, 150))
  abline(0, 1)

  plot(log(dt$lake_maxdepth_m), log(res))
  abline(0, 1)

  test1 <- data.frame(measured = dt$lake_maxdepth_m,
                      predicted = res)
  yardstick::rmse(test1, measured, predicted)

  test2 <- data.frame(measured = dt$lake_maxdepth_m,
                      predicted = calc_depth(exp(dt$inlake_slope),
                                             exp(dt$dist_deepest)))
  yardstick::rmse(test2, measured, predicted)


  # compute maxdepth using mcmc chains
  inlake_slope_pred <- predict(fit1,
                              resp = "inlake_slope",
                              # transform = exp,
                              # summary = TRUE,
                              probs = 0.5,
                              newdata = dt)

  par(mfrow = c(1, 2))
  hist(dt$inlake_slope)
  hist(inlake_slope_pred[,1])
  test <- predictive_interval(fit1, 0.01)[,1]
  test <- posterior_predict(fit1)
  head(predictive_interval(fit1, 0.1))
  hist(predictive_interval(fit1, 0.01)[,1])
  hist(brms::predictive_interval(fit1)[,2])
  hist(brms::predictive_interval(fit1)[,1])

  %>%
    data.frame() %>% pull(Estimate)
  dist_deepest_pred <- predict(fit2,
                               resp = "dist_deepest",
                               transform = exp,
                               summary = TRUE,
                               probs = 0.5,
                               newdata = dt) %>%
    data.frame() %>% pull(Estimate)

  plot(inlake_slope_pred, exp(dt$inlake_slope), xlim = c(0, 1), ylim = c(0, 1))
  abline(0, 1)

  plot(dist_deepest_pred, exp(dt$dist_deepest),
       xlim = c(min(exp(dt$dist_deepest), na.rm = TRUE),
                max(exp(dt$dist_deepest), na.rm = TRUE)))
  abline(0, 1)

  dt <- mutate(dt,
               lake_maxdepth_m_pred = tan(inlake_slope_pred) *
                 dist_deepest_pred)

  plot(dt$lake_maxdepth_m, dt$lake_maxdepth_m_pred, ylim = c(0, 25))

  pp_check(fit1)

# ---- bare-bones non-linear attempt ----
# prior1 <- prior(beta(2, 18), nlpar = "p") + prior(beta(2, 18), nlpar = "k")
# fit <- brm(
#   bf(lake_maxdepth_m ~ p * tan(buffer100m_slope_mean) *
#        k * dist_viscenter, p ~ 1, k ~ 1,
#      nl = TRUE), data = dt, prior = prior1)

}

#---- Model without shape_class ----
fit <- lm(lake_maxdepth_m ~
            buffer100m_slope_max + lake_waterarea_ha,# + shape_class,
          data = dt)
fit_dt_noshape <- data.frame(resid = fit$residuals,
                     shape_class = dt$shape_class,
                     fitted_values = fit$fitted.values,
                     lake_maxdepth_m = dt$lake_maxdepth_m,
                     stringsAsFactors = FALSE)

res[["no_shape"]] <- fit_dt_noshape

ggplot() +
  geom_point(data = dplyr::filter(fit_dt_noshape, shape_class == "convex"),
             aes(x = lake_maxdepth_m, y = fitted_values, color = shape_class),
             alpha = 0.3) +
  geom_point(data = dplyr::filter(fit_dt_noshape, shape_class == "concave"),
             aes(x = lake_maxdepth_m, y = fitted_values, color = shape_class),
             alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Measured max depth") + ylab("Predicted max depth") +
  labs(color = "") +
  ggtitle("Max depth ~ Lake Area + Buffer slope")
  # theme(legend.position = "none")

# residual = observed - predicted
# positive value = underprediction
# negative value = overprediction
ggplot() +
  geom_boxplot(data = fit_dt_noshape, aes(y = resid, x = shape_class),
               outlier.shape = NA) +
  ylim(-20, 20)

#---- Model with shape_class ----
fit <- lm(lake_maxdepth_m ~
            buffer100m_slope_max + lake_waterarea_ha + shape_class,
          data = dt)
fit_dt_shape <- data.frame(resid = fit$residuals,
                     shape_class = dt$shape_class,
                     fitted_values = fit$fitted.values,
                     lake_maxdepth_m = dt$lake_maxdepth_m,
                     stringsAsFactors = FALSE)

res[["shape"]] <- fit_dt_shape

ggplot() +
  geom_point(data = dplyr::filter(fit_dt_shape, shape_class == "convex"),
             aes(x = lake_maxdepth_m, y = fitted_values, color = shape_class),
             alpha = 0.3) +
  geom_point(data = dplyr::filter(fit_dt_shape, shape_class == "concave"),
             aes(x = lake_maxdepth_m, y = fitted_values, color = shape_class),
             alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Measured max depth") + ylab("Predicted max depth") +
  labs(color = "") +
  ggtitle("Max depth ~ Lake Area + Buffer slope + Shape class")
# theme(legend.position = "none")

# residual = observed - predicted
# positive value = underprediction
# negative value = overprediction
ggplot() +
  geom_boxplot(data = fit_dt_shape, aes(y = resid, x = shape_class),
               outlier.shape = NA) +
  ylim(-20, 20)

saveRDS(res, "data/01_depth_model/depth_model.rds")

# ---- Oliver et al. 2016 random effects model ----
library(dplyr)
# setwd("../")
oliver2015 <- LAGOSNE::lagos_load_oliver_2015() %>%
  mutate(lake_maxdepth_m_oliver = zmaxpredict)
dt         <- read.csv("data/lagosne_depth_predictors.csv",
               stringsAsFactors = FALSE) # %>%
  # dplyr::filter(shape_class %in% c("convex", "concave"))

## select specific predictors
dt <- dplyr::select(dt, lake_maxdepth_m,
                    buffer100m_slope_max, lake_waterarea_ha,
                    lake_shorelinedevfactor_nounits,
                    ws_lake_arearatio,
                    shape_class, hu4_zoneid, lagoslakeid) %>%
  mutate(lagoslakeid = as.character(lagoslakeid))

## standardize predictors and response vars
# all predictors were log transformed, centered, and scaled
# depth was log transformed
scale2 <- function(x, na.rm = TRUE){
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
}
dt <- dt %>%
  dplyr::mutate_if(is.numeric, log) %>%
  dplyr::mutate_at(vars(buffer100m_slope_max:ws_lake_arearatio),
                   scale2)

## create a holdout dataset using 10% of the data from each hu4
train <- group_by(dt, hu4_zoneid) %>% dplyr::sample_frac(.9)
test  <- dplyr::anti_join(dt, train, by = 'lagoslakeid')

## fit model
library(lme4)
eq <- as.formula("lake_maxdepth_m ~ buffer100m_slope_max + lake_waterarea_ha + (lake_waterarea_ha | hu4_zoneid)")
fit <- lme4::lmer(eq, data = train, REML = FALSE)
test$lake_maxdepth_m_predicted <- predict(fit, test)

saveRDS(list(fit = fit, test = test, train = train),
        "data/01_depth_model/oliver_model.rds")

if(interactive()){
  plot(test$lake_maxdepth_m_predicted, test$lake_maxdepth_m, xlim = c(0, 5), ylim = c(0, 5))
  abline(0, 1)

  ## calculate rmse
  yardstick::rmse(test, exp(lake_maxdepth_m), exp(lake_maxdepth_m_predicted))
  yardstick::rsq(test, exp(lake_maxdepth_m), exp(lake_maxdepth_m_predicted))

  test <- test %>%
    left_join(dplyr::select(dplyr::mutate(oliver2015, lagoslakeid = as.character(lagoslakeid)), lagoslakeid,
                            lake_maxdepth_m_oliver))
  plot(exp(test$lake_maxdepth_m_predicted), test$lake_maxdepth_m_oliver)
  abline(0, 1)

  plot(oliver2015$zmaxpredict, oliver2015$zmaxobs)
  abline(0, 1)
}
