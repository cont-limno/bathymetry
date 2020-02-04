source("scripts/99_utils.R")
# setwd("../")

dt <- read.csv("data/lagosne_depth_predictors.csv",
               stringsAsFactors = FALSE) %>%
  dplyr::filter(!is.na(shape_class)) %>%
  dplyr::filter(!(shape_class %in% c("neither"))) %>%
  dplyr::filter(inlake_slope < 1) %>%
  dplyr::filter(!is.na(reservoir_class))

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

if(interactive()){
  library(brms)

# ---- modeling maxdepth as a multivariate response variable model

# ---- more complicated nonlinear ----

  prior1 <- prior(normal(0, 100), nlpar = "p") +
    prior(beta(2, 30), nlpar = "inlakeslope") +
    prior(lognormal(1, 1.9), nlpar = "distdeepest")

  # prior1 <- prior(normal(0, 100), nlpar = "p") +
  #   prior(normal(0, 100), nlpar = "inlakeslope") +
  #   prior(normal(0, 100), nlpar = "distdeepest")

  inits <- list(p = 0,
                inlakeslope = exp(median(dt$inlake_slope)),
                distdeepest = exp(median(dt$dist_deepest)))
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
