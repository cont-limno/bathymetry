source("scripts/99_utils.R")
# setwd("../")

dt <- read.csv("data/lagosne_depth_predictors.csv",
               stringsAsFactors = FALSE) %>%
  dplyr::filter(!is.na(shape_class))

res <- list()

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
