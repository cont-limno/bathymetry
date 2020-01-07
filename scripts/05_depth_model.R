source("scripts/99_utils.R")
# setwd("../")

dt <- read.csv("data/lagosne_depth_predictors.csv",
               stringsAsFactors = FALSE) %>%
  dplyr::filter(!is.na(shape_class))

#---- Model without shape_class ----
fit <- lm(lake_maxdepth_m ~
            buffer100m_slope_max + lake_waterarea_ha,# + shape_class,
          data = dt)
fit_dt <- data.frame(resid = fit$residuals,
                     shape_class = dt$shape_class,
                     fitted_values = fit$fitted.values,
                     lake_maxdepth_m = dt$lake_maxdepth_m,
                     stringsAsFactors = FALSE)

ggplot() +
  geom_point(data = dplyr::filter(fit_dt, shape_class == "convex"),
             aes(x = lake_maxdepth_m, y = fitted_values, color = shape_class),
             alpha = 0.3) +
  geom_point(data = dplyr::filter(fit_dt, shape_class == "concave"),
             aes(x = lake_maxdepth_m, y = fitted_values, color = shape_class),
             alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Measured max depth") + ylab("Predicted max depth") +
  labs(color = "") +
  ggtitle("Max depth ~ Lake Area + Buffer slope")
  # theme(legend.position = "none")

ggplot() +
  geom_boxplot(data = fit_dt, aes(y = resid, x = shape_class),
               outlier.shape = NA) +
  ylim(-20, 20)

#---- Model with shape_class ----
fit <- lm(lake_maxdepth_m ~
            buffer100m_slope_max + lake_waterarea_ha + shape_class,
          data = dt)
fit_dt <- data.frame(resid = fit$residuals,
                     shape_class = dt$shape_class,
                     fitted_values = fit$fitted.values,
                     lake_maxdepth_m = dt$lake_maxdepth_m,
                     stringsAsFactors = FALSE)

ggplot() +
  geom_point(data = dplyr::filter(fit_dt, shape_class == "convex"),
             aes(x = lake_maxdepth_m, y = fitted_values, color = shape_class),
             alpha = 0.3) +
  geom_point(data = dplyr::filter(fit_dt, shape_class == "concave"),
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
  geom_boxplot(data = fit_dt, aes(y = resid, x = shape_class),
               outlier.shape = NA) +
  ylim(-20, 20)

