# ---- splines2 ----
library(splines2)

knots <- c(0.3, 0.5, 0.6)
x     <- seq(0, 1, 0.01)

isOut <- iSpline(x, knots = knots, degree = 2, intercept = TRUE)

dim(isOut)

plot(x, isOut[,1])

x <- seq.int(0, 1, 0.01)
knots <- c(0.2, 0.4, 0.7, 0.9)
ibsMat <- ibs(x, knots = knots, degree = 1, intercept = TRUE)

# ---- smooth.spline ----

plot(dist ~ speed, data = cars, main = "data(cars)  &  smoothing splines")
cars.spl <- with(cars, smooth.spline(speed, dist))
cars.spl

lines(cars.spl, col = "blue")
ss10 <- smooth.spline(cars[,"speed"], cars[,"dist"], df = 10)
lines(ss10, lty = 2, col = "red")
legend(5,120,c(paste("default [C.V.] => df =",round(cars.spl$df,1)),
               "s( * , df = 10)"), col = c("blue","red"), lty = 1:2,
       bg = 'bisque')

# test on rlakeanalyzer data ----
voldevshallow <- data.frame(
        area = c(39400000, 35439986.666602, 32344460.5094566, 29690397.106071,
                 27305624.7313029, 25090742.8299854, 22983516.1241443,
                 20944837.8303206, 18951770.4118829, 16993568.590542, 15069117.6666513,
                 13185069.5407765, 11354305.4190789, 9594516.61534161,
                 7926780.07170867, 6374054.90949652, 4959558.76499986, 3705006.78659029,
                 2628716.97576323, 1743606.61288258, 1055129.52496772,
                 559238.667950055, 240514.798261364, 70710.6610567963, 8224.25144224477, 0),
      depth = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L,
                 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L)
)

plot(depth ~ area, data = voldevshallow)
voldev.spl <- with(voldevshallow[c(1:3,
                                   which.min(abs(voldevshallow$area - voldevshallow$area[1] /2))),],
                   smooth.spline(area, depth))
lines(voldev.spl, col = "blue")

predict(voldev.spl, 0)

# ---- mgcv ----

library(mgcv)

head(trees)


