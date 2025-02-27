library(rLakeAnalyzer)

## explore:
# load.bathy
exampleFilePath <- system.file('extdata', 'Sparkling.bth', package="rLakeAnalyzer")
sparkling.bathy <- load.bathy(exampleFilePath)
plot(sparkling.bathy$areas, sparkling.bathy$depths, type='l', ylim=c(20,0),
     ylab='Depths (m)', xlab='Areas (m^2)')

# ?approx.bathy
Voldev.ex        <- approx.bathy(Zmax = 25, Zmean = 12, lkeArea = 39400000,
                                 method = "voldev") # red
Voldevshallow.ex <- approx.bathy(Zmax = 25, Zmean = 6, lkeArea = 39400000,
                                 method = "voldev") # blue
Cone.ex          <- approx.bathy(Zmax = 25, lkeArea = 39400000,
                                 method = "cone") # black

# plot depth-area curves
plot(Cone.ex$depths ~ Cone.ex$Area.at.z,
     xlab = "Area (m^3)", ylab = "Depth (m)",
     ylim = rev(range(Cone.ex$depths)))
points(Voldev.ex$depths ~ Voldev.ex$Area.at.z,
       ylim = rev(range(Voldev.ex$depths)), col = "red")
points(Voldevshallow.ex$depths ~ Voldevshallow.ex$Area.at.z,
       ylim = rev(range(Voldevshallow.ex$depths)),
       col = "blue")

clipr::write_clip(
data.frame(area = Voldevshallow.ex$Area.at.z, depths = Voldevshallow.ex$depths)
)

# get.offsets


# LAGOSNE::lake_info(name = "Lake Mendota", state = "Wisconsin")
# wikilake::lake_wiki("Lake Mendota")
# zratio is max/mean depth
approx_depth <- function(zratio = NA, area = NA, method = "cone"){
        area   <- 39400000
        zratio <- 3/pi
        # if a perfect cone then zratio is 3/pi
        radius = sqrt(area/pi)

        test <- tan(atan(zratio)) * radius
}
