
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Bathymetry data from thousands of lakes show that difficulty modeling in-lake slope confounds lake depth prediction

### Products

Data Summary: [manuscript/data.pdf](manuscript/data.pdf)

Figures: [manuscript/figures.pdf](manuscript/figures.pdf)

Tables: [manuscript/tables.pdf](manuscript/tables.pdf)

Data: [bathymetry derived lake
depth](data/00_bathy_depth/00_bathy_depth.csv)

| column name                   | description                                                                                                                                                                                                                                                                 |
| :---------------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| llid                          | unique lake identifier developed for LAGOS-US                                                                                                                                                                                                                               |
| state                         | abbreviation of the state used to search for a lake’s depth                                                                                                                                                                                                                 |
| max\_depth\_m                 | lake maximum depth in meters                                                                                                                                                                                                                                                |
| mean\_depth\_m                | lake mean depth in meters                                                                                                                                                                                                                                                   |
| source                        | url link of the source of depth data                                                                                                                                                                                                                                        |
| lake\_waterarea\_ha           | surface area of lake waterbody polygon from NHD (excludes islands) by way of the LAGOS-US Locus module                                                                                                                                                                      |
| lake\_connectivity\_permanent | connectivity of focal lake to upstream features (DrainageLK = drainage lake with an upstream lake, Drainage = drainage lake with upstream stream, Headwater = lake with outlet but no inlet, Isolated = lake with no inlets or outlets) by way of the LAGOS-US Locus module |
| lake\_lat\_decdeg             | the latitude of the lake center point (NAD83) from the LAGOS-US Locus module                                                                                                                                                                                                |
| lake\_lon\_decdeg             | the longitude of the lake center point (NAD83) from the LAGOS-US Locus module                                                                                                                                                                                               |
| lat                           | the latitude of the lake center point                                                                                                                                                                                                                                       |
| long                          | the longitude of the lake center point                                                                                                                                                                                                                                      |

Data: [bathymetry derived depth
predictors](data/depth_predictors.csv)

| column name                       | description                                                                                                                                                                                                   |
| :-------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| lagoslakeid                       | unique lake identifier developed for LAGOS-US                                                                                                                                                                 |
| lake\_maxdepth\_m                 | lake maximum depth in meters                                                                                                                                                                                  |
| maxdepth\_true\_true              | lake maximum depth calculated from inlake\_slope and dist\_deepest in meters                                                                                                                                  |
| maxdepth\_true\_false             | lake maximum depth calculated from inlake\_slope and dist\_viscenter                                                                                                                                          |
| maxdepth\_false\_true             | lake maximum depth calculated from slope\_mean and dist\_deepest in meters                                                                                                                                    |
| maxdepth\_false\_false            | lake maximum depth calculated from slope\_mean and dist\_viscenter in meters                                                                                                                                  |
| inlake\_slope                     | slope of the lake bottom assuming a straight line from the shore to the deepest point of the lake in units of depth change in meters per meter distance                                                       |
| slope\_mean                       | average slope of the land surrounding a lake in a 100m buffer                                                                                                                                                 |
| dist\_deepest                     | distance from the lake shoreline to the deepest point of the lake in meters                                                                                                                                   |
| dist\_viscenter                   | distance from the lake shoreline to the point furthest from the lake shoreline in meters                                                                                                                      |
| reservoir\_class                  | classification of lakes are natural lakes or reservoirs by way of Polus et al.                                                                                                                                |
| shape\_class                      | classification of lakes as concave or convex following Hakanson (1977)                                                                                                                                        |
| lake\_perimeter\_m                | perimeter of outer boundary of lake waterbody polygon from the NHD (excludes islands) by way of the LAGOS-US Locus module                                                                                     |
| lake\_islandarea\_ha              | surface area of islands within outer boundary of lake waterbody polygon from NHD by way of the LAGOS-US Locus module                                                                                          |
| lake\_elevation\_m                | the elevation of the lake polygon centroid, in meters (referenced to the North American Vertical Datum of 1988 (NAVD88)) and obtained from the National Elevation Dataset by way of the LAGOS-US Locus module |
| lake\_waterarea\_ha               | surface area of lake waterbody polygon from NHD (excludes islands) by way of the LAGOS-US Locus module                                                                                                        |
| lake\_shorelinedevfactor\_nounits | shoreline development factor calculated as the lake\_perimeter\_m / (2*√(π*lake\_waterarea\_ha\*10000)) by way of the LAGOS-US Locus module                                                                   |
| ws\_lake\_arearatio               | ratio between interlake watershed area and lake water area by way of the LAGOS-US Locus module                                                                                                                |
| hu4\_zoneid                       | unique identifier assigned by LAGOS-US for zones in the spatial division HU4                                                                                                                                  |
