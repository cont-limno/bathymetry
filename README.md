
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Paper
DOI](https://img.shields.io/badge/Paper-DOI-blue.svg)](https://doi.org)
[![Code
DOI](https://img.shields.io/badge/Code-DOI-blue.svg)](https://doi.org/)
[![Docker
Build](https://img.shields.io/badge/Docker%20Image-jsta/bathymetry-green.svg)](https://cloud.docker.com/repository/docker/jsta/bathymetry)

Code and data for:

**Stachelek et al. In prep**. Bathymetry data from thousands of lakes
show that lake depth prediction is confounded by difficulty modeling
inlake slope.

### Products

Figures: [manuscript/combined.pdf](manuscript/combined.pdf)

Data: [bathymetry derived depth predictors](data/depth_predictors.csv)

| column name                       | variable                   | description                                                                                                                                                                                                   |
| :-------------------------------- | :------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| lagoslakeid                       | \-                         | unique lake identifier developed for LAGOS-US                                                                                                                                                                 |
| lake\_maxdepth\_m                 | Max depth (m)              | lake maximum depth in meters                                                                                                                                                                                  |
| maxdepth\_true\_true              | \-                         | lake maximum depth calculated from inlake\_slope and dist\_deepest in meters                                                                                                                                  |
| maxdepth\_true\_false             | \-                         | lake maximum depth calculated from inlake\_slope and dist\_viscenter                                                                                                                                          |
| maxdepth\_false\_true             | \-                         | lake maximum depth calculated from slope\_mean and dist\_deepest in meters                                                                                                                                    |
| maxdepth\_false\_false            | \-                         | lake maximum depth calculated from slope\_mean and dist\_viscenter in meters                                                                                                                                  |
| inlake\_slope                     | Inlake slope (m/m)         | slope of the lake bottom assuming a straight line from the shore to the deepest point of the lake in units of depth change in meters per meter distance                                                       |
| slope\_mean                       | Mean slope (m/m)           | average slope of the land surrounding a lake in a 100m buffer                                                                                                                                                 |
| dist\_deepest                     | Deepest point distance (m) | distance from the lake shoreline to the deepest point of the lake in meters                                                                                                                                   |
| dist\_viscenter                   | Visual center distance (m) | distance from the lake shoreline to the point furthest from the lake shoreline in meters                                                                                                                      |
| reservoir\_class                  | Reservoir class            | classification of lakes are natural lakes or reservoirs by way of Polus et al.                                                                                                                                |
| shape\_class                      | Shape class                | classification of lakes as concave or convex following Hakanson (1977)                                                                                                                                        |
| lake\_perimeter\_m                | Perimeter (m)              | perimeter of outer boundary of lake waterbody polygon from the NHD (excludes islands) by way of the LAGOS-US Locus module                                                                                     |
| lake\_islandarea\_ha              | Island area (ha)           | surface area of islands within outer boundary of lake waterbody polygon from NHD by way of the LAGOS-US Locus module                                                                                          |
| lake\_elevation\_m                | Elevation (m)              | the elevation of the lake polygon centroid, in meters (referenced to the North American Vertical Datum of 1988 (NAVD88)) and obtained from the National Elevation Dataset by way of the LAGOS-US Locus module |
| lake\_waterarea\_ha               | Area (ha)                  | surface area of lake waterbody polygon from NHD (excludes islands) by way of the LAGOS-US Locus module                                                                                                        |
| lake\_shorelinedevfactor\_nounits | Shoreline development      | shoreline development factor calculated as the lake\_perimeter\_m / (2*sqrt(pi*lake\_waterarea\_ha\*10000)) by way of the LAGOS-US Locus module                                                               |
| ws\_lake\_arearatio               | Watershed-lake ratio       | ratio between interlake watershed area and lake water area by way of the LAGOS-US Locus module                                                                                                                |
| hu4\_zoneid                       | HUC4 ID                    | unique identifier assigned by LAGOS-US for zones in the spatial division HU4                                                                                                                                  |

Data: [bathymetry derived lake
depth](data/00_bathy_depth/00_bathy_depth.csv)

| column name                   | variable       | description                                                                                                                                                                                                                                                                 |
| :---------------------------- | :------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| llid                          | \-             | unique lake identifier developed for LAGOS-US                                                                                                                                                                                                                               |
| state                         | State          | abbreviation of the state used to search for a lake’s depth                                                                                                                                                                                                                 |
| max\_depth\_m                 | Max depth (m)  | lake maximum depth in meters                                                                                                                                                                                                                                                |
| mean\_depth\_m                | Mean depth (m) | lake mean depth in meters                                                                                                                                                                                                                                                   |
| source                        | \-             | url link to the source of depth data                                                                                                                                                                                                                                        |
| lake\_waterarea\_ha           | Area (ha)      | surface area of lake waterbody polygon from NHD (excludes islands) by way of the LAGOS-US Locus module                                                                                                                                                                      |
| lake\_connectivity\_permanent | \-             | connectivity of focal lake to upstream features (DrainageLK = drainage lake with an upstream lake, Drainage = drainage lake with upstream stream, Headwater = lake with outlet but no inlet, Isolated = lake with no inlets or outlets) by way of the LAGOS-US Locus module |
| lake\_lat\_decdeg             | \-             | the latitude of the lake center point (NAD83) from the LAGOS-US Locus module                                                                                                                                                                                                |
| lake\_lon\_decdeg             | \-             | the longitude of the lake center point (NAD83) from the LAGOS-US Locus module                                                                                                                                                                                               |
| lat                           | \-             | the latitude of the lake center point                                                                                                                                                                                                                                       |
| long                          | \-             | the longitude of the lake center point                                                                                                                                                                                                                                      |
