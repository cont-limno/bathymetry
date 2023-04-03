
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Paper
DOI](https://img.shields.io/badge/Paper-10.1080/20442041.2021.2006553-blue.svg)](https://doi.org/10.1080/20442041.2021.2006553)
[![Code
DOI](https://img.shields.io/badge/Code-10.5281/zenodo.5672711-blue.svg)](https://doi.org/10.5281/zenodo.5672711)
[![Data
DOI](https://img.shields.io/badge/Data-10.6084/m9.figshare.12722246-blue.svg)](https://doi.org/10.6084/m9.figshare.12722246)

<!--- [![Docker Build](https://img.shields.io/badge/Docker%20Image-jsta/bathymetry-green.svg)](https://cloud.docker.com/repository/docker/jsta/bathymetry) --->

Code and data for:

**Stachelek J.**, P. J. Hanly, P. A., Soranno. Accepted. Imperfect slope
measurements drive overestimation in geometric cone model of lake and
reservoir depth. Inland Waters. <doi:10.1080/20442041.2021.2006553>

### Products

Paper: [manuscript/manuscript.pdf](manuscript/manuscript.pdf)

Data: [bathymetry derived depth predictors](data/depth_predictors.csv)

| column name             | variable                   | description                                                                                                                                                                                                   |
|:------------------------|:---------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| lagoslakeid             | \-                         | unique lake identifier developed for LAGOS-US                                                                                                                                                                 |
| lake_maxdepth_m         | Max depth (m)              | lake maximum depth in meters                                                                                                                                                                                  |
| maxdepth_true_true      | \-                         | lake maximum depth calculated from inlake_slope and dist_deepest in meters                                                                                                                                    |
| maxdepth_true_false     | \-                         | lake maximum depth calculated from inlake_slope and dist_viscenter                                                                                                                                            |
| maxdepth_false_true     | \-                         | lake maximum depth calculated from slope_mean and dist_deepest in meters                                                                                                                                      |
| maxdepth_false_false    | \-                         | lake maximum depth calculated from slope_mean and dist_viscenter in meters                                                                                                                                    |
| dist_deepest            | Deepest point distance (m) | distance from the lake shoreline to the deepest point of the lake in meters                                                                                                                                   |
| dist_viscenter          | Visual center distance (m) | distance from the lake shoreline to the point furthest from the lake shoreline in meters                                                                                                                      |
| reservoir_class         | Reservoir class            | classification of lakes are natural lakes or reservoirs by way of Polus et al.                                                                                                                                |
| shape_class             | Shape class                | classification of lakes as concave or convex following Hakanson (1977)                                                                                                                                        |
| lake_perimeter_m        | Perimeter (m)              | perimeter of outer boundary of lake waterbody polygon from the NHD (excludes islands) by way of the LAGOS-US Locus module                                                                                     |
| lake_islandarea_ha      | Island area (ha)           | surface area of islands within outer boundary of lake waterbody polygon from NHD by way of the LAGOS-US Locus module                                                                                          |
| lake_elevation_m        | Elevation (m)              | the elevation of the lake polygon centroid, in meters (referenced to the North American Vertical Datum of 1988 (NAVD88)) and obtained from the National Elevation Dataset by way of the LAGOS-US Locus module |
| lake_waterarea_ha       | Area (ha)                  | surface area of lake waterbody polygon from NHD (excludes islands) by way of the LAGOS-US Locus module                                                                                                        |
| lake_shorelinedevfactor | Shoreline development      | shoreline development factor calculated as the lake_perimeter_m / (2*sqrt(pi*lake_waterarea_ha\*10000)) by way of the LAGOS-US Locus module                                                                   |
| ws_lake_arearatio       | Watershed-lake ratio       | ratio between interlake watershed area and lake water area by way of the LAGOS-US Locus module                                                                                                                |
| hu4_zoneid              | HUC4 ID                    | unique identifier assigned by LAGOS-US for zones in the spatial division HU4                                                                                                                                  |

Data: [bathymetry derived lake
depth](data/00_bathy_depth/00_bathy_depth.csv)

| column name                 | variable       | description                                                                                                                                                                                                                                                                 |
|:----------------------------|:---------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| llid                        | \-             | unique lake identifier developed for LAGOS-US                                                                                                                                                                                                                               |
| state                       | State          | abbreviation of the state used to search for a lake’s depth                                                                                                                                                                                                                 |
| max_depth_m                 | Max depth (m)  | lake maximum depth in meters                                                                                                                                                                                                                                                |
| mean_depth_m                | Mean depth (m) | lake mean depth in meters                                                                                                                                                                                                                                                   |
| source                      | \-             | url link to the source of depth data                                                                                                                                                                                                                                        |
| lake_waterarea_ha           | Area (ha)      | surface area of lake waterbody polygon from NHD (excludes islands) by way of the LAGOS-US Locus module                                                                                                                                                                      |
| lake_connectivity_permanent | \-             | connectivity of focal lake to upstream features (DrainageLK = drainage lake with an upstream lake, Drainage = drainage lake with upstream stream, Headwater = lake with outlet but no inlet, Isolated = lake with no inlets or outlets) by way of the LAGOS-US Locus module |
| lake_lat_decdeg             | \-             | the latitude of the lake center point (NAD83) from the LAGOS-US Locus module                                                                                                                                                                                                |
| lake_lon_decdeg             | \-             | the longitude of the lake center point (NAD83) from the LAGOS-US Locus module                                                                                                                                                                                               |
| lat                         | \-             | the latitude of the lake center point                                                                                                                                                                                                                                       |
| long                        | \-             | the longitude of the lake center point                                                                                                                                                                                                                                      |

### Reproducibility

``` shell
# local
make -t data/00_hypso/hypso.csv
make -t data/00_bathy_depth/bathy_pnts.rds
make -t data/00_bathy_depth/00_bathy_depth.csv
make -t data/00_bathy_depth/bathy_geometry.csv
make -t data/00_hypso/hypso_classes.csv
make -t data/00_reservoir_classification/reservoir_classes_clean.csv
make -t data/lagosus_depth_predictors.csv
make -t data/lagosne_depth_predictors.csv
make -t data/00_geometry/nearshore.csv
make all
```

<!--- ```shell --->
<!--- # TODO: docker --->
<!--- ``` --->
