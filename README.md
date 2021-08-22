
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Paper
DOI](https://img.shields.io/badge/Paper-DOI-blue.svg)](https://doi.org)
[![Code
DOI](https://img.shields.io/badge/Code-DOI-blue.svg)](https://doi.org/)
[![Docker
Build](https://img.shields.io/badge/Docker%20Image-jsta/bathymetry-green.svg)](https://cloud.docker.com/repository/docker/jsta/bathymetry)

Code and data for:

**Stachelek et al. In prep**. Imperfect slope measurements drive
overestimation in geometric cone model of lake and reservoir depth

### Products

Figures: [manuscript/combined.pdf](manuscript/combined.pdf)

Data: [bathymetry derived depth predictors](data/depth_predictors.csv)

<table>
<thead>
<tr>
<th style="text-align:left;">
column name
</th>
<th style="text-align:left;">
variable
</th>
<th style="text-align:left;">
description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
lagoslakeid
</td>
<td style="text-align:left;">

-   </td>
    <td style="text-align:left;">
    unique lake identifier developed for LAGOS-US
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    lake\_maxdepth\_m
    </td>
    <td style="text-align:left;">
    Max depth (m)
    </td>
    <td style="text-align:left;">
    lake maximum depth in meters
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    maxdepth\_true\_true
    </td>
    <td style="text-align:left;">

    -   </td>
        <td style="text-align:left;">
        lake maximum depth calculated from inlake\_slope and
        dist\_deepest in meters
        </td>
        </tr>
        <tr>
        <td style="text-align:left;">
        maxdepth\_true\_false
        </td>
        <td style="text-align:left;">

        -   </td>
            <td style="text-align:left;">
            lake maximum depth calculated from inlake\_slope and
            dist\_viscenter
            </td>
            </tr>
            <tr>
            <td style="text-align:left;">
            maxdepth\_false\_true
            </td>
            <td style="text-align:left;">

            -   </td>
                <td style="text-align:left;">
                lake maximum depth calculated from slope\_mean and
                dist\_deepest in meters
                </td>
                </tr>
                <tr>
                <td style="text-align:left;">
                maxdepth\_false\_false
                </td>
                <td style="text-align:left;">

                -   </td>
                    <td style="text-align:left;">
                    lake maximum depth calculated from slope\_mean and
                    dist\_viscenter in meters
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    dist\_deepest
                    </td>
                    <td style="text-align:left;">
                    Deepest point distance (m)
                    </td>
                    <td style="text-align:left;">
                    distance from the lake shoreline to the deepest
                    point of the lake in meters
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    dist\_viscenter
                    </td>
                    <td style="text-align:left;">
                    Visual center distance (m)
                    </td>
                    <td style="text-align:left;">
                    distance from the lake shoreline to the point
                    furthest from the lake shoreline in meters
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    reservoir\_class
                    </td>
                    <td style="text-align:left;">
                    Reservoir class
                    </td>
                    <td style="text-align:left;">
                    classification of lakes are natural lakes or
                    reservoirs by way of Polus et al. 
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    shape\_class
                    </td>
                    <td style="text-align:left;">
                    Shape class
                    </td>
                    <td style="text-align:left;">
                    classification of lakes as concave or convex
                    following Hakanson (1977)
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    lake\_perimeter\_m
                    </td>
                    <td style="text-align:left;">
                    Perimeter (m)
                    </td>
                    <td style="text-align:left;">
                    perimeter of outer boundary of lake waterbody
                    polygon from the NHD (excludes islands) by way of
                    the LAGOS-US Locus module
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    lake\_islandarea\_ha
                    </td>
                    <td style="text-align:left;">
                    Island area (ha)
                    </td>
                    <td style="text-align:left;">
                    surface area of islands within outer boundary of
                    lake waterbody polygon from NHD by way of the
                    LAGOS-US Locus module
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    lake\_elevation\_m
                    </td>
                    <td style="text-align:left;">
                    Elevation (m)
                    </td>
                    <td style="text-align:left;">
                    the elevation of the lake polygon centroid, in
                    meters (referenced to the North American Vertical
                    Datum of 1988 (NAVD88)) and obtained from the
                    National Elevation Dataset by way of the LAGOS-US
                    Locus module
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    lake\_waterarea\_ha
                    </td>
                    <td style="text-align:left;">
                    Area (ha)
                    </td>
                    <td style="text-align:left;">
                    surface area of lake waterbody polygon from NHD
                    (excludes islands) by way of the LAGOS-US Locus
                    module
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    lake\_shorelinedevfactor
                    </td>
                    <td style="text-align:left;">
                    Shoreline development
                    </td>
                    <td style="text-align:left;">
                    shoreline development factor calculated as the
                    lake\_perimeter\_m /
                    (2*sqrt(pi*lake\_waterarea\_ha\*10000)) by way of
                    the LAGOS-US Locus module
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    ws\_lake\_arearatio
                    </td>
                    <td style="text-align:left;">
                    Watershed-lake ratio
                    </td>
                    <td style="text-align:left;">
                    ratio between interlake watershed area and lake
                    water area by way of the LAGOS-US Locus module
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    hu4\_zoneid
                    </td>
                    <td style="text-align:left;">
                    HUC4 ID
                    </td>
                    <td style="text-align:left;">
                    unique identifier assigned by LAGOS-US for zones in
                    the spatial division HU4
                    </td>
                    </tr>
                    </tbody>
                    </table>

Data: [bathymetry derived lake
depth](data/00_bathy_depth/00_bathy_depth.csv)

<table>
<thead>
<tr>
<th style="text-align:left;">
column name
</th>
<th style="text-align:left;">
variable
</th>
<th style="text-align:left;">
description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
llid
</td>
<td style="text-align:left;">

-   </td>
    <td style="text-align:left;">
    unique lake identifier developed for LAGOS-US
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    state
    </td>
    <td style="text-align:left;">
    State
    </td>
    <td style="text-align:left;">
    abbreviation of the state used to search for a lake’s depth
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    max\_depth\_m
    </td>
    <td style="text-align:left;">
    Max depth (m)
    </td>
    <td style="text-align:left;">
    lake maximum depth in meters
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    mean\_depth\_m
    </td>
    <td style="text-align:left;">
    Mean depth (m)
    </td>
    <td style="text-align:left;">
    lake mean depth in meters
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    source
    </td>
    <td style="text-align:left;">

    -   </td>
        <td style="text-align:left;">
        url link to the source of depth data
        </td>
        </tr>
        <tr>
        <td style="text-align:left;">
        lake\_waterarea\_ha
        </td>
        <td style="text-align:left;">
        Area (ha)
        </td>
        <td style="text-align:left;">
        surface area of lake waterbody polygon from NHD (excludes
        islands) by way of the LAGOS-US Locus module
        </td>
        </tr>
        <tr>
        <td style="text-align:left;">
        lake\_connectivity\_permanent
        </td>
        <td style="text-align:left;">

        -   </td>
            <td style="text-align:left;">
            connectivity of focal lake to upstream features (DrainageLK
            = drainage lake with an upstream lake, Drainage = drainage
            lake with upstream stream, Headwater = lake with outlet but
            no inlet, Isolated = lake with no inlets or outlets) by way
            of the LAGOS-US Locus module
            </td>
            </tr>
            <tr>
            <td style="text-align:left;">
            lake\_lat\_decdeg
            </td>
            <td style="text-align:left;">

            -   </td>
                <td style="text-align:left;">
                the latitude of the lake center point (NAD83) from the
                LAGOS-US Locus module
                </td>
                </tr>
                <tr>
                <td style="text-align:left;">
                lake\_lon\_decdeg
                </td>
                <td style="text-align:left;">

                -   </td>
                    <td style="text-align:left;">
                    the longitude of the lake center point (NAD83) from
                    the LAGOS-US Locus module
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                    lat
                    </td>
                    <td style="text-align:left;">

                    -   </td>
                        <td style="text-align:left;">
                        the latitude of the lake center point
                        </td>
                        </tr>
                        <tr>
                        <td style="text-align:left;">
                        long
                        </td>
                        <td style="text-align:left;">

                        -   </td>
                            <td style="text-align:left;">
                            the longitude of the lake center point
                            </td>
                            </tr>
                            </tbody>
                            </table>

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
```

``` shell
# docker
```
