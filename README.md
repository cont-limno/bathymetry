
<!-- README.md is generated from README.Rmd. Please edit that file -->

## LAGOS-US-Depth: Lake depth at continental scales

[![Paper DOI](https://img.shields.io/badge/Paper-DOI-blue.svg)]()
[![Code
DOI](https://zenodo.org/badge/123951266.svg)](https://zenodo.org/badge/latestdoi/123951266)

### Data (see [data/lagosus\_depth.csv]())

| Variable name          | Description                                                        |
| :--------------------- | :----------------------------------------------------------------- |
| lagoslakeid            | unique lake identifier developed for LAGOS-US                      |
| lake\_namegnis         | lake name from the gnis database                                   |
| lake\_states           | abbreviation(s) of state(s) intersecting the lake polygon          |
| lake\_state            | abbrevaiation of the state used to search for a lake’s depth       |
| lake\_lat\_decdeg      | the latitude of the lake center point (NAD83).                     |
| lake\_lon\_decdeg      | the longitude of the lake center point (NAD83).                    |
| lake\_maxdepth\_m      | lake maximum depth in meters                                       |
| lake\_meandepth\_m     | lake mean depth in meters                                          |
| lake\_waterarea\_ha    | surface area of lake waterbody polygon from NHD (excludes islands) |
| programtype\_depth     | type of program used as the source of depth data                   |
| programlink\_depth     | link or name of the program used as the source of depth data       |
| lagos\_effort          | name of depth searching effort used for internal tracking          |
| predicted\_maxdepth\_m | lake maximum depth in meters predicted from Stachelek et. al       |

### Products

Figures: [manuscript/figures.pdf](manuscript/figures.pdf)

Tables: [manuscript/tables.pdf](manuscript/tables.pdf)

### Abstract

Lake depth is widely regarded as a critical predictor of nutrient
cycling in lakes. Unfortunately, existing databases lack widespread
information on this basic measure. Lack of depth data is a key
impediment to effective lake nutrient prediction in thousands of lakes
at regional to continental scales. Although a number of investigators
have developed predictive models of lake depth based on topography
surrounding a lake, predictions from such models have proved to be
fairly imprecise. To improve prediction of lake depth, we compiled a
database of maximum depth for more than 15,000 lakes throughout the
conterminous United States and used it to develop predictive models of
lake depth based on factors such as lake elevation, topography,
geometry, impoundment status, and landform. We used fitted models to
predict lake depth for nearly all lakes in the conterminous United
States and show how our statistical models, which are stratified on lake
and watershed characteristics, represent an improvement over existing
models. Ultimately, our lake depth predictions could be used not only as
an aid in predicting lake nutrients but also as a rough check on
estimates of total lake volume used in Earth System Models.
