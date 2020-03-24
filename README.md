
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Bathymetry data from thousands of lakes show that difficulty modeling in-lake slope limits lake depth prediction

### Products

Data Summary: [manuscript/data.pdf](manuscript/data.pdf)

Figures: [manuscript/figures.pdf](manuscript/figures.pdf)

Tables: [manuscript/tables.pdf](manuscript/tables.pdf)

Data: [bathymetry derived lake
depth](data/00_bathy_depth/00_bathy_depth.csv)

<details>

<summary>Data dictionary</summary>

col\_name description  
—————————-
—————————————————————————————————————————————————————————————————————————————————————————-
llid unique lake identifier developed for LAGOS-US  
state abbreviation of the state used to search for a lake’s depth  
max\_depth\_m lake maximum depth in meters  
mean\_depth\_m lake mean depth in meters  
source url link of the source of depth data  
effort NA  
lake\_waterarea\_ha surface area of lake waterbody polygon from NHD
(excludes islands) by way of the LAGOS-US Locus module  
lake\_connectivity\_permanent connectivity of focal lake to upstream
features (DrainageLK = drainage lake with an upstream lake, Drainage =
drainage lake with upstream stream, Headwater = lake with outlet but no
inlet, Isolated = lake with no inlets or outlets) by way of the LAGOS-US
Locus module lake\_lat\_decdeg the latitude of the lake center point
(NAD83) from the LAGOS-US Locus module  
lake\_lon\_decdeg the longitude of the lake center point (NAD83) from
the LAGOS-US Locus module  
lat the latitude of the lake center point  
long the longitude of the lake center point

</details>

 

Data: [bathymetry derived lake geometry](data/depth_predictors.csv)

<details>

<summary>Data dictionary</summary>

| field                             |
| :-------------------------------- |
| lagoslakeid                       |
| lake\_maxdepth\_m                 |
| maxdepth\_true\_true              |
| maxdepth\_true\_false             |
| maxdepth\_false\_true             |
| maxdepth\_false\_false            |
| inlake\_slope                     |
| slope\_mean                       |
| dist\_deepest                     |
| dist\_viscenter                   |
| reservoir\_class                  |
| shape\_class                      |
| lake\_perimeter\_m                |
| lake\_islandarea\_ha              |
| lake\_elevation\_m                |
| lake\_waterarea\_ha               |
| lake\_shorelinedevfactor\_nounits |
| ws\_lake\_arearatio               |
| hu4\_zoneid                       |

</details>
