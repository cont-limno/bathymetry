
<!-- README.md is generated from README.Rmd. Please edit that file -->

## LAGOS-US-Depth: Lake depth at continental scales

### Products

Data Summary: [manuscript/data.pdf](manuscript/data.pdf)

Figures: [manuscript/figures.pdf](manuscript/figures.pdf)

Tables: [manuscript/tables.pdf](manuscript/tables.pdf)

Data:
[data/lagosus\_depth.csv](data/lagosus_depth.csv)

### Data dictionary

| Variable name               | Description                                                                                            |
| :-------------------------- | :----------------------------------------------------------------------------------------------------- |
| lagoslakeid                 | unique lake identifier developed for LAGOS-US                                                          |
| lake\_namegnis              | lake name from the gnis database by way of the LAGOS-US Locus module                                   |
| lake\_states                | abbreviation(s) of state(s) intersecting the lake polygon from the LAGOS-US Locus module               |
| lake\_state                 | abbreviation of the state used to search for a lake’s depth                                            |
| lake\_lat\_decdeg           | the latitude of the lake center point (NAD83) from the LAGOS-US Locus module                           |
| lake\_lon\_decdeg           | the longitude of the lake center point (NAD83) from the LAGOS-US Locus module                          |
| lake\_maxdepth\_m           | lake maximum depth in meters                                                                           |
| lake\_meandepth\_m          | lake mean depth in meters                                                                              |
| lake\_waterarea\_ha         | surface area of lake waterbody polygon from NHD (excludes islands) by way of the LAGOS-US Locus module |
| sourcename\_depth           | name of the source of depth data                                                                       |
| sourceurl\_depth            | url link of the source of depth data                                                                   |
| sourcetype\_depth           | type of the source of depth data; one of Citizen Monitoring, Government, University, Commercial        |
| lagos\_effort               | name of depth searching effort used for internal tracking; one of LAGOSNE, LAGOSUS, bathymetry, NLA    |
| lagos\_effort\_reliablility | reliability of depth searching effort used for quality assurance and de-duplication                    |
