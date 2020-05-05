# https://github.com/ropensci/EML#putting-it-all-together
# https://github.com/EDIorg/MetadataTemplates

library(EML)

joe <- eml$creator(
  individualName = eml$individualName(
    givenName = "Joseph",
    surName = "Stachelek"),
  electronicMailAddress = "stachel2@msu.edu")

abstract <- set_TextType("data/abstract.md")

# https://vocab.lternet.edu/vocab/vocab/index.php
# http://vocab.lternet.edu/keywordDistiller/
# keywordSet

geographicDescription <- "Northeast and Midwest United States"
# st_bbox(st_transform(st_read("data/bathymetry.gpkg"), 4326))
# range(read.csv("data/depth_predictors.csv")$lake_elevation_m)
coverage <-
  set_coverage(geographicDescription = geographicDescription,
               west = -103.84, east = -67.10,
               north = 48.73, south = 37.15,
               altitudeMin = 0.2, altitudeMaximum = 833.9,
               altitudeUnits = "meter")

contact <-
  list(
    individualName = joe$individualName,
    electronicMailAddress = joe$electronicMailAddress,
    # address = "480 Wilson Road, East Lansing, MI, 48824, USA",
    organizationName = "Michigan State University",
    phone = "517-884-1769")

methods      <- set_methods("data/methods.md")

my_eml <- eml$eml(
  packageId = uuid::UUIDgenerate(),
  system = "uuid",
  dataset = eml$dataset(
    title = "Lake bathymetry data for the Northeast and Midwest United States",
    creator = joe,
    pubDate = "2020",
    intellectualRights = "https://www.mozilla.org/en-US/MPL/2.0/",
    abstract = abstract,
    # keywordSet = keywordSet,
    coverage = coverage,
    contact = contact,
    methods = methods#,
    # dataTable = eml$dataTable(
    #   entityName = "hf205-01-TPexp1.csv",
    #   entityDescription = "tipping point experiment 1",
    #   physical = physical,
    #   attributeList = attributeList)
  ))

eml_validate(my_eml)
