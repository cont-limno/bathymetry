source("scripts/99_utils.R")
# http://www.bathybase.org/Data/BathybaseDb.zip

dir.create("data/bathybase_bathy", showWarnings = FALSE)
if(!file.exists("data/bathybase_bathy/BathybaseDb.zip")){
  download.file("http://www.bathybase.org/Data/BathybaseDb.zip",
                "data/bathybase_bathy/BathybaseDb.zip")
  unzip("data/bathybase_bathy/BathybaseDb.zip")
}

# spider the info.json files
json_files <- list.files("data/bathybase_bathy/", pattern = "info.json",
                         recursive = TRUE,
                         full.names = TRUE, include.dirs = TRUE)
metadata <- lapply(json_files, function(x)
  as.data.frame(jsonlite::read_json(x)))
metadata <- dplyr::bind_rows(metadata)
unique(metadata$Source)


# calculate hypso

# compare to hypso.tsv files
