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
dt <- raster("data/bathybase_bathy/1-99/1/bathy.tiff")
get_hypso <- function(rsub){
  # rsub <- rsubs[[1]]
  # rsub <- rsubs[[which(names(rsubs) == 89659)]]
  maxdepth <-  abs(cellStats(rsub, "max"))

  # define depth intervals by raster resolution
  # min_res   <- res(rsub)[1]
  min_res   <- 0.5
  # 0 to max
  depth_int <- seq(0, round(maxdepth/min_res) * min_res, by = min_res)

  # calculate area of raster between depth intervals
  # reclassify raster based on depth intervals
  # calculate area of each class
  rc <- raster::cut(rsub, breaks = depth_int) %>%
    as.data.frame()
  # drop depth_int entries for missing layers
  depth_int <- depth_int[1:length(depth_int) %in% unique(rc$layer)]
  depth_mid <- # add interval midpoints
    c(0, as.numeric(na.omit((depth_int + lag(depth_int))/2)))
  # cbind(depth_int, depth_mid)
  # length(depth_int) == length(unique(tidyr::drop_na(rc, layer)$layer))

  rc <- rc %>% tidyr::drop_na(layer) %>%
    group_by(layer) %>% tally() %>% arrange(desc(layer)) %>%
    cumsum() %>%
    mutate(area_m2 = n * res(rsub)[1] * res(rsub)[2]) %>%
    mutate(depth_int = rev(depth_mid)) %>%
    mutate(area_percent = scales::rescale(area_m2, to = c(0, 100))) %>%
    mutate(depth_percent = scales::rescale(depth_int, to = c(0, 100)))

  # plot(rc$area_percent, rc$depth_percent, ylim = c(100, 0))
  rc
}

hypso <- get_hypso(dt)

# compare to hypso.tsv files
test <- janitor::clean_names(
read_tsv("data/bathybase_bathy/1-99/1/hypso.tsv"))

plot(test$area_m_2, test$depth_m)
plot(hypso$area_m2, hypso$depth_int)
