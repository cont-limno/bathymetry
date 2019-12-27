source("scripts/99_utils.R")

library(lakeattributes) # remotes::install_github("USGS-R/lakeattributes")

fpath	<-	system.file('Bathy/', package = "lakeattributes")
flist <- list.files(fpath)
# list.files(fpath, include.dirs = TRUE, full.names = TRUE)
has_bathy <- data.frame(site_id = gsub(".bth", "", flist),
                        has_bathy = 1,
                        stringsAsFactors = FALSE)

get_bathy("nhd_10595614")

dt <- LAGOSNE::coordinatize(lakeattributes::location, "lat", "lon") %>%
  left_join(has_bathy) %>%
  dplyr::filter(!is.na(has_bathy)) %>%
  st_join(st_transform(usa_sf(), 4326)) %>%
  dplyr::filter(state == "WI")

compare_areas <- function(x){
  gb_cone <- get_bathy(x, cone_est = TRUE)
  gb_nocone <- get_bathy(x, cone_est = FALSE)
  if(is.null(gb_nocone)){
    return(TRUE)
  }else{
    identical(gb_cone$areas, gb_nocone$areas)
  }
}

test <- lapply(seq_len(nrow(dt)), function(x){
  print(x)
  compare_areas(dt$site_id[x])
})

unlist(test)
# ALL lakeattributes bathymetrys are simply cone estimates!!!

mapview::mapview(dt)
