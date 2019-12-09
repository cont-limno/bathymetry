# ---- load packages ----
library(googledrive)
suppressMessages(library(LAGOSNE))
suppressMessages(library(sf))
library(assertr)
library(readr)
library(nlaR)
library(tidyr)
library(ggplot2)
suppressMessages(library(janitor))
library(stringr)
library(ggforce)
suppressMessages(library(cowplot))
library(cutr) # devtools::install_github("moodymudskipper/cutr")
library(broom)
suppressMessages(library(raster))
library(LAGOSNEgis)
suppressMessages(library(scales))
library(elevatr)
suppressMessages(library(dplyr))
library(progress)

# ---- misc fxn ----
# jsta::get_if_not_exists
get_if_not_exists <- function(x, destfile, read_function = readRDS,
                              ow = FALSE, ...){

  if(is.function(x)){
    if(!file.exists(destfile) | ow){
      res <- x(destfile, ...)
      return(res)
    }else{
      message(paste0("A local evaulation of x already exists on disk"))
      return(read_function(destfile))
    }
  }

  if(!is.function(x)){
    if(!file.exists(destfile) | ow){
      download.file(x, destfile)
    }else{
      message(paste0("A local copy of ", x, " already exists on disk"))
    }
    invisible(x)
  }
}

# ---- assertr -----
greater_than_0 <- function(x){
  if(!is.na(x) & x <= 0){
    return(FALSE)
  }
}
row_redux <- function(df){df[[1]] - df[[2]]}

# ---- convienience functions ----

# jsta::key_state
key_state <- function(x){
  key <- data.frame(state.abb = datasets::state.abb,
                    state.name = datasets::state.name,
                    stringsAsFactors = FALSE)
  dplyr::left_join(x, key,
                   by = c("state.name"))
}

thousand_k <- function(x){
  res <- rep(NA, length(x))
  for(i in 1:length(x)){
    if(x[i] >= 1000 & x[i] < Inf){
      res[i] <- paste0(substring(x[i], 1, 1), ".", substring(x[i], 2, 2), "k")
    }else{
      res[i] <- x[i]
    }
  }
  res
}

# jsta::usa_sf()
usa_sf <- function(crs){
  res <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  state_key <- data.frame(state = datasets::state.abb,
                          ID = tolower(datasets::state.name),
                          stringsAsFactors = FALSE)
  res <- dplyr::left_join(res, state_key, by = "ID")
  dplyr::filter(res, !is.na(.data$state))
}

get_csv   <- function(destfile, drive_name){
  tryCatch(drive_download(drive_name, type = "csv",
                          path = destfile),
           error = function(e){
             print(drive_name)
             write.csv(NA, file = destfile, row.names = FALSE)
             return(NA)
           }
  )
  return(
    read.csv(destfile, stringsAsFactors = FALSE)
  )
}

# convert collection of depth contours/points to a depth raster
poly_to_filled_raster <- function(dt_raw, depth_attr, wh, proj){
  # use state plane for given state
  dt         <- st_transform(dt_raw, proj)
  dt         <- dplyr::select(dt, depth_attr)

  r             <- raster(xmn = st_bbox(dt)[1], ymn = st_bbox(dt)[2],
                          xmx = st_bbox(dt)[3], ymx = st_bbox(dt)[4])
  r[]           <- NA
  r             <- rasterize(as_Spatial(dt), r, field = depth_attr)
  projection(r) <- as.character(st_crs(dt))[2]

  r2 <- r
  r2[which.max(r2[])] <- NaN

  while(
    any(is.nan(extract(r2, concaveman::concaveman(dt))[[1]]))
  ){
    # print(wh)
    w_mat <- matrix(1, wh, wh)
    r2 <- tryCatch(raster::focal(r, w = w_mat,
                        fun = function(x){fill.na(x, width = wh)},
                        pad = TRUE, na.rm = FALSE),
                   error = function(e) r2)
    wh <- wh + 4
  }
  # plot(r2)

  r3 <- mask(r2, concaveman::concaveman(dt))
  # plot(r3)
  list(r = r3, wh = wh)
}

# https://stackoverflow.com/a/45658609/3362993
fill.na <- function(x, width) {
  center = 0.5 + (width * width / 2)
  if( is.na(x)[center] ) {
    return( round(mean(x, na.rm=TRUE),0) )
  } else {
    return( round(x[center],0) )
  }
}

# library(maptools)
# library(spatstat)
# test4 <- as(as_Spatial(dt), "ppp")
# width <- ceiling(res(r)[2] / mean(spatstat::nndist(test4, k=1))) * 5
# if(width %% 2 == 0){width <- width + 1}

# ratio of bounding circle area to poly area
# area_ratio <- st_area(
#   lwgeom::st_minimum_bounding_circle(concaveman::concaveman(dt))) /
#   st_area(concaveman::concaveman(dt))
