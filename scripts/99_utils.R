# ---- load packages ----
library(googledrive)
suppressMessages(library(dplyr))
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

