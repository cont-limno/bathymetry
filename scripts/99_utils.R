library(googledrive)
library(dplyr)
library(LAGOSNE)
library(sf)
library(assertr)
library(readr)

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
