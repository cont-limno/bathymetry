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
library(janitor)
library(stringr)

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
