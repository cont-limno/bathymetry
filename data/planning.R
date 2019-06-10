# collapse log file by state
#
# 1. take files with a lagoslakeid field
# 2. check if fields contain depth from the provider
# 2. bind rows
# 3. select lat, lon, name, and llid columns

library(dplyr)
library(tidyr)
library(janitor)
library(googledrive)
# drive_find(type = "spreadsheet")
if(!file.exists("data/depth_log.csv")){
  drive_download("LakeDepthLog_for_Reference", type = "csv",
                 path = "data/depth_log.csv")
}

# jsta::get_if_not_exists
get_if_not_exists <- function(x, destfile, read_function = readRDS,
         overwrite = FALSE, ...){

  if(is.function(x)){
    if(!file.exists(destfile) | overwrite){
      res <- x(destfile, ...)
      return(res)
    }else{
      message(paste0("A local evaulation of x already exists on disk"))
      return(read_function(destfile))
    }
  }

  if(!is.function(x)){
    if(!file.exists(destfile) | overwrite){
      download.file(x, destfile)
    }else{
      message(paste0("A local copy of ", x, " already exists on disk"))
    }
    invisible(x)
  }
}

depth_log <- read.csv("data/depth_log.csv", stringsAsFactors = FALSE, skip = 3) %>%
  janitor::clean_names() %>%
  dplyr::filter(nchar(dataset_assigned_to) <= 0) %>%
  dplyr::filter(notes != "DEPTHS IN LAGOS-NE") %>%
  rowwise() %>%
  mutate(file_name = strsplit(dataset_path, "\\\\|/")[[1]][2])

out_names <- paste0("data/", depth_log$file_name, ".csv")
get_csv   <- function(destfile, drive_name){
  tryCatch(drive_download(paste0(drive_name, "_R"), type = "csv",
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
depth_files <- lapply(seq_len(nrow(depth_log)), function(x){
  get_if_not_exists(get_csv, out_names[x], read.csv,
                    drive_name = depth_log$file_name[x])
  }) %>%
  setNames(depth_log$file_name)

has_llid    <- unlist(lapply(depth_files,
                          function(x) "Linked_lagoslakeid" %in% names(x)))
depth_log   <- depth_log[has_llid,]
depth_files <- depth_files[has_llid]
depth_log$n_llids <- unlist(lapply(depth_files, function(x)
  length(unique(x$Linked_lagoslakeid))))

res <- dplyr::select(depth_log, state, program_name,
                           number_of_linked_lake_sites, n_llids) %>%
  dplyr::filter(state != "Tribal") %>%
  dplyr::filter(nchar(state) == 2) %>%
  dplyr::mutate(number_of_linked_lake_sites =
                  as.numeric(number_of_linked_lake_sites)) %>%
  dplyr::filter(!is.na(number_of_linked_lake_sites))



