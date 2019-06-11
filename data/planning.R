# collapse log file by state
#
# 1. take files with a lagoslakeid field
# 2. check if fields contain depth from the provider
# 2. bind rows
# 3. select lat, lon, name, and llid columns

library(assertr)
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
  dplyr::filter(state != "Tribal") %>%
  dplyr::mutate(number_of_linked_lake_sites =
                  as.numeric(number_of_linked_lake_sites)) %>%
  dplyr::filter(!is.na(number_of_linked_lake_sites)) %>%
  rowwise() %>%
  mutate(file_name = strsplit(dataset_path, "\\\\|/")[[1]][2]) %>%
  dplyr::select(state, program_name,
                number_of_linked_lake_sites, file_name)

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
# View(cbind(names(depth_files), depth_log$file_name, has_llid))
depth_log   <- depth_log[has_llid,]
depth_files <- depth_files[has_llid]

# remove duplicate llids among files
llids <- lapply(depth_files, function(x){
  dplyr::select(x, Linked_lagoslakeid)
  })
llids <- llids  %>%
  unlist(recursive = FALSE) %>%
  tibble::enframe() %>%
  unnest()

# remove llids in NLA (they all have depths according to Katelyn)
nla <- dplyr::filter(llids, stringr::str_detect(name, "US_EPA"))
llids <- llids[!(llids$value %in% nla$value),]

llids <- llids[!duplicated(llids$value),]
llids <- llids %>%
  mutate(file_name = gsub(".Linked_lagoslakeid", "", name)) %>%
  rename(llid = value) %>%
  group_by(file_name) %>%
  count(name = "n_llids")
depth_log <- left_join(depth_log, llids, by = "file_name") %>%
  dplyr::filter(!is.na(n_llids))
depth_log <- depth_log %>%
  verify(n_llids <= number_of_linked_lake_sites)

res <- depth_log %>%
  group_by(state) %>%
  summarize(total_llids = sum(n_llids)) %>%
  arrange(total_llids) %>%
  mutate(cusum = cumsum(total_llids))

people <-data.frame(stringsAsFactors=FALSE,
people = c("Lauren (1)", "Jessica (1)", "Joe (1)", "Ian (1-2)", "Pat (1-2)",
                  "Katelyn (1-2)", "Kendra (1-2)", "Allie (~5 or more)",
                  "Lindsie - (10-15)", "Sam (10-15)"),
         num_states = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 5L, 10L, 10L),
         num_lakes = c(247.7777778, 247.7777778, 247.7777778, 495.5555556,
                         495.5555556, 495.5555556, 495.5555556, 1238.888889,
                         2477.777778, 2477.777778),
         cusum = c(247.7777778, 495.5555556, 743.3333333, 1238.888889, 1734.444444,
                     2230, 2725.555556, 3964.444444, 6442.222222, 8920),
         states = c("MI, ID, TN, AL, OR, DE, LA, CA", "NV, VA, AZ, WV, GA", "SC, KY, CT", "WY, NM, MS, SD", "NE, OK, NC, MT", "UT, ND", "WA, KS", "CO, NY", "NH, TX, FL", "GLNC")
         )

jsta::pdf_table(knitr::kable(people), out_name = "assignments.pdf")
jsta::pdf_table(knitr::kable(dplyr::select(depth_log, state, n_llids, file_name,
                                           -number_of_linked_lake_sites,
                                           -program_name)), out_name = "log.pdf")




