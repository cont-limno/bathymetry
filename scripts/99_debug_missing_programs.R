library(dplyr)

programs <- read.csv("data/00_manual/depth_log_all.csv", stringsAsFactors = FALSE)
programs_raw <- programs %>%  distinct(file_name) %>% pull(file_name)
flist_raw    <- list.files("data/99_depth_collection/", pattern = "*.csv",
                    include.dirs = TRUE, full.names = TRUE)

state <- "MT_"

flist <- flist_raw[str_detect(flist_raw, state)]
programs_raw[str_detect(programs_raw, state)]

dt <- lapply(flist, read.csv, stringsAsFactors = FALSE)
names(dt) <- basename(flist)
unlist(lapply(dt, function(x)  any(names(x) %in% "Linked_lagoslakeid")))

used <- dplyr::rbind_all(
  dt[names(dt) %in%
  paste0(programs_raw[str_detect(programs_raw, state)], ".csv")
  ])$Linked_lagoslakeid

dt[[1]]$Linked_lagoslakeid %in% used
dt[[2]]$Linked_lagoslakeid %in% used
dt[[3]]$Linked_lagoslakeid %in% used
dt[[4]]$Linked_lagoslakeid %in% used
dt[[5]]$Linked_lagoslakeid %in% used

