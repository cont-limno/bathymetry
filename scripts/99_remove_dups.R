# create a list of ids in the FL datasets that are NOT in Lakewatch
library(LAGOSNE)
library(dplyr)

lg <- lagosne_load("1.087.1")

ny_llids <- distinct(lg$epi_nutr, lagoslakeid) %>%
  left_join(dplyr::select(lg$locus, lagoslakeid, state_zoneid)) %>%
  left_join(dplyr::select(lg$state, state, state_zoneid)) %>%
  left_join(lg$lakes_limno) %>%
  dplyr::filter(state == "NY" & !is.na(maxdepth)) %>%
  pull(lagoslakeid)

lakewatch_key <- read.csv("/home/jose/Downloads/FL_LAKEWATCH_SITES.csv", stringsAsFactors = FALSE)
lakewatch <- read.csv("/home/jose/Desktop/FL_LAKEWATCH_station_lakedepth - FL_LAKEWATCH_station_lakedepth.csv", stringsAsFactors = FALSE, skip = 1) %>%
  dplyr::left_join(lakewatch_key) %>%
  dplyr::filter(!is.na(Linked_lagoslakeid))

log_raw <- read.csv("data/depth_log_all.csv", stringsAsFactors = FALSE)
dt <- log_raw %>%
  dplyr::filter(state %in% c("FL", "NY")) %>%
  dplyr::filter(Linked_lagoslakeid %in%
                  c(lakewatch$Linked_lagoslakeid, ny_llids)) %>%
  arrange(Linked_lagoslakeid) %>%
  pull(Linked_lagoslakeid)

# sum(dplyr::filter(log_raw, state == "NY")$Linked_lagoslakeid %in% ny_llids)
# length(ny_llids)

clipr::write_clip(
  paste0('=FILTER(A:O, NOT(REGEXMATCH(TO_TEXT(A:A),"',paste0('^',dt, '$', collapse = "|"),'")))')
  )

#### Does LAGOSNE have NH lake depths?
dt <- read.csv("data/depth_log_all.csv", stringsAsFactors = FALSE) %>%
  dplyr::filter(state == "NH")

library(LAGOSNE)

lg <- lagosne_load()
lg2 <- lg$lakes_limno %>%
  dplyr::left_join(lg$locus) %>%
  dplyr::left_join(lg$state) %>%
  dplyr::filter(state == "NH") %>%
  dplyr::filter(!is.na(maxdepth))

dplyr::filter(dt, !(Linked_lagoslakeid %in% lg2$lagoslakeid)) %>%
  arrange(Linked_lagoslakeid) %>%
  pull(Linked_lagoslakeid)







