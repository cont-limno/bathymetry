source("scripts/99_utils.R")

lg_ne <- lagosne_load("1.087.3")
lg    <- lagosus_load(modules = "locus")

# get list of lagosne ids that:
#   1) drop any lakes with a one-to-many lagosne to lagosus link
res <- lg$locus$locus_link %>%
  distinct(lagoslakeid, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(lagosne_lagoslakeid)) %>%
  group_by(lagosne_lagoslakeid) %>%
  add_tally() %>% arrange(desc(n)) %>%
  dplyr::filter(n == 1) %>%
  ungroup() %>% distinct(lagosne_lagoslakeid, lagoslakeid) %>%
  #   2) drop any lakes with a > 10% area change
  left_join(dplyr::select(lg_ne$locus,
    lagosne_lagoslakeid = lagoslakeid, lake_area_ha)) %>%
  left_join(dplyr::select(lg$locus$locus_characteristics,
    lagoslakeid, lake_waterarea_ha)) %>%
  left_join(dplyr::select(lg$locus$locus_information,
    lagoslakeid, lake_centroidstate)) %>%
  mutate(diff = abs(lake_area_ha - lake_waterarea_ha) / lake_waterarea_ha) %>%
  arrange(desc(diff)) %>%
  dplyr::filter(diff <= 0.1) %>%
  dplyr::select(-diff)

write.csv(res, "data/00_lagosne/00_lagosne_xwalk.csv",
  row.names = FALSE)
# res <- read.csv("data/00_lagosne/00_lagosne_xwalk.csv", stringsAsFactors = FALSE)

# plot(res$lake_waterarea_ha, res$lake_area_ha)

# are there lakes that were combined (instead of split)?
# test <- ungroup(res) %>% group_by(lagoslakeid) %>%
#   add_tally() %>% arrange(desc(n))

# are there lakes with a 1 to 1 relationship that have a different id?
# test <- lg$locus$locus_link %>%
#   distinct(lagoslakeid, .keep_all = TRUE) %>%
#   dplyr::filter(!is.na(lagosne_lagoslakeid)) %>%
#   group_by(lagosne_lagoslakeid) %>%
#   add_tally() %>% arrange(desc(n)) %>%
#   dplyr::filter(n == 1)
#
# which(test$lagoslakeid != test$lagosne_lagoslakeid)