source("scripts/99_utils.R")

# get nla files from the cont-limno depth folder and merge with data from nlaR
# drive_download(file = "US_EPA_NLA2012_siteinfo_v1_linked_R",
#                path = "data/00_nla/2012_nla.csv")
# drive_download(file = "US_EPA_NLA2007_siteinfo_v1_linked_R",
#                path = "data/00_nla/2007_nla.csv")

dt_raw_2012 <- read_csv("data/00_nla/2012_nla.csv")
dt_raw_2007 <- read_csv("data/00_nla/2007_nla.csv") #nla 2007 file already has depth

nl2012 <- nla_load(2012)$wide_siteinfo %>%
  dplyr::select(SITE_ID, LAT_DD83, LON_DD83) %>%
  distinct(SITE_ID, .keep_all = TRUE) %>%
  left_join(dplyr::select(nla_load(2012)$wide_profile,
                          SITE_ID, INDEX_SITE_DEPTH)) %>%
  # TODO: pull lake names?
  tidyr::drop_na(INDEX_SITE_DEPTH) %>%
  group_by(SITE_ID) %>%
  slice(which.max(INDEX_SITE_DEPTH)) %>%
  LAGOSNE::coordinatize("LAT_DD83", "LON_DD83")

dt_2012 <- dt_raw_2012 %>%
  distinct(SITE_ID, .keep_all = TRUE) %>%
  left_join(nl2012) %>%
  dplyr::filter(!is.na(INDEX_SITE_DEPTH)) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  janitor::clean_names("snake") %>%
  rename(llid = linked_lagoslakeid)


dt_2007 <- dt_raw_2007 %>%
  distinct(SITE_ID, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(DEPTHMAX)) %>%
  data.frame(stringsAsFactors = FALSE)

# deal with 2007/2012 dups?

# anticipated columns:
# llid, name, legacy_name, state, max_depth_m, mean_depth_m, source, lat, long
