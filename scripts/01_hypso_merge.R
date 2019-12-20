source("scripts/99_utils.R")

hypso_ct <- read.csv("data/ct_hypso.csv", stringsAsFactors = FALSE) %>%
  mutate(state = "CT")
hypso_mn <- read.csv("data/mn_hypso.csv", stringsAsFactors = FALSE) %>%
  mutate(state = "MN")
hypso_mi <- read.csv("data/mi_hypso.csv", stringsAsFactors = FALSE) %>%
  mutate(state = "MI")
hypso_nh <- read.csv("data/nh_hypso.csv", stringsAsFactors = FALSE) %>%
  mutate(state = "NH")
hypso_ks <- read.csv("data/ks_hypso.csv", stringsAsFactors = FALSE) %>%
  mutate(state = "KS")

# merge csv's and save
res <- dplyr::bind_rows(hypso_mn, hypso_ct, hypso_mi, hypso_nh, hypso_ks)
write.csv(res, "data/00_hypso/hypso.csv", row.names = FALSE)
# res <- read.csv("data/00_hypso/hypso.csv", stringsAsFactors = FALSE)

if(interactive()){
  # check max depth against lagosne estimates
  dt_raw_ne <- read.csv("data/lagosne_depth_predictors.csv",
                        stringsAsFactors = FALSE)
  lg_ne     <- lagosne_load()

  test <- res %>%
    ungroup() %>% distinct(llid, maxdepth) %>%
    mutate(llid = as.numeric(llid)) %>%
    rename(maxdepth_hypso = maxdepth) %>%
    dplyr::left_join(dplyr::select(dt_raw_ne,
                        llid, lagosne_lagoslakeid, lake_centroidstate)) %>%
    drop_na() %>%
    left_join(dplyr::select(lg_ne$lakes_limno,
                            lagoslakeid, maxdepth, lagosname1),
              by = c("lagosne_lagoslakeid" = "lagoslakeid"))

  (gg <- ggplot(data = test) +
    geom_point(aes(x = maxdepth, y = maxdepth_hypso,
                   color = lake_centroidstate)) +
    geom_abline(aes(slope = 1, intercept = 0)))

  # plotly::ggplotly(gg)
  bad_llids <- c(70357, 9914, 83617)
  View(dplyr::filter(test, lagosne_lagoslakeid %in% bad_llids))
  mapview::mapview(query_gis("LAGOS_NE_All_Lakes_4ha", "lagoslakeid", 83617)) +
  mapview::mapview(raster("data/mn_bathy/83617.tif"))

}


