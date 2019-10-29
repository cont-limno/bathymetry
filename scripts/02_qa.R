source("scripts/99_utils.R")

# TODO: make sure max depth is at least equal to the maximum sample depth from limno

dt <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE)

dt_bad <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE) %>%
  dplyr::filter(max_depth_m == mean_depth_m) %>%
  mutate(test = urltools::domain(source))

table(dt_bad$test)
jsta::pdf_table(knitr::kable(table(dt_bad$test)))
jsta::pdf_table(knitr::kable(t(table(dt_bad$state))))

lg_873 <- dt_bad[,c("llid", "max_depth_m", "mean_depth_m")] %>%
  rename(lagoslakeid = llid,
         max_depth_873 = max_depth_m,
         mean_depth_873 = mean_depth_m)

# lg_871_raw <- lagosne_load("1.087.1")
lg_871 <- lg_871_raw$lakes_limno %>%
  dplyr::filter(lagoslakeid %in% lg_873$lagoslakeid) %>%
  dplyr::select(lagoslakeid, maxdepth, meandepth) %>%
  rename(max_depth_871 = maxdepth,
         mean_depth_871 = meandepth)

res <- left_join(lg_873, lg_871)

jsta::pdf_table(knitr::kable(res))
jsta::pdf_table(knitr::kable(dplyr::filter(res, !is.na(max_depth_871))))
