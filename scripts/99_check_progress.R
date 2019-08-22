library(readxl)
library(dplyr)
library(googledrive)
library(assertr)
# drive_find(type = "spreadsheet")
if(!file.exists("data/backups/depth_log_all.csv")){
  drive_download("depth_log_all", type = "csv",
                 path = "data/backups/depth_log_all.csv", overwrite = TRUE)
}

dt_raw <- read.csv("data/backups/depth_log_all.csv", stringsAsFactors = FALSE)

# convert ft to m
dt <- dt_raw %>%
  mutate_at(vars(contains("depth")), as.numeric) %>%
  mutate(max_depth_m = case_when(
    is.na(max_depth_m) & !is.na(max_depth_ft) ~ max_depth_ft * 0.3048,
    TRUE ~ max_depth_m
  )) %>%
  mutate(mean_depth_m = case_when(
    is.na(mean_depth_m) & !is.na(mean_depth_ft) ~ mean_depth_ft * 0.3048,
    TRUE ~ mean_depth_m
  )) %>%
  dplyr::select(-mean_depth_ft, -max_depth_ft)

# TODO: assertr the arg below
# which(dt$mean_depth_m == dt$max_depth_m)

# How many states have been touched?
# Of those states, how what is the response rate for max/mean depth?

frac <- dt %>%
  group_by(state) %>%
  summarize_at(vars(max_depth_m:mean_depth_m),
               function(x) round(sum(!is.na(x)) / length(x), 2)) %>%
  data.frame() %>% setNames(c("state", "max_frac", "mean_frac"))

n <- dt %>%
  group_by(state) %>%
  summarize_at(vars(max_depth_m:mean_depth_m),
               function(x) sum(!is.na(x))) %>%
  data.frame() %>% setNames(c("state", "max_n", "mean_n"))

res <- left_join(frac, n, by = "state") %>%
  arrange(desc(max_frac))

jsta::pdf_table(knitr::kable(res), "frac_sorted.pdf")
jsta::pdf_table(knitr::kable(arrange(res, desc(max_n))), "n_sorted.pdf")


# calculate total number of lakes we're working with here
lg <- LAGOSNE::lagosne_load()
sum(res$max_n) + sum(!is.na(lg$lakes_limno$maxdepth))
