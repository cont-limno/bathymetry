source("scripts/99_utils.R")
# merge depth_log_all with lakewatch and rm lines with missing lakes


# ---- download_from_drive ----
# unlink("data/00_manual/depth_log_all.csv")
# drive_download(file = "depth_log_all",
#                path = "data/00_manual/depth_log_all.csv", overwrite = TRUE)
dt_raw <- read_csv("data/00_manual/depth_log_all.csv")

dt_lw <- get_if_not_exists(x = drive_download,
                            destfile = "data/00_manual/lw.csv",
                            read_function = read_csv,
                           file = "FL_LAKEWATCH_sites_linked_R",
                            type = "csv", path = "data/00_manual/lw.csv",
                            overwrite = FALSE)

# ---- convert_merge_ft_to_m ----

raw <- dt_raw %>%
  mutate_at(vars(contains("depth")), as.numeric) %>%
  # ## the ft value should always be greater than the m value if both are present
  assert_rows(row_redux, greater_than_0, c(max_depth_ft, max_depth_m)) %>%
  assert_rows(row_redux, greater_than_0, c(mean_depth_ft, mean_depth_m)) %>%
  mutate(max_depth_m = case_when(
    is.na(max_depth_m) & !is.na(max_depth_ft) ~ max_depth_ft * 0.3048,
    TRUE ~ max_depth_m
  )) %>%
  mutate(mean_depth_m = case_when(
    is.na(mean_depth_m) & !is.na(mean_depth_ft) ~ mean_depth_ft * 0.3048,
    TRUE ~ mean_depth_m
  )) %>%
  dplyr::select(-mean_depth_ft, -max_depth_ft)

# ---- assign_GLNC_to_states ----
raw <- raw %>%
  coordinatize("lat", "lon") %>%
  mutate(has_max = !is.na(max_depth_m) & nchar(max_depth_m) > 0) %>%
  st_join(jsta::usa_sf()) %>%
  mutate(state.x = case_when(
    state.x == "Western States GLNC" & !is.na(state.y) ~ state.y,
    TRUE ~ state.x)) %>%
  dplyr::select(-state.y) %>% dplyr::rename(state = state.x) %>%
  dplyr::filter(state != "Western States GLNC") %>%
  data.frame(stringsAsFactors = FALSE) %>%
  janitor::clean_names("snake")

# ---- qa ----
# ## max_depth_m > mean_depth_m
assert_rows(raw, row_redux, greater_than_0, c(max_depth_m, mean_depth_m))

# ## state codes are legit
# assert(codes are legit)
unique(raw$state)
table(table(raw$state))


# ---- graph_checks ----

# histograms by state
ggplot(data = raw, aes(x = max_depth_m)) +
  geom_histogram() +
  facet_wrap_paginate(~state, scales = "free", page = 2, ncol = 3, nrow = 4)

# outlier checks
dplyr::filter(raw, max_depth_m < 1)
raw[which.min(raw$max_depth_m),]

# join with locus preview
ll_locus <- read.csv("data/00_lagosus_locus/lake_characteristics.csv", stringsAsFactors = FALSE)
test <- left_join(raw, ll_locus, by = c("linked_lagoslakeid" = "lagoslakeid"))

ggplot() +
  geom_point(data = test, aes(x = lake_waterarea_ha, y = max_depth_m)) +
  xlim(0, 20000) +
  ylim(0, 150)

dplyr::filter(test, lake_waterarea_ha <= 20 & max_depth_m > 50)

dplyr::filter(test, max_depth_m > 250)
dplyr::filter(test, lake_waterarea_ha > 200000)

plot(test$lake_waterarea_ha, test$max_depth_m,
     xlim = c(0, 20000),
     ylim = c(0, 150))

# compare against lagosne
lg <- lagosne_load("1.087.3")

# ---- check common data sources ----
test <- data.frame(url = urltools::domain(raw$url),
           url_raw = raw$url,
           stringsAsFactors = FALSE) %>%
  dplyr::filter(!is.na(url)) %>%
  dplyr::filter(url != "none found") %>%
  dplyr::filter(url != "no depth") %>%
  dplyr::filter(url != "no depth found") %>%
  group_by(url_raw) %>%
  dplyr::tally() %>%
  arrange(desc(n)) %>%
  data.frame()
head(test)

dplyr::filter(raw, stringr::str_detect(url, "gf.nd.gov"))
dplyr::filter(raw, stringr::str_detect(url, ".kdheks.gov"))

# ---- export ----

