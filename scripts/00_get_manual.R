source("scripts/99_utils.R")
# merge depth_log_all with lakewatch and rm lines with missing lakes


# ---- download_from_drive ----
dt_raw <- get_if_not_exists(x = drive_download,
                            destfile = "data/00_manual/depth_log_all.csv",
                            read_function = read_csv, file = "depth_log_all",
                            type = "csv", path = "data/00_manual/depth_log_all.csv",
                            overwrite = FALSE, verbose = TRUE)

dt_lw <- get_if_not_exists(x = drive_download,
                            destfile = "data/00_manual/lw.csv",
                            read_function = read_csv,
                           file = "FL_LAKEWATCH_sites_linked_R",
                            type = "csv", path = "data/00_manual/lw.csv",
                            overwrite = FALSE)

# ---- convert_merge_ft_to_m ----
raw <- dt_raw %>%
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

# ---- assign_GLNC_to_states ----
raw <- raw %>%
  coordinatize("lat", "lon") %>%
  mutate(has_max = !is.na(max_depth_m) & nchar(max_depth_m) > 0) %>%
  st_join(jsta::usa_sf()) %>%
  mutate(state.x = case_when(
    state.x == "Western States GLNC" & !is.na(state.y) ~ state.y,
    TRUE ~ state.x)) %>%
  dplyr::select(-state.y) %>% dplyr::rename(state = state.x) %>%
  dplyr::filter(state != "Western States GLNC")

# ---- assertr_raw ----
# TODO: assertr the args below
# * dt$mean_depth_m <= dt$max_depth_m
# * the ft value should always be greater than the m value if both are present


