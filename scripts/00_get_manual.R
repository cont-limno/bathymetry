source("scripts/99_utils.R")
# merge depth_log_all with lakewatch and rm lines with missing lakes

# ---- download_from_drive ----
# unlink("data/00_manual/depth_log_all.csv")
# drive_download(file = "depth_log_all",
#                path = "data/00_manual/depth_log_all.csv", overwrite = TRUE)
dt_raw <- suppressWarnings(suppressMessages(
  read_csv("data/00_manual/depth_log_all.csv", col_types = cols())))

# collected in very early efforts and lack a lagoslakeid
one_off_files <- c("data/00_manual/ar.csv" = "AR_ADEQ_SWQM_sites_lakedepth",
                   "data/00_manual/ca.csv" = "CA_USGS_WSC_sites_lakedepth",
                   "data/00_manual/fl.csv" = "FL_LAKEWATCH_station_lakedepth")
one_off_skips <- c(0, 0, 1)
# lapply(seq_len(length(misc_one_offs)),
#        function(x) drive_download(
#          file = as.character(misc_one_offs[x]),
#          path = names(misc_one_offs)[x],
#          overwrite = TRUE))

one_offs <- lapply(seq_len(length(one_off_files)), function(i){
  # i <- 3
  state <- toupper(gsub(".csv", "", basename(names(one_off_files)[i])))
  message(paste0("dealing with one-off file from ", state))
  res <- read.csv(names(one_off_files)[i], stringsAsFactors = FALSE,
                    header = TRUE, skip = one_off_skips[i]) %>%
    janitor::clean_names() %>%
    mutate(file_name = as.character(one_off_files[i]),
           state = state) %>%
    select(lat = latitude_measure, lon = longitude_measure,
           name = lake_name_google_earth, file_name, state,
           max_depth_ft, mean_depth_ft,
           max_depth_m, mean_depth_m, url = url_with_depth_info,
           comments) %>%
    mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
    dplyr::filter(!is.na(lat) & !is.na(lon))

  # pull lagos polys
  lg_poly <- LAGOSUSgis::query_gis_(query = paste0("SELECT * FROM LAGOS_US_All_Lakes_1ha WHERE lake_centroidstate LIKE '", state, "' AND lake_totalarea_ha > 4"))
  lg_poly <- lwgeom::st_make_valid(lg_poly)

  res_final <- res %>%
    coordinatize(latname = "lat", longname = "lon") %>%
    st_transform(crs = st_crs(lg_poly)) %>%
    st_join(lg_poly) %>%
    dplyr::filter(!is.na(lagoslakeid)) %>%
    st_drop_geometry() %>%
    dplyr::distinct(lagoslakeid, max_depth_m, max_depth_ft, .keep_all = TRUE) %>%
    dplyr::select(c("lagoslakeid", names(res))) %>%
    dplyr::rename(Linked_lagoslakeid = lagoslakeid)

  res_final
  })

dt_raw <- dplyr::bind_rows(dt_raw, dplyr::bind_rows(one_offs))

# ---- convert_merge_ft_to_m ----
raw <- convert_ft_m(dt_raw)

# ---- assign_GLNC_to_states ----
raw <- raw %>%
  coordinatize("lat", "lon") %>%
  mutate(has_max = !is.na(max_depth_m) & nchar(max_depth_m) > 0) %>%
  st_join(st_transform(jsta::usa_sf(), 4326)) %>%
  mutate(state.x = case_when(
    state.x == "Western States GLNC" & !is.na(state.y) ~ state.y,
    TRUE ~ state.x)) %>%
  dplyr::select(-state.y) %>% dplyr::rename(state = state.x) %>%
  dplyr::filter(state != "Western States GLNC") %>%
  # ## state codes are legit
  # assert(raw, function(x) x %in% state.abb, state)
  data.frame(stringsAsFactors = FALSE) %>%
  janitor::clean_names("snake")

# ---- join_lake_area ----
locus <- lagosus_load("locus")$locus$locus_characteristics
raw <- left_join(raw, dplyr::select(locus, lagoslakeid, lake_waterarea_ha,
                                    lake_connectivity_permanent),
                 by = c("linked_lagoslakeid" = "lagoslakeid"))

# ---- export ----
res <- raw[,!duplicated(names(raw))] %>%
  rename(llid = linked_lagoslakeid) %>%
  mutate(legacy_name = NA) %>%
  mutate(effort = "manual") %>%
  rename(source = url) %>%
  select(llid, name, legacy_name, state,
         max_depth_m, mean_depth_m, source,
         lake_waterarea_ha, lake_connectivity_permanent, effort,
         lat, long = lon)# %>%
# dplyr::filter(!is.na(max_depth_m) | !is.na(mean_depth_m))

res <- rm_dups(res)

write.csv(res, "data/00_manual/00_manual.csv", row.names = FALSE)
# res <- read.csv("data/00_manual/00_manual.csv", stringsAsFactors = FALSE)

# ---- graph_checks ----
if(interactive()){
# histograms by state
ggplot(data = res, aes(x = max_depth_m)) +
  geom_histogram() +
  facet_wrap_paginate(~state, scales = "free", page = 2, ncol = 3, nrow = 4)

# missing data by state
  res %>%
    group_by(state) %>%
    mutate(prop_maxdepth = round(mean(!is.na(max_depth_m)), 2)) %>%
    add_tally() %>%
    distinct(state, prop_maxdepth, n)  %>%
    arrange(prop_maxdepth) %>%
    data.frame()

# outlier checks
dplyr::filter(raw, max_depth_m < 1) %>% View()
raw[which.min(raw$max_depth_m),]

# join with locus preview
ll_locus <- lagosus_load("locus")$locus$locus_characteristics
test <- left_join(dplyr::select(raw, -lake_waterarea_ha), ll_locus,
                  by = c("linked_lagoslakeid" = "lagoslakeid"))

# labelled histogram of max depth availability by area class
library(cutr) # devtools::install_github("moodymudskipper/cutr")

test2 <- test %>%
  mutate(area_class =
           smart_cut(test$lake_waterarea_ha, c(1, 4, 40, 80, 400,
                                               1000, 20000, Inf),
             labels = ~paste(sep="-", thousand_k(.y[1]), thousand_k(.y[2])))
         ) %>%
  drop_na(area_class) %>%
  group_by(area_class) %>%
  mutate(prop_maxdepth = round(mean(!is.na(max_depth_m)), 2)) %>%
  add_tally() %>%
  distinct(area_class, prop_maxdepth, n) %>%
  arrange(area_class)

test2 %>%
  ggplot() +
  geom_col(aes(y = n, x = area_class)) +
  geom_text(aes(y = n, x = area_class, label = prop_maxdepth),
            vjust = -0.5, size = 4) +
  ylim(0, 4000) + xlab("Area (ha)") +
  ggtitle("Proportion max depth availability by lake area class")

# what lakes' data are coming from navionics?

test2 <- test %>%
  drop_na(max_depth_m) %>%
  mutate(is_navionics = case_when(
    as.logical(!is.na(str_match(tolower(url), "navionics"))) ~ TRUE,
    TRUE ~ FALSE)) %>%
  sf::st_sf()

sum(test2$is_navionics) / nrow(test2) * 100 # roughly 20 percent

ggplot() +
  geom_sf(data = test2, aes(color = is_navionics)) # mostly texas, florida, and sd

# map of missing/not-missing mean depth
ggplot() +
  geom_sf(data = test2, aes(color = !is.na(mean_depth_m)))

# outlier qa
ggplot() +
  geom_point(data = test, aes(x = lake_waterarea_ha, y = max_depth_m)) +
  xlim(3, 400) +
  ylim(0, 150)

ggplot() +
  geom_point(data = test, aes(x = mean_depth_m, y = max_depth_m)) +
  ylim(0, 150)

dplyr::filter(test, lake_waterarea_ha > 300 & max_depth_m < 2)

dplyr::filter(test, max_depth_m > 250)
dplyr::filter(test, lake_waterarea_ha > 200000)

plot(test$lake_waterarea_ha, test$max_depth_m,
     xlim = c(0, 20000),
     ylim = c(0, 150))

}

# ---- check common data sources ----
if(interactive()){
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

}
