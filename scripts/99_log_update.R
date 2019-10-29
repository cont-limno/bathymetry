source("scripts/99_utils.R")

# ---- read-in-todo-files ----
dt         <- read.csv("data/lagosus_depth.csv", stringsAsFactors = FALSE)
flist_todo <- drive_ls("Depth_TODO")

out_names <- paste0("data/99_depth_collection/", flist_todo$name)

depth_files <- lapply(seq_len(length(out_names)), function(x){
  get_if_not_exists(get_csv, out_names[x], read.csv,
                    drive_name = flist_todo$name[x])
}) %>%
  setNames(flist_todo$name)

# ---- id-unique-llids ----

has_llid    <- unlist(lapply(depth_files,
                             function(x) "Linked_lagoslakeid" %in% names(x)))
# View(cbind(names(depth_files), depth_log$file_name, has_llid))
depth_files <- depth_files[has_llid]

# remove duplicate llids among files
llids <- lapply(depth_files, function(x){
  dplyr::select(x, Linked_lagoslakeid)
})
llids <- llids  %>%
  unlist(recursive = FALSE) %>%
  tibble::enframe() %>%
  unnest()
llids <- llids[!duplicated(llids$value),]
llids <- llids %>%
  mutate(file_name = gsub(".Linked_lagoslakeid", "", name)) %>%
  rename(llid = value) %>%
  dplyr::filter(!is.na(llid))
llids <- llids[!(llids$llid %in% dt$llid),]

# ---- clean-input-files-for-log ----

depth_log_all <- lapply(depth_files,
                        function(x) dplyr::select(x,
          matches("^lat|_lat_|y_coord|latitude$|latdd$|^latitude|ylat"),
          matches("^lon|_lon_|x_coord|longitude$|longdd$|^longitude|xlon"),
          matches("waterbodyname|commonname|county_name|waterbody_name|station_name"),
          "Linked_lagoslakeid")) %>%
  lapply(function(x) mutate_all(x, as.character)) %>%
  lapply(function(x) setNames(x, tolower(names(x))))

lat_col  <- lapply(depth_files, function(x)
  names(dplyr::select(x,
  matches("^lat|_lat_|y_coord|latitude$|latdd$|^latitude|ylat|lat_ddmmss"))))
lat_col <- unlist(lapply(lat_col, function(x) x[length(x)]))
lon_col  <- lapply(depth_files,
              function(x) names(dplyr::select(x,
              matches("^longitude|^lon|_lon_|x_coord|longitude$|longdd$|xlon|lon_ddmmss"))))
lon_col <- unlist(lapply(lon_col, function(x) x[length(x)]))
name_col <- lapply(depth_files, function(x) names(dplyr::select(x,
                                                                matches("station_alt_name|waterbodyname|commonname|waterbody_name|station_name|sta_desc|monitoringlocationname|loc_name|site_name|^site$|long_description|stationname|lakes4|monitoring_location_name|streamname_facilityname|station_description|water_name|locale_name"))))
name_col <- unlist(lapply(name_col, function(x) x[length(x)]))

depth_log_all <- lapply(seq_len(length(depth_files)),
                        function(x) dplyr::select(depth_files[[x]],
                            lat = lat_col[x],
                            lon = lon_col[x],
                            name = matches(paste0("^", name_col[x], "$")),
                            Linked_lagoslakeid))
depth_log_all <- lapply(depth_log_all, function(x) mutate_all(x, as.character))

res <- dplyr::bind_rows(depth_log_all) %>%
  left_join(
    dplyr::select(mutate(llids, llid = as.character(llid)), llid, file_name),
            by = c("Linked_lagoslakeid" = "llid")) %>%
  dplyr::filter(!is.na(file_name)) %>%
  dplyr::filter(!duplicated(Linked_lagoslakeid)) %>%
  mutate(max_depth_ft = NA, mean_depth_ft = NA, lakename_googleearth = NA,
         max_depth_m = NA, mean_depth_m = NA, url = NA, comments = NA) %>%
  mutate(rowid = NA, assigned_to = NA, state = NA)

res <- dplyr::select(res, Linked_lagoslakeid, rowid, lat, lon,
                      name, file_name, state, assigned_to, max_depth_ft:comments)

write.csv(res, "data/depth_todo.csv", row.names = FALSE)
