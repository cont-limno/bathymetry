library(readxl)
library(dplyr)
library(googledrive)
# drive_find(type = "spreadsheet")
if(!file.exists("data/backups/depth_log_all.csv")){
  drive_download("depth_log_all", type = "csv",
                 path = "data/backups/depth_log_all.csv", overwrite = TRUE)
}

dt_raw <- read.csv("data/backups/depth_log_all.csv", stringsAsFactors = FALSE)

# How many states have been touched?
# Of those states, how what is the response rate for max/mean depth?

dt <- dt_raw %>%
  group_by(state) %>%
  # dplyr::filter(state == "AL") %>%
  summarize_at(vars(max_depth_ft:comments),
               function(x) sum(is.na(x)) / length(x)) %>%
  data.frame() %>% # dt$state == unique(dt_raw$state)
  dplyr::select(-state) %>%
  apply(1, function(x) min(x[x > 0])) %>%
  as.numeric()

dt <- data.frame(name = unique(dt_raw$state),
                   frac = dt, stringsAsFactors = FALSE) %>%
  mutate(frac = 1 - frac) %>%
  arrange(desc(frac))

missing_data <- dt[dt$frac == 0,"name"]

dt <- dplyr::filter(dt, frac > 0) %>%
  add_row(name =
            paste(missing_data, collapse = ","),
          frac = 0)

jsta::pdf_table(dt)

26/34

dt_raw %>%
  summarize_at(vars(max_depth_ft:comments),
               function(x) sum(is.na(x)) / length(x))

  sum(!is.na(dt_raw$max_depth_ft))

