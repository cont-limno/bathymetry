library(readxl)
library(dplyr)

dt_raw <- readxl::read_excel("depth_log_all.xlsx")

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

