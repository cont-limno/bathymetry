library(dplyr)
library(googledrive)
# drive_find(type = "spreadsheet")
if(!file.exists("data/backups/depth_log_all.csv")){
  drive_download("depth_log_all", type = "csv",
                 path = "data/backups/depth_log_all.csv", overwrite = TRUE)
}

dt <- read.csv("data/backups/depth_log_all.csv", stringsAsFactors = FALSE) %>%
  rename(people = Assigned.to.)

# unique(dt$people)
# dplyr::filter(dt, people == "")

# set apportionment goal
people <- data.frame(stringsAsFactors = FALSE,
                    people = c("Lauren", "Jessica", "Joe",
                               "Ian", "Katelyn", "Jake",
                               "Arika", "Allie", "Lindsie",
                               "Sam"),
                    num_states = c(1L, 1L, 1L,
                                   2L, 2L, 2L,
                                   1L, 3L, 10L,
                                   9L)) %>%
  mutate(target_frac = num_states / sum(num_states))

res <- dt %>%
  group_by(people) %>%
  tally() %>% ungroup() %>%
  mutate(current_frac = n / sum(n)) %>%
  left_join(people) %>%
  mutate(n_target = target_frac * sum(n)) %>%
  mutate(n_diff = n_target - n) %>%
  arrange(desc(n_diff)) # %>%
  # dplyr::filter(n_diff > 0)

# Which people have the most untouched lakes?
touched <- dt %>%
  group_by(state) %>%
  # dplyr::filter(state == "AL") %>%
  summarize_at(vars(max_depth_ft:comments),
               function(x) sum(is.na(x)) / length(x)) %>%
  data.frame() %>% # dt$state == unique(dt_raw$state)
  dplyr::select(-state) %>%
  apply(1, function(x) min(x[x > 0])) %>%
  as.numeric() != 1
data.frame(touched = !touched, state = unique(dt$state),
           stringsAsFactors = FALSE) %>%
  arrange(touched) %>%
  left_join(distinct(dplyr::select(dt, people, state), people, state, .keep_all = TRUE)) %>%
  arrange(people) %>%
  View()

