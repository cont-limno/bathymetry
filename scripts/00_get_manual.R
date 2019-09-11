library(googledrive)

# merge depth_log_all with lakewatch and rm lines with missing lakes

if(!file.exists("data/manual/depth_log_all.csv")){
  drive_download("depth_log_all", type = "csv",
                 path = "data/manual/depth_log_all.csv", overwrite = TRUE)
}

dt_raw <- read.csv("data/manual/depth_log_all.csv", stringsAsFactors = FALSE)
