# ---- load packages ----
library(tidyselect) # >= 0.2.99.9000
library(googledrive)
suppressMessages(library(LAGOSNE))
suppressMessages(library(sf))
sf::sf_use_s2(FALSE)
library(assertr)
library(readr)
library(nlaR)
library(tidyr)
library(ggplot2)
suppressMessages(library(janitor))
library(stringr)
library(ggforce)
suppressMessages(library(cowplot))
library(cutr) # devtools::install_github("moodymudskipper/cutr")
library(broom)
suppressMessages(library(raster))
library(LAGOSNEgis) # install_github("cont-limno/LAGOSNEgis")
suppressMessages(library(scales))
library(elevatr)
suppressMessages(library(dplyr))
library(progress)
suppressMessages(library(LAGOSUS)) # install_github("cont-limno/LAGOSUS")
Sys.setenv(LAGOSUS_DEPTH_VER = 0.1) # set module versions
library(polylabelr)
library(esri2sf) # install_github("yonghah/esri2sf")
library(rnaturalearth)
suppressMessages(library(mapview))
suppressMessages(library(ggrastr)) # install_github('VPetukhov/ggrastr')
suppressMessages(library(lwgeom))
suppressMessages(library(rmapshaper))
suppressMessages(library(kableExtra))
suppressMessages(library(LAGOSUSgis)) # install_github("cont-limno/LAGOSUSgis")
suppressMessages(library(rgdal))
library(rnaturalearthhires) # install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
suppressMessages(library(GGally))
library(ranger)
suppressMessages(library(yardstick))
suppressMessages(library(recipes))
library(rsample)
library(parsnip)

# ---- misc fxn ----
# jsta::get_if_not_exists
get_if_not_exists <- function(x, destfile, read_function = readRDS,
                              ow = FALSE, ...) {

  if (is.function(x)) {
    if (!file.exists(destfile) | ow) {
      res <- x(destfile, ...)
      return(res)
    } else {
      message(paste0("A local evaulation of x already exists on disk"))
      return(read_function(destfile))
    }
  }

  if (!is.function(x)) {
    if (!file.exists(destfile) | ow) {
      download.file(x, destfile)
    } else {
      message(paste0("A local copy of ", x, " already exists on disk"))
    }
    invisible(x)
  }
}

# ---- assertr -----
greater_than_0 <- function(x) {
  if (!is.na(x) & x <= 0) {
    return(FALSE)
  }
}
row_redux <- function(df) {
  df[[1]] - df[[2]]
}

# ---- convienience functions ----

# jsta::key_state
key_state <- function(x) {
  key <- data.frame(state.abb = datasets::state.abb,
    state.name = datasets::state.name,
    stringsAsFactors = FALSE)
  dplyr::left_join(x, key,
    by = c("state.name"))
}

thousand_k <- function(x) {
  res <- rep(NA, length(x))
  for (i in 1:length(x)) {
    if (x[i] >= 1000 & x[i] < Inf) {
      res[i] <- paste0(substring(x[i], 1, 1), ".", substring(x[i], 2, 2), "k")
    } else {
      res[i] <- x[i]
    }
  }
  res
}

# jsta::usa_sf()
usa_sf <- function(crs) {
  res <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  state_key <- data.frame(state = datasets::state.abb,
    ID = tolower(datasets::state.name),
    stringsAsFactors = FALSE)
  res <- dplyr::left_join(res, state_key, by = "ID")
  dplyr::filter(res, !is.na(.data$state))
}

get_csv   <- function(destfile, drive_name) {
  tryCatch(drive_download(drive_name, type = "csv",
    path = destfile),
  error = function(e) {
    print(drive_name)
    write.csv(NA, file = destfile, row.names = FALSE)
    return(NA)
  }
  )
  return(
    read.csv(destfile, stringsAsFactors = FALSE)
  )
}

# convert collection of depth contours/points to a depth raster
poly_to_filled_raster <- function(dt_raw, depth_attr, wh, proj) {
  # use state plane for given state
  dt         <- st_transform(dt_raw, proj)
  dt         <- dplyr::select(dt, all_of(depth_attr))

  r             <- raster(xmn = st_bbox(dt)[1], ymn = st_bbox(dt)[2],
    xmx = st_bbox(dt)[3], ymx = st_bbox(dt)[4])
  r[]           <- NA
  r             <- raster::rasterize(as_Spatial(dt), r, field = depth_attr)
  projection(r) <- projection(as_Spatial(dt))

  r2 <- r
  # r2[which.max(r2[])] <- NaN # dont remember why I did this...

  # moving window(focal) fill - https://stackoverflow.com/a/45658609/3362993
  wh_init <- wh
  while (
    any(is.na(extract(r2, concaveman::concaveman(dt))[[1]])) |
      wh == wh_init
  ) {
    # print(wh)
    w_mat <- matrix(1, wh, wh)
    r2 <- tryCatch(raster::focal(r, w = w_mat,
      fun = function(x) {
        fill.na(x, width = wh)
      },
      pad = TRUE, na.rm = FALSE, NAonly = TRUE),
    error = function(e) r2)
    wh <- wh + 4
  }
  # plot(r2)

  r3 <- mask(r2, concaveman::concaveman(dt))
  # plot(r3)
  list(r = r3, wh = wh)
}

# https://stackoverflow.com/a/45658609/3362993
fill.na <- function(x, width) {
  center <- 0.5 + (width * width / 2)
  if (is.na(x)[center]) {
    return(round(mean(x, na.rm = TRUE), 0))
  } else {
    return(round(x[center], 0))
  }
}

rm_dups <- function(res) {
  # deal with duplicates where depth is missing versus where present
  ## remove the smaller lake of duplicates with no depth data
  res_na <- res %>%
    group_by(llid) %>%
    dplyr::filter(is.na(max_depth_m) & is.na(mean_depth_m)) %>%
    dplyr::filter(lake_waterarea_ha == max(lake_waterarea_ha)) %>%
    arrange(llid) %>%
    ungroup() %>% distinct(llid, lake_waterarea_ha, .keep_all = TRUE)

  ## remove the shallower lake of duplicates with depth data
  res_values <- res %>%
    distinct(llid, max_depth_m, .keep_all = TRUE) %>%
    group_by(llid) %>%
    dplyr::filter(!is.na(max_depth_m) | !is.na(mean_depth_m)) %>%
    # fill NA mean depth within llids with present mean depth data
    # https://stackoverflow.com/a/40515261/3362993
    # arrange(desc(max_depth_m)) %>%
    # add_count() %>%
    # arrange(desc(n), llid) %>%
    # mutate(mean_test = mean_depth_m[which(!is.na(mean_depth_m))[1]]) # %>%
    # View()
    dplyr::filter(max_depth_m == max(max_depth_m)) %>%
    arrange(llid)

  res <- dplyr::bind_rows(res_na, res_values)

  ## deal with lakes that have both missing and present depth in separate rows
  dup_llids <- as.numeric(na.omit(res[duplicated(res$llid), ]$llid))
  res_na <- dplyr::filter(res, llid %in% dup_llids) %>%
    arrange(llid) %>%
    dplyr::filter(max_depth_m == max_depth_m)
  res <- dplyr::filter(res, !(llid %in% res_na$llid))
  res <- bind_rows(res, res_na)

  res
}

# https://stackoverflow.com/a/5173906/3362993
decimalplaces <- function(x) {
  # x <- 0.1
  # x <- 0.100
  if (!is.na(x)) {
    if (abs(x - round(x)) > .Machine$double.eps^0.5) {
      nchar(
        strsplit(as.character(x),
          # sub('0+$', '', as.character(x)), # uncomment 2 ignore trailing zeros
          ".", fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  } else {
    NA
  }
}

convert_ft_m <- function(dt_raw) {
  # track decimal sig figs ft to m based on conversion factor:
  # 0 = 4, 0.1 = 5, 0.01 = 6
  target_decimals <- data.frame(decimal_places_ft = c(0, 1, 2),
    decimal_places_target = c(4, 5, 6),
    stringsAsFactors = FALSE)

  res <- dt_raw %>%
    mutate_at(vars(contains("depth")), as.numeric) %>%
    # ## the ft value should always be greater than the m value if both are present
    assert_rows(row_redux, greater_than_0, c(max_depth_ft, max_depth_m)) %>%
    assert_rows(row_redux, greater_than_0, c(mean_depth_ft, mean_depth_m)) %>%
    # ## max_depth_m > mean_depth_m
    assert_rows(row_redux, greater_than_0, c(max_depth_m, mean_depth_m)) %>%
    mutate(decimal_places_ft = sapply(max_depth_ft, decimalplaces)) %>%
    arrange(desc(decimal_places_ft))

  res <- res %>%
    left_join(target_decimals, by = "decimal_places_ft") %>%
    mutate(max_depth_m = case_when(
      is.na(max_depth_m) & !is.na(max_depth_ft) ~ max_depth_ft * 0.3048,
      TRUE ~ max_depth_m
    )) %>%
    mutate(mean_depth_m = case_when(
      is.na(mean_depth_m) & !is.na(mean_depth_ft) ~ mean_depth_ft * 0.3048,
      TRUE ~ mean_depth_m
    )) %>%
    # dplyr::select(max_depth_m, max_depth_ft,
    #               decimal_places_ft, decimal_places_target) %>%
    mutate(decimal_places_m = sapply(max_depth_m, decimalplaces)) %>%
    # arrange(desc(decimal_places_m)) %>%
    dplyr::select(-mean_depth_ft, -max_depth_ft, -contains("decimal_places"))

  res
}

# single_state("AS:PO")
# single_state("AS PO")
single_state <- function(x) {
  res <- sapply(x, function(y) strsplit(y, ":")[[1]][1])
  sapply(res, function(y) strsplit(y, " ")[[1]][1])
}

calc_depth <- function(slope, distance, grain = 1) {
  # dplyr::filter(test, llid == 2654) %>%
  #   dplyr::select(inlake_slope, dist_deepest)

  # rad_conversion_fac <- 180 / pi

  theta <- atan(slope / grain)
  depth <- tan(theta) * distance
  depth
}

is_url <- function(x) {
  # https://stackoverflow.com/a/3809435/3362993
  length(grep("[-a-zA-Z0-9@:%._\\+~#=]\\.[a-zA-Z0-9()]{2,10}\\b([-a-zA-Z0-9()@:%_\\+.~#?&\\/\\/=]*)",
    x)) > 0
}

# library(maptools)
# library(spatstat)
# test4 <- as(as_Spatial(dt), "ppp")
# width <- ceiling(res(r)[2] / mean(spatstat::nndist(test4, k=1))) * 5
# if(width %% 2 == 0){width <- width + 1}

# ratio of bounding circle area to poly area
# area_ratio <- st_area(
#   lwgeom::st_minimum_bounding_circle(concaveman::concaveman(dt))) /
#   st_area(concaveman::concaveman(dt))

# jsta::jheatmap
jheatmap <- function(dt, focal_columns = NULL) {
  test <- data.frame(dt) %>% dplyr::select_if(is.numeric) %>%
    corrr::correlate(quiet = TRUE) %>% data.frame()
  test_rowname <- test$rowname
  test <- dplyr::select(test, -"rowname")
  row.names(test) <- test_rowname
  if (length(focal_columns) > 0) {
    test <- dplyr::select(test, focal_columns)
  }
  hmap.palette_red <- colorRampPalette(RColorBrewer::brewer.pal(n = 7,
    name = "Reds"))
  hmap.palette_blue <- colorRampPalette(RColorBrewer::brewer.pal(n = 7,
    name = "Blues"))
  frac_color <- ceiling(range(as.numeric(unlist(test)), na.rm = TRUE) *
    10)
  frac_pos <- frac_color[which(frac_color > 0)]
  frac_neg <- frac_color[which(frac_color < 0)]
  if (length(frac_neg) == 0) {
    hmap_cols <- rev(c(rev(hmap.palette_red(frac_pos))))
  }
  else {
    if (length(frac_pos) == 0) {
      hmap_cols <- rev(c(hmap.palette_blue(abs(frac_neg))))
    }
    else {
      hmap_cols <- rev(c(rev(hmap.palette_red(frac_pos)),
        hmap.palette_blue(abs(frac_neg))))
    }
  }
  if (length(focal_columns) == 1) {
    test_rowname <- row.names(test)[order(test[, focal_columns])]
    test <- arrange(test, rlang::UQ(rlang::sym(focal_columns))) %>%
      dplyr::filter(!is.na(rlang::UQ(rlang::sym(focal_columns))))
    test_rowname <- test_rowname[test_rowname != focal_columns]
    row.names(test) <- test_rowname
  }
  if (length(focal_columns) > 0) {
    suppressWarnings(pheatmap::pheatmap(test, color = hmap_cols,
      cluster_rows = FALSE, cluster_cols = FALSE))
  }
  else {
    pheatmap::pheatmap(t(test), color = hmap_cols)
  }
}

# jsta::gg_quantdot
gg_quantdot <- function(dt, grp, var) {
  # dt <- mtcars; grp <- "cyl"; var <- "mpg"

  # https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
  p       <- c(0.05, 0.5, 0.95)
  p_names <- paste0(as.character(p * 100), "%")
  p_funs  <- lapply(seq_along(p), function(x) {
    function(...) {
      quantile(probs = p[x], na.rm = TRUE, ...)
    }
  })
  p_funs <- setNames(p_funs, p_names)

  dt %>%
    group_by({{ grp }}) %>%
    summarise_at({{ var }}, p_funs) %>%
    ggplot() +
    geom_pointrange(aes(x = {{ grp }}, y = .data$`50%`,
      ymin = .data$`5%`, ymax = .data$`95%`)) +
    ylab(var)
}

# https://stackoverflow.com/a/64376355/3362993
# get_formula(lm(dist ~ speed, data = cars))
get_formula <- function(model) {
  broom::tidy(model)[, 1:2] %>%
    mutate(sign = ifelse(sign(estimate) == 1, " + ", " - ")) %>% # coeff signs
    mutate_if(is.numeric, ~ abs(round(., 2))) %>% # for improving formatting
    mutate(a = ifelse(term == "(Intercept)", paste0("y ~ ", estimate), paste0(sign, estimate, " * ", term))) %>%
    summarise(formula = paste(a, collapse = "")) %>%
    as.character()
}

flatten_multipoint <- function(x) {
  list(as.data.frame(st_coordinates(x)))
}

# pad_str("a", 3)
# pad_str(c("a", "b"), 3)[1]
pad_str <- function(x, n) {

  .pad_str <- function(x, n) {
    if (nchar(x) < n) {
      paste0(x, paste0(rep(" ", abs(nchar(x) - n)), collapse = ""))
    } else {
      x
    }
  }
  as.character(sapply(x, function(x) .pad_str(x, n)))
}
