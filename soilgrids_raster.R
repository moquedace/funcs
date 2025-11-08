soil_attributes <- c(
  "bdod",
  "cec",
  "cfvo",
  "clay",
  "nitrogen",
  "ocd",
  "ocs",
  "phh2o",
  "sand",
  "silt",
  "soc",
  "wrb"
)

depths_cm <- c(
  "0-5",
  "5-15",
  "15-30",
  "30-60",
  "60-100",
  "100-200"
)

metrics <- c(
  "mean",
  "Q0.05",
  "Q0.5",
  "Q0.95",
  "uncertainty"
)


combinations <- expand.grid(
  attribute = soil_attributes_eng,
  depths = depths_cm,
  metrics = metrics
)


soilgrids_raster <- function(
    attribute,
    depth,
    metric,
    base_url       = "https://files.isric.org/soilgrids/latest/data/",
    vsicurl_prefix = "/vsicurl/"
) {
  
  source("https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R")
  
  pkg <- c(
    "dplyr", "terra", "geobr", "sf", "future.apply",
    "stringr", "janitor", "ggplot2",
    "data.table", 
    "future", 
    "xml2", "rvest"
  )
  
  install_load_pkg(pkg)
  
  
  attribute_url <- paste0(base_url, attribute)
  
  html_page <- tryCatch(
    read_html(attribute_url),
    error = function(e) {
      stop(
        sprintf(
          "Could not access URL for attribute '%s'. Check if the attribute exists or if the URL is reachable.\nURL: %s",
          attribute, attribute_url
        )
      )
    }
  )
  
  vrt_files <- html_page %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    { .[str_detect(., "\\.vrt$")] }
  
  if (!length(vrt_files)) {
    stop(sprintf("No .vrt files found for attribute '%s'.", attribute))
  }
  
  vrt_depth <- vrt_files %>%
    { .[str_detect(., depth)] }
  
  if (!length(vrt_depth)) {
    stop(sprintf(
      "No .vrt files found for depth '%s' in attribute '%s'.",
      depth, attribute
    ))
  }
  
  vrt_depth_metric <- vrt_depth %>%
    { .[str_detect(., metric)] }
  
  if (!length(vrt_depth_metric)) {
    stop(sprintf(
      "No .vrt files found for metric '%s' at depth '%s' in attribute '%s'.",
      metric, depth, attribute
    ))
  }
  
  vrt_paths <- paste0(vsicurl_prefix, attribute_url, "/", vrt_depth_metric)
  
  terra::rast(vrt_paths) %>% 
    `names<-`(
      make_clean_names(
        paste(
          attribute, depth, metric, sep = "_"
        )
      )
    )
}

