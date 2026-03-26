library(cli)
cli_rule(left = "Initializing script")

cli_alert_info("Loading R packages")

library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(openxlsx, warn.conflicts = FALSE)
library(yaml, warn.conflicts = FALSE)
library(rcdf, warn.conflicts = FALSE)
library(phscs, warn.conflicts = FALSE)
library(tsg, warn.conflicts = FALSE)


# Load configuration file
cli_alert_info("Reading configuration file")
config <- yaml::read_yaml("config.yaml", readLines.warn = FALSE)
area <- paste(config$area_code, config$area_name)
area_overall <- config$area_name[1]
if(length(config$area_code) > 1 & !is.null(config$area)) {
  area_overall <- config$area
}

# Load data
path_input_data <- config$path$input_data
file_format <- str_remove(config$input_data_type, "^\\.+")
file_input_data <- paste0(area, ".", file_format)
path_output <- file.path(config$path$output, config$cbms_round)

if(!dir.exists(path_output)) {
  dir.create(path_output, recursive = TRUE)
}

if(is.null(path_input_data)) {
  path_input_data <- file.path(
    "data",
    config$cbms_round,
    file_format,
    file_input_data
  )
}

# Read environment variables
env <- read_env()
cli_alert_info("Loading data")

data <- read_rcdf(
  path = path_input_data,
  decryption_key = sapply(paste0('PRIVATE_KEY_PATH', "_", config$area_code), \(x) env[[x]]),
  password = sapply(paste0('PRIVATE_KEY_PW', "_", config$area_code), \(x) env[[x]]),
  return_meta = TRUE
)


if(!(config$aggregation$level %in% c("barangay", "city_mun"))) {
  stop("Invalid `aggregation$level` indicated in the `config.yaml`. This only accepts 'city_mun' or 'barangay.'")
}


area_names <- attributes(data)$metadata$area_names |>
  dplyr::select(
    city_mun_geo_code = id,
    barangay_geo_code = area_code,
    city_mun = name,
    barangay = area_name,
  )

for(record in names(data)) {

  if(record == "__data_dictionary") next

  data[[record]] <- data[[record]] |>
    mutate(
      barangay_geo_code = paste0(
        region_code,
        province_code,
        city_mun_code,
        barangay_code
      )
    ) |>
    left_join(area_names, by = "barangay_geo_code") |>
    select(
      any_of("uuid"),
      city_mun_geo_code,
      barangay_geo_code,
      city_mun,
      barangay,
      everything()
    ) |>
    rename_label(
      barangay_geo_code = "Barangay geographic code",
      city_mun_geo_code = "City/municipality geographic code",
      city_mun = "City/municipality",
      barangay = "Barangay"
    )
}

# Extract data dictionary from RCDF metadata
data_dictionary <- data[['__data_dictionary']]
data[['__data_dictionary']] <- NULL

data_stats <- c()

for(i in seq_along(data)) {

  if(i == "__data_dictionary") next

  data_stats <- c(
    data_stats,
    "v" = paste0(
      stringr::str_pad(paste0(names(data)[i], " "), pad = " ", side = "right", width = 40),
      cli::style_bold(col_green(
        stringr::str_pad(
          formatC(nrow(data[[i]]), big.mark = ','),
          width = 7,
        )
      )), " rows × ",
      cli::style_bold(col_green(
        stringr::str_pad(
          formatC(ncol(data[[i]]), big.mark = ','),
          width = 3,
        )
      )), " columns"
    )
  )
}

cli_rule(left = "{area}")
cli_verbatim(data_stats)
cli_text('\n')

script_path <- config$path$script

if(!is.null(script_path)) {
  if(file.exists(script_path) & str_detect(script_path, "\\.(r|R)$")) {
    source(script_path)
  }
} else {
  warning("Script path is not specified in the `config.yaml`.")
}
