
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 03: DATA QUALITY CHECKING
# =====================================================

library(readr)

# 1. Define our dataset

input_file <- "data/processed/populated_places.csv"

# 2. Create counters

total_rows <- 0
missing_names <- 0
missing_latitudes <- 0
missing_longitudes <- 0
missing_timezones <- 0
invalid_latitudes <- 0
invalid_longitudes <- 0
missing_populations <- 0

# 3. Create a function to inspect each chunk

check_chunk <- function(chunk, position) {
  
  total_rows <<- total_rows + nrow(chunk)
  
  missing_names <<- missing_names +
    sum(is.na(chunk$name) | trimws(chunk$name) == "")
  
  missing_latitudes <<- missing_latitudes +
    sum(is.na(chunk$latitude))
  
  missing_longitudes <<- missing_longitudes +
    sum(is.na(chunk$longitude))
  
  missing_timezones <<- missing_timezones +
    sum(is.na(chunk$timezone) | trimws(chunk$timezone) == "")
  
  invalid_latitudes <<- invalid_latitudes +
    sum(
      !is.na(chunk$latitude) &
        (chunk$latitude < -90 | chunk$latitude > 90)
    )
  
  invalid_longitudes <<- invalid_longitudes +
    sum(
      !is.na(chunk$longitude) &
        (chunk$longitude < -180 | chunk$longitude > 180)
    )
  
  missing_populations <<- missing_populations +
    sum(is.na(chunk$population))
  
  cat("Checked", total_rows, "records\n")
}

# 4. Read the CSV in manageable chunks

readr::read_csv_chunked(
  file = input_file,
  
  callback = readr::DataFrameCallback$new(check_chunk),
  
  chunk_size = 100000,
  
  col_types = readr::cols(
    geoname_id = readr::col_double(),
    name = readr::col_character(),
    latitude = readr::col_double(),
    longitude = readr::col_double(),
    feature_class = readr::col_character(),
    feature_code = readr::col_character(),
    country_code = readr::col_character(),
    population = readr::col_double(),
    timezone = readr::col_character()
  ),
  
  progress = TRUE
)

# 5. Display our quality report

quality_report <- data.frame(
  Check = c(
    "Total records",
    "Missing names",
    "Missing latitudes",
    "Missing longitudes",
    "Missing time zones",
    "Invalid latitudes",
    "Invalid longitudes",
    "Missing populations"
  ),
  
  Count = c(
    total_rows,
    missing_names,
    missing_latitudes,
    missing_longitudes,
    missing_timezones,
    invalid_latitudes,
    invalid_longitudes,
    missing_populations
  )
)

cat("\nGLOBAL ADHAN CLOCK DATA QUALITY REPORT\n")

print(quality_report)

cat("\nDATA QUALITY CHECK COMPLETE\n")
