
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 41: BROADER ASTRONOMICAL VALIDATION
# ============================================================

cat("\n")
cat("============================================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 41: BROADER ASTRONOMICAL VALIDATION\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 1. PROJECT CONFIGURATION
# ------------------------------------------------------------

project_root <- "C:/Users/CDT WO AZEEZ SAMAD/Downloads/Global_Adhan_Clock"

setwd(project_root)

required_packages <- c("data.table", "jsonlite")

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      paste(
        "Missing package:", package,
        "\nInstall it using install.packages('", package, "')"
      )
    )
  }
}

library(data.table)

# ------------------------------------------------------------
# 2. LOAD OUR ASTRONOMICAL ENGINE
# ------------------------------------------------------------

source("R/prayer_config.R")
source("R/solar_position.R")
source("R/equation_of_time.R")
source("R/solar_noon.R")
source("R/sunrise_sunset.R")
source("R/prayer_times.R")

cat("ASTRONOMICAL ENGINE LOADED\n")
cat("Calculation method:", prayer_config$method, "\n")
cat("Fajr angle:", prayer_config$fajr_angle, "degrees\n")
cat("Isha angle:", prayer_config$isha_angle, "degrees\n")
cat("Asr method:", prayer_config$asr_method, "\n\n")

# ------------------------------------------------------------
# 3. DEFINE RESEARCH DATES
# ------------------------------------------------------------

season_dates <- data.table(
  season = c(
    "March Equinox",
    "June Solstice",
    "September Equinox",
    "December Solstice"
  ),
  research_date = as.Date(c(
    "2026-03-20",
    "2026-06-21",
    "2026-09-23",
    "2026-12-21"
  ))
)

# ------------------------------------------------------------
# 4. DEFINE LATITUDE BANDS
# ------------------------------------------------------------

latitude_breaks <- c(-90, -66, -23.5, 23.5, 66, 90)

latitude_labels <- c(
  "High Southern",
  "Temperate Southern",
  "Tropical",
  "Temperate Northern",
  "High Northern"
)

target_per_band <- 20

set.seed(4101)

# ------------------------------------------------------------
# 5. LOAD RESEARCH LOCATIONS
# ------------------------------------------------------------

cat("LOADING GLOBAL GEOGRAPHICAL DATA\n")

places <- fread(
  "data/processed/application_places.csv"
)

required_columns <- c(
  "geoname_id",
  "name",
  "latitude",
  "longitude",
  "country_code",
  "population",
  "feature_class",
  "timezone"
)

missing_columns <- setdiff(
  required_columns,
  names(places)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

places <- places[
  feature_class == "P" &
    population >= 100000 &
    !is.na(timezone) &
    !is.na(latitude) &
    !is.na(longitude) &
    latitude >= -90 &
    latitude <= 90 &
    longitude >= -180 &
    longitude <= 180
]

# Keep only recognised timezone names.

valid_timezones <- OlsonNames()

places <- places[
  timezone %chin% valid_timezones
]

places[, latitude_band := cut(
  latitude,
  breaks = latitude_breaks,
  labels = latitude_labels,
  include.lowest = TRUE,
  right = TRUE
)]

places <- places[
  !is.na(latitude_band)
]

cat("Eligible populated places:", nrow(places), "\n")
cat(
  "Available latitude bands:",
  uniqueN(places$latitude_band),
  "\n\n"
)

# ------------------------------------------------------------
# 6. SELECT A STRATIFIED GLOBAL SAMPLE
# ------------------------------------------------------------

cat("SELECTING GLOBAL VALIDATION SAMPLE\n")

validation_sample <- places[
  ,
  .SD[
    sample(
      seq_len(.N),
      min(target_per_band, .N)
    )
  ],
  by = latitude_band
]

validation_sample <- unique(
  validation_sample,
  by = "geoname_id"
)

setorder(
  validation_sample,
  latitude_band,
  country_code,
  name
)

cat(
  "Selected research locations:",
  nrow(validation_sample),
  "\n\n"
)

print(
  validation_sample[
    ,
    .(
      locations = .N,
      countries = uniqueN(country_code)
    ),
    by = latitude_band
  ]
)

# Save the exact sample for reproducibility.

output_directory <- file.path(
  "data",
  "processed",
  "astronomical_validation"
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

fwrite(
  validation_sample,
  file.path(
    output_directory,
    "script41_validation_sample.csv"
  )
)

# ------------------------------------------------------------
# 7. PREPARE ALADHAN API REQUEST FUNCTION
# ------------------------------------------------------------

fetch_aladhan_times <- function(
    research_date,
    latitude,
    longitude,
    timezone
) {
  
  formatted_date <- format(
    as.Date(research_date),
    "%d-%m-%Y"
  )
  
  encoded_timezone <- utils::URLencode(
    timezone,
    reserved = TRUE
  )
  
  api_url <- paste0(
    "https://api.aladhan.com/v1/timings/",
    formatted_date,
    "?latitude=", latitude,
    "&longitude=", longitude,
    "&method=3",
    "&school=0",
    "&latitudeAdjustmentMethod=3",
    "&timezonestring=", encoded_timezone,
    "&iso8601=false"
  )
  
  temporary_file <- tempfile(
    fileext = ".json"
  )
  
  download_status <- tryCatch(
    utils::download.file(
      url = api_url,
      destfile = temporary_file,
      quiet = TRUE,
      mode = "wb",
      method = "libcurl"
    ),
    error = function(e) {
      return(999)
    }
  )
  
  if (
    length(download_status) == 0 ||
    download_status != 0
  ) {
    unlink(temporary_file)
    
    return(list(
      success = FALSE,
      message = "API download failed",
      timings = NULL
    ))
  }
  
  api_response <- tryCatch(
    jsonlite::fromJSON(
      temporary_file,
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )
  
  unlink(temporary_file)
  
  if (is.null(api_response)) {
    return(list(
      success = FALSE,
      message = "Invalid JSON response",
      timings = NULL
    ))
  }
  
  if (
    is.null(api_response$code) ||
    api_response$code != 200
  ) {
    return(list(
      success = FALSE,
      message = "API returned an unsuccessful response",
      timings = NULL
    ))
  }
  
  if (is.null(api_response$data$timings)) {
    return(list(
      success = FALSE,
      message = "Prayer timings missing from response",
      timings = NULL
    ))
  }
  
  return(list(
    success = TRUE,
    message = "Success",
    timings = api_response$data$timings
  ))
}

# ------------------------------------------------------------
# 8. PREPARE TIME CONVERSION FUNCTIONS
# ------------------------------------------------------------

extract_minutes <- function(time_value) {
  
  if (
    is.null(time_value) ||
    length(time_value) == 0 ||
    is.na(time_value)
  ) {
    return(NA_real_)
  }
  
  time_match <- regmatches(
    as.character(time_value),
    regexpr(
      "[0-9]{1,2}:[0-9]{2}",
      as.character(time_value)
    )
  )
  
  if (length(time_match) == 0 || time_match == "") {
    return(NA_real_)
  }
  
  time_parts <- strsplit(
    time_match,
    ":"
  )[[1]]
  
  hour <- as.numeric(time_parts[1])
  minute <- as.numeric(time_parts[2])
  
  if (
    is.na(hour) ||
    is.na(minute) ||
    hour < 0 ||
    hour > 23 ||
    minute < 0 ||
    minute > 59
  ) {
    return(NA_real_)
  }
  
  return(hour * 60 + minute)
}

calculate_time_difference <- function(
    our_minutes,
    api_minutes
) {
  
  if (
    is.na(our_minutes) ||
    is.na(api_minutes)
  ) {
    return(NA_real_)
  }
  
  difference <- abs(
    our_minutes - api_minutes
  )
  
  # Circular difference across midnight.
  
  return(
    min(difference, 1440 - difference)
  )
}

# ------------------------------------------------------------
# 9. DEFINE PRAYERS
# ------------------------------------------------------------

prayer_names <- c(
  "Fajr",
  "Dhuhr",
  "Asr",
  "Maghrib",
  "Isha"
)

# ------------------------------------------------------------
# 10. RUN VALIDATION EXPERIMENT
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("STARTING ASTRONOMICAL VALIDATION\n")
cat("============================================================\n\n")

validation_results <- list()

result_counter <- 0

total_requests <- nrow(validation_sample) *
  nrow(season_dates)

request_counter <- 0

for (place_index in seq_len(nrow(validation_sample))) {
  
  place <- validation_sample[place_index]
  
  for (date_index in seq_len(nrow(season_dates))) {
    
    season <- season_dates[date_index]
    
    request_counter <- request_counter + 1
    
    cat(
      "\nRequest",
      request_counter,
      "of",
      total_requests,
      "\n"
    )
    
    cat(
      "Place:", place$name,
      "| Country:", place$country_code,
      "| Season:", season$season,
      "\n"
    )
    
    # --------------------------------------------------------
    # OUR R CALCULATION
    # --------------------------------------------------------
    
    our_result <- tryCatch(
      calculate_prayer_times_for_place(
        research_date = season$research_date,
        latitude = place$latitude,
        longitude = place$longitude,
        timezone = place$timezone,
        config = prayer_config
      ),
      error = function(e) NULL
    )
    
    # --------------------------------------------------------
    # ALADHAN REFERENCE CALCULATION
    # --------------------------------------------------------
    
    api_result <- fetch_aladhan_times(
      research_date = season$research_date,
      latitude = place$latitude,
      longitude = place$longitude,
      timezone = place$timezone
    )
    
    # --------------------------------------------------------
    # COMPARE EACH PRAYER
    # --------------------------------------------------------
    
    for (prayer in prayer_names) {
      
      our_field <- paste0(
        tolower(prayer),
        "_local"
      )
      
      our_time <- NA_character_
      
      if (
        !is.null(our_result) &&
        our_field %in% names(our_result)
      ) {
        our_time <- as.character(
          our_result[[our_field]]
        )
      }
      
      api_time <- NA_character_
      
      if (
        isTRUE(api_result$success) &&
        prayer %in% names(api_result$timings)
      ) {
        api_time <- as.character(
          api_result$timings[[prayer]]
        )
      }
      
      our_minutes <- extract_minutes(
        our_time
      )
      
      api_minutes <- extract_minutes(
        api_time
      )
      
      difference <- calculate_time_difference(
        our_minutes,
        api_minutes
      )
      
      # ------------------------------------------------------
      # COMPARISON STATUS
      # ------------------------------------------------------
      
      if (is.null(our_result)) {
        
        comparison_status <- "Our calculation failed"
        
      } else if (!api_result$success) {
        
        comparison_status <- "API request failed"
        
      } else if (is.na(our_minutes)) {
        
        comparison_status <- "Our prayer unavailable"
        
      } else if (is.na(api_minutes)) {
        
        comparison_status <- "API prayer unavailable"
        
      } else {
        
        comparison_status <- "Comparable"
        
      }
      
      result_counter <- result_counter + 1
      
      validation_results[[result_counter]] <- data.table(
        geoname_id = place$geoname_id,
        place_name = place$name,
        country_code = place$country_code,
        latitude = place$latitude,
        longitude = place$longitude,
        latitude_band = as.character(
          place$latitude_band
        ),
        timezone = place$timezone,
        season = season$season,
        research_date = as.character(
          season$research_date
        ),
        prayer = prayer,
        our_time_local = our_time,
        api_time_local = api_time,
        our_minutes = our_minutes,
        api_minutes = api_minutes,
        difference_minutes = difference,
        comparison_status = comparison_status,
        api_status = api_result$message
      )
    }
    
    # A short pause between API requests.
    
    Sys.sleep(0.25)
  }
}

# ------------------------------------------------------------
# 11. COMBINE VALIDATION RESULTS
# ------------------------------------------------------------

cat("\n")
cat("COMBINING VALIDATION RESULTS\n")

validation_data <- rbindlist(
  validation_results,
  fill = TRUE
)

fwrite(
  validation_data,
  file.path(
    output_directory,
    "script41_all_validation_results.csv"
  )
)

# ------------------------------------------------------------
# 12. OVERALL QUALITY CHECK
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("OVERALL VALIDATION QUALITY\n")
cat("============================================================\n")

cat(
  "Selected places:",
  nrow(validation_sample),
  "\n"
)

cat(
  "Location-date requests:",
  total_requests,
  "\n"
)

cat(
  "Prayer comparison records:",
  nrow(validation_data),
  "\n"
)

cat(
  "Comparable prayer records:",
  validation_data[
    comparison_status == "Comparable",
    .N
  ],
  "\n"
)

cat(
  "Unavailable or failed records:",
  validation_data[
    comparison_status != "Comparable",
    .N
  ],
  "\n"
)

# ------------------------------------------------------------
# 13. CALCULATE OVERALL PRAYER ACCURACY METRICS
# ------------------------------------------------------------

comparable_data <- validation_data[
  comparison_status == "Comparable" &
    !is.na(difference_minutes)
]

if (nrow(comparable_data) > 0) {
  
  prayer_summary <- comparable_data[
    ,
    .(
      comparisons = .N,
      
      mean_absolute_difference = mean(
        difference_minutes
      ),
      
      median_difference = median(
        difference_minutes
      ),
      
      percentile_95 = as.numeric(
        quantile(
          difference_minutes,
          0.95,
          na.rm = TRUE
        )
      ),
      
      maximum_difference = max(
        difference_minutes
      ),
      
      within_1_minute = sum(
        difference_minutes <= 1
      ),
      
      within_2_minutes = sum(
        difference_minutes <= 2
      ),
      
      within_3_minutes = sum(
        difference_minutes <= 3
      )
    ),
    by = prayer
  ]
  
  prayer_summary[
    ,
    percentage_within_1_minute :=
      100 * within_1_minute / comparisons
  ]
  
  prayer_summary[
    ,
    percentage_within_2_minutes :=
      100 * within_2_minutes / comparisons
  ]
  
  prayer_summary[
    ,
    percentage_within_3_minutes :=
      100 * within_3_minutes / comparisons
  ]
  
  setorder(
    prayer_summary,
    match(prayer, prayer_names)
  )
  
} else {
  
  prayer_summary <- data.table()
}

fwrite(
  prayer_summary,
  file.path(
    output_directory,
    "script41_prayer_accuracy_summary.csv"
  )
)

# ------------------------------------------------------------
# 14. ANALYSE ACCURACY BY LATITUDE BAND
# ------------------------------------------------------------

if (nrow(comparable_data) > 0) {
  
  latitude_summary <- comparable_data[
    ,
    .(
      comparisons = .N,
      
      mean_difference = mean(
        difference_minutes
      ),
      
      median_difference = median(
        difference_minutes
      ),
      
      maximum_difference = max(
        difference_minutes
      ),
      
      within_2_minutes = sum(
        difference_minutes <= 2
      ),
      
      percentage_within_2_minutes =
        100 * mean(difference_minutes <= 2)
    ),
    by = latitude_band
  ]
  
} else {
  
  latitude_summary <- data.table()
}

fwrite(
  latitude_summary,
  file.path(
    output_directory,
    "script41_latitude_accuracy_summary.csv"
  )
)

# ------------------------------------------------------------
# 15. ANALYSE RESULTS BY SEASON
# ------------------------------------------------------------

if (nrow(comparable_data) > 0) {
  
  seasonal_summary <- comparable_data[
    ,
    .(
      comparisons = .N,
      
      mean_difference = mean(
        difference_minutes
      ),
      
      median_difference = median(
        difference_minutes
      ),
      
      maximum_difference = max(
        difference_minutes
      ),
      
      percentage_within_2_minutes =
        100 * mean(difference_minutes <= 2)
    ),
    by = season
  ]
  
} else {
  
  seasonal_summary <- data.table()
}

fwrite(
  seasonal_summary,
  file.path(
    output_directory,
    "script41_seasonal_accuracy_summary.csv"
  )
)

# ------------------------------------------------------------
# 16. INVESTIGATE UNAVAILABLE PRAYERS
# ------------------------------------------------------------

unavailable_summary <- validation_data[
  comparison_status != "Comparable",
  .(
    records = .N,
    affected_places = uniqueN(geoname_id)
  ),
  by = .(
    latitude_band,
    prayer,
    comparison_status
  )
]

fwrite(
  unavailable_summary,
  file.path(
    output_directory,
    "script41_unavailable_prayers_summary.csv"
  )
)

# ------------------------------------------------------------
# 17. IDENTIFY LARGEST DIFFERENCES
# ------------------------------------------------------------

largest_differences <- comparable_data[
  order(-difference_minutes)
][1:min(30, .N)]

fwrite(
  largest_differences,
  file.path(
    output_directory,
    "script41_largest_differences.csv"
  )
)

# ------------------------------------------------------------
# 18. PRINT RESULTS
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PRAYER ACCURACY SUMMARY\n")
cat("============================================================\n")

print(prayer_summary)

cat("\n")
cat("============================================================\n")
cat("LATITUDE BAND ACCURACY\n")
cat("============================================================\n")

print(latitude_summary)

cat("\n")
cat("============================================================\n")
cat("SEASONAL ACCURACY\n")
cat("============================================================\n")

print(seasonal_summary)

cat("\n")
cat("============================================================\n")
cat("UNAVAILABLE PRAYER INVESTIGATION\n")
cat("============================================================\n")

print(unavailable_summary)

cat("\n")
cat("============================================================\n")
cat("LARGEST PRAYER-TIME DIFFERENCES\n")
cat("============================================================\n")

print(largest_differences)

# ------------------------------------------------------------
# 19. FINAL VALIDATION REPORT
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("SCRIPT 41 FINAL QUALITY CHECK\n")
cat("============================================================\n")

cat(
  "Research locations:",
  nrow(validation_sample),
  "\n"
)

cat(
  "Seasonal dates:",
  nrow(season_dates),
  "\n"
)

cat(
  "Prayer records:",
  nrow(validation_data),
  "\n"
)

cat(
  "Comparable records:",
  nrow(comparable_data),
  "\n"
)

cat(
  "Unique locations:",
  uniqueN(validation_data$geoname_id),
  "\n"
)

cat(
  "Unique countries:",
  uniqueN(validation_data$country_code),
  "\n"
)

cat(
  "Unique latitude bands:",
  uniqueN(validation_data$latitude_band),
  "\n"
)

cat("\nOUTPUT DIRECTORY:\n")
cat(output_directory, "\n")

cat("\nOUTPUT FILES:\n")
cat("1. script41_validation_sample.csv\n")
cat("2. script41_all_validation_results.csv\n")
cat("3. script41_prayer_accuracy_summary.csv\n")
cat("4. script41_latitude_accuracy_summary.csv\n")
cat("5. script41_seasonal_accuracy_summary.csv\n")
cat("6. script41_unavailable_prayers_summary.csv\n")
cat("7. script41_largest_differences.csv\n")

cat("\n")
cat("============================================================\n")
cat("SCRIPT 41 COMPLETED\n")
cat("============================================================\n")
