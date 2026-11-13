
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 41B: HIGH-LATITUDE METHOD COMPARISON
#
# Compare AlAdhan's three high-latitude adjustment methods.
# Investigate the largest Script 41 discrepancies.
#
# Method 1: Middle of the Night
# Method 2: One Seventh
# Method 3: Angle Based
#
# No changes to our astronomical engine.
# ============================================================

library(data.table)
library(jsonlite)

cat("\n====================================================\n")
cat("GLOBAL ADHAN CLOCK - SCRIPT 41B\n")
cat("HIGH-LATITUDE METHOD COMPARISON\n")
cat("====================================================\n")

# ------------------------------------------------------------
# 1. FILES
# ------------------------------------------------------------

validation_file <- file.path(
  "data", "processed", "astronomical_validation",
  "script41_all_validation_results.csv"
)

output_directory <- file.path(
  "data", "processed", "astronomical_validation",
  "high_latitude_method_comparison"
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(validation_file)) {
  stop("Script 41 validation file not found.")
}

validation_data <- fread(validation_file)

# ------------------------------------------------------------
# 2. TARGET LOCATIONS AND DATES
# ------------------------------------------------------------

test_cases <- data.table(
  geoname_id = c(
    710719,
    1497337,
    1497337,
    524305
  ),
  place_name = c(
    "Chernivtsi",
    "Norilsk",
    "Norilsk",
    "Murmansk"
  ),
  research_date = c(
    "2026-06-21",
    "2026-03-20",
    "2026-09-23",
    "2026-09-23"
  )
)

# Retrieve coordinates and timezone from saved results.
place_information <- unique(
  validation_data[
    ,
    .(
      geoname_id,
      latitude,
      longitude,
      timezone
    )
  ]
)

test_cases <- merge(
  test_cases,
  place_information,
  by = "geoname_id",
  all.x = TRUE
)

if (anyNA(test_cases$latitude) ||
    anyNA(test_cases$longitude) ||
    anyNA(test_cases$timezone)) {
  stop("A target location is missing geographical information.")
}

# ------------------------------------------------------------
# 3. API FUNCTION
# ------------------------------------------------------------

fetch_api_times <- function(
    latitude,
    longitude,
    timezone,
    research_date,
    adjustment_method
) {
  
  date_parts <- strsplit(
    research_date,
    "-"
  )[[1]]
  
  api_date <- paste(
    date_parts[3],
    date_parts[2],
    date_parts[1],
    sep = "-"
  )
  
  encoded_timezone <- URLencode(
    timezone,
    reserved = TRUE
  )
  
  request_url <- paste0(
    "https://api.aladhan.com/v1/timings/",
    api_date,
    "?latitude=", latitude,
    "&longitude=", longitude,
    "&method=3",
    "&school=0",
    "&latitudeAdjustmentMethod=", adjustment_method,
    "&timezonestring=", encoded_timezone,
    "&iso8601=false"
  )
  
  temporary_file <- tempfile(fileext = ".json")
  
  result <- tryCatch({
    
    download.file(
      request_url,
      destfile = temporary_file,
      mode = "wb",
      quiet = TRUE
    )
    
    response <- fromJSON(
      paste(
        readLines(
          temporary_file,
          warn = FALSE
        ),
        collapse = ""
      ),
      simplifyVector = FALSE
    )
    
    if (response$code != 200) {
      stop("API returned a non-200 response.")
    }
    
    timings <- response$data$timings
    metadata <- response$data$meta
    
    data.table(
      api_status = "Success",
      api_timezone = metadata$timezone,
      api_method = metadata$method$name,
      api_latitude_adjustment =
        metadata$latitudeAdjustmentMethod,
      Fajr = substr(timings$Fajr, 1, 5),
      Dhuhr = substr(timings$Dhuhr, 1, 5),
      Asr = substr(timings$Asr, 1, 5),
      Maghrib = substr(timings$Maghrib, 1, 5),
      Isha = substr(timings$Isha, 1, 5)
    )
    
  }, error = function(e) {
    
    data.table(
      api_status = paste(
        "Failed:",
        conditionMessage(e)
      ),
      api_timezone = NA_character_,
      api_method = NA_character_,
      api_latitude_adjustment = NA_character_,
      Fajr = NA_character_,
      Dhuhr = NA_character_,
      Asr = NA_character_,
      Maghrib = NA_character_,
      Isha = NA_character_
    )
    
  })
  
  if (file.exists(temporary_file)) {
    unlink(temporary_file)
  }
  
  result
}

# ------------------------------------------------------------
# 4. REQUEST ALL THREE ADJUSTMENT METHODS
# ------------------------------------------------------------

adjustment_methods <- c(1, 2, 3)

results <- list()
result_counter <- 1

for (i in seq_len(nrow(test_cases))) {
  
  current_case <- test_cases[i]
  
  for (method in adjustment_methods) {
    
    cat(
      "\nTesting:",
      current_case$place_name,
      current_case$research_date,
      "| Adjustment method:",
      method,
      "\n"
    )
    
    api_result <- fetch_api_times(
      latitude = current_case$latitude,
      longitude = current_case$longitude,
      timezone = current_case$timezone,
      research_date = current_case$research_date,
      adjustment_method = method
    )
    
    api_result[
      ,
      `:=`(
        geoname_id = current_case$geoname_id,
        place_name = current_case$place_name,
        research_date = current_case$research_date,
        latitude = current_case$latitude,
        longitude = current_case$longitude,
        timezone = current_case$timezone,
        adjustment_method = method
      )
    ]
    
    results[[result_counter]] <- api_result
    
    result_counter <- result_counter + 1
    
    Sys.sleep(0.3)
  }
}

api_results <- rbindlist(
  results,
  fill = TRUE
)

# ------------------------------------------------------------
# 5. RESHAPE RESULTS
# ------------------------------------------------------------

long_results <- melt(
  api_results,
  id.vars = c(
    "geoname_id",
    "place_name",
    "research_date",
    "latitude",
    "longitude",
    "timezone",
    "adjustment_method",
    "api_status",
    "api_timezone",
    "api_method",
    "api_latitude_adjustment"
  ),
  measure.vars = c(
    "Fajr",
    "Dhuhr",
    "Asr",
    "Maghrib",
    "Isha"
  ),
  variable.name = "prayer",
  value.name = "api_time"
)

long_results[
  ,
  api_minutes := fifelse(
    !is.na(api_time),
    as.integer(substr(api_time, 1, 2)) * 60 +
      as.integer(substr(api_time, 4, 5)),
    NA_integer_
  )
]

# ------------------------------------------------------------
# 6. COMPARE ADJUSTMENT METHODS
# ------------------------------------------------------------

method_comparison <- dcast(
  long_results,
  geoname_id + place_name + research_date +
    prayer ~ adjustment_method,
  value.var = "api_time"
)

setnames(
  method_comparison,
  old = c("1", "2", "3"),
  new = c(
    "middle_of_night",
    "one_seventh",
    "angle_based"
  ),
  skip_absent = TRUE
)

# ------------------------------------------------------------
# 7. SAVE OUTPUTS
# ------------------------------------------------------------

fwrite(
  api_results,
  file.path(
    output_directory,
    "script41B_api_method_results.csv"
  )
)

fwrite(
  long_results,
  file.path(
    output_directory,
    "script41B_api_results_long.csv"
  )
)

fwrite(
  method_comparison,
  file.path(
    output_directory,
    "script41B_method_comparison.csv"
  )
)

# ------------------------------------------------------------
# 8. PRINT RESULTS
# ------------------------------------------------------------

cat("\n====================================================\n")
cat("API REQUEST STATUS\n")
cat("====================================================\n")

print(
  api_results[
    ,
    .N,
    by = .(
      adjustment_method,
      api_status
    )
  ]
)

cat("\n====================================================\n")
cat("HIGH-LATITUDE METHOD COMPARISON\n")
cat("====================================================\n")

print(method_comparison)

cat("\n====================================================\n")
cat("REFERENCE API METADATA\n")
cat("====================================================\n")

print(
  unique(
    api_results[
      ,
      .(
        place_name,
        research_date,
        adjustment_method,
        api_timezone,
        api_method,
        api_latitude_adjustment
      )
    ]
  )
)

cat("\n====================================================\n")
cat("SCRIPT 41B COMPLETED\n")
cat("====================================================\n")

cat("API requests attempted:", nrow(api_results), "\n")
cat("Successful requests:",
    sum(api_results$api_status == "Success"), "\n")

cat("\nSaved to:\n")
cat(output_directory, "\n")
