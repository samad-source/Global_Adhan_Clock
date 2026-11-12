
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 24: LOCAL-DATE-AWARE GLOBAL SUNRISE/SUNSET
# VERSION 3 — UTC DATE-BOUNDARY CORRECTION
# =====================================================


# =====================================================
# 1. LOAD PACKAGES AND ASTRONOMICAL FUNCTIONS
# =====================================================

library(data.table)

source("R/solar_position.R")
source("R/equation_of_time.R")
source("R/prayer_config.R")


# =====================================================
# 2. SET RESEARCH DATE
# =====================================================

research_date <- "2026-11-10"

stopifnot(!is.na(as.Date(research_date)))


# =====================================================
# 3. LOAD GLOBAL GEOGRAPHICAL DATA
# =====================================================

places <- fread(
  "data/processed/global_sunrise_sunset.csv"
)

cat("\n====================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("LOCAL-DATE-AWARE SOLAR CALCULATOR\n")
cat("====================================\n")

cat("Research date:", research_date, "\n")
cat("Locations loaded:", nrow(places), "\n")


# =====================================================
# 4. PRESERVE ORIGINAL SCRIPT 22 COLUMNS
# =====================================================

if ("sunrise_utc" %in% names(places)) {
  setnames(
    places,
    "sunrise_utc",
    "sunrise_utc_script22"
  )
}

if ("sunset_utc" %in% names(places)) {
  setnames(
    places,
    "sunset_utc",
    "sunset_utc_script22"
  )
}


# =====================================================
# 5. PREPARE RESULT STORAGE
# =====================================================

n <- nrow(places)

local_noon_epoch <- rep(NA_real_, n)
solar_noon_epoch <- rep(NA_real_, n)

sunrise_epoch <- rep(NA_real_, n)
sunset_epoch <- rep(NA_real_, n)

declination_results <- rep(NA_real_, n)
equation_of_time_results <- rep(NA_real_, n)

calculation_status <- rep(
  "Not calculated",
  n
)


# =====================================================
# 6. VALIDATE TIME ZONES
# =====================================================

valid_timezones <- OlsonNames()

timezone_valid <- (
  !is.na(places$timezone) &
    trimws(places$timezone) %in% valid_timezones
)

cat(
  "Valid time zones:",
  sum(timezone_valid),
  "\n"
)

cat(
  "Invalid time zones:",
  sum(!timezone_valid),
  "\n"
)


# =====================================================
# 7. CALCULATE EACH LOCATION
# =====================================================

cat("\nStarting astronomical calculations...\n")

for (i in seq_len(n)) {
  
  # Progress indicator
  
  if (i %% 500 == 0 || i == n) {
    cat(
      "Processed:",
      i,
      "of",
      n,
      "locations\n"
    )
  }
  
  # Validate time zone
  
  if (!timezone_valid[i]) {
    calculation_status[i] <- "Invalid time zone"
    next
  }
  
  # Extract geographical information
  
  latitude <- places$latitude[i]
  longitude <- places$longitude[i]
  
  timezone_name <- trimws(
    places$timezone[i]
  )
  
  # Validate coordinates
  
  if (
    !is.finite(latitude) ||
    !is.finite(longitude) ||
    latitude < -90 ||
    latitude > 90 ||
    longitude < -180 ||
    longitude > 180
  ) {
    calculation_status[i] <- "Invalid coordinates"
    next
  }
  
  
  # ---------------------------------------------------
  # CALCULATE LOCATION INDEPENDENTLY
  # ---------------------------------------------------
  
  result <- tryCatch({
    
    # A. LOCAL NOON
    
    local_noon <- as.POSIXct(
      paste(research_date, "12:00:00"),
      format = "%Y-%m-%d %H:%M:%S",
      tz = timezone_name
    )
    
    if (is.na(local_noon)) {
      stop("Unable to calculate local noon")
    }
    
    # Convert to a UTC instant
    
    local_noon_utc <- as.POSIXct(
      as.numeric(local_noon),
      origin = "1970-01-01",
      tz = "UTC"
    )
    
    
    # B. SOLAR POSITION
    
    declination <- calculate_solar_declination(
      local_noon_utc
    )
    
    equation_of_time <- calculate_equation_of_time(
      local_noon_utc
    )
    
    if (
      !is.finite(declination) ||
      !is.finite(equation_of_time)
    ) {
      stop("Invalid solar position calculation")
    }
    
    
    # C. SOLAR NOON
    
    utc_date <- format(
      local_noon_utc,
      "%Y-%m-%d",
      tz = "UTC"
    )
    
    utc_midnight <- as.POSIXct(
      paste(utc_date, "00:00:00"),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
    
    solar_noon_minutes <- (
      720 -
        4 * longitude -
        equation_of_time
    )
    
    # Initial candidate timestamp
    
    solar_noon_time <- utc_midnight +
      solar_noon_minutes * 60
    
    # Correct the UTC date by finding the candidate
    # closest to the local-noon UTC instant.
    
    time_difference_seconds <- as.numeric(
      difftime(
        solar_noon_time,
        local_noon_utc,
        units = "secs"
      )
    )
    
    # Move forward by one UTC day when necessary
    
    while (time_difference_seconds < -43200) {
      
      solar_noon_time <- solar_noon_time + 86400
      
      time_difference_seconds <- as.numeric(
        difftime(
          solar_noon_time,
          local_noon_utc,
          units = "secs"
        )
      )
    }
    
    # Move backward by one UTC day when necessary
    
    while (time_difference_seconds > 43200) {
      
      solar_noon_time <- solar_noon_time - 86400
      
      time_difference_seconds <- as.numeric(
        difftime(
          solar_noon_time,
          local_noon_utc,
          units = "secs"
        )
      )
    }
    
    
    # D. SUNRISE AND SUNSET
    
    latitude_rad <- latitude * pi / 180
    
    declination_rad <- declination * pi / 180
    
    zenith_rad <- (
      prayer_config$sunrise_sunset_zenith *
        pi / 180
    )
    
    cos_hour_angle <- (
      cos(zenith_rad) -
        sin(latitude_rad) * sin(declination_rad)
    ) / (
      cos(latitude_rad) * cos(declination_rad)
    )
    
    if (!is.finite(cos_hour_angle)) {
      stop("Invalid solar hour angle")
    }
    
    # Check whether sunrise and sunset are possible
    
    if (
      cos_hour_angle < -1 ||
      cos_hour_angle > 1
    ) {
      
      list(
        local_noon = as.numeric(local_noon_utc),
        solar_noon = as.numeric(solar_noon_time),
        sunrise = NA_real_,
        sunset = NA_real_,
        declination = declination,
        equation_of_time = equation_of_time,
        status = "Sunrise/sunset unavailable"
      )
      
    } else {
      
      hour_angle <- acos(cos_hour_angle) * 180 / pi
      
      sunrise_time <- solar_noon_time -
        4 * hour_angle * 60
      
      sunset_time <- solar_noon_time +
        4 * hour_angle * 60
      
      list(
        local_noon = as.numeric(local_noon_utc),
        solar_noon = as.numeric(solar_noon_time),
        sunrise = as.numeric(sunrise_time),
        sunset = as.numeric(sunset_time),
        declination = declination,
        equation_of_time = equation_of_time,
        status = "Successful"
      )
    }
    
  }, error = function(e) {
    
    list(
      status = paste(
        "Calculation error:",
        conditionMessage(e)
      )
    )
    
  })
  
  
  # ---------------------------------------------------
  # E. STORE RESULTS
  # ---------------------------------------------------
  
  if (!is.null(result$local_noon)) {
    local_noon_epoch[i] <- result$local_noon
  }
  
  if (!is.null(result$solar_noon)) {
    solar_noon_epoch[i] <- result$solar_noon
  }
  
  if (!is.null(result$sunrise)) {
    sunrise_epoch[i] <- result$sunrise
  }
  
  if (!is.null(result$sunset)) {
    sunset_epoch[i] <- result$sunset
  }
  
  if (!is.null(result$declination)) {
    declination_results[i] <- result$declination
  }
  
  if (!is.null(result$equation_of_time)) {
    equation_of_time_results[i] <-
      result$equation_of_time
  }
  
  calculation_status[i] <- result$status
  
}


# =====================================================
# 8. CONVERT UNIX TIMESTAMPS TO POSIXct
# =====================================================

places[, local_noon_datetime_utc := as.POSIXct(
  local_noon_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)]

places[, solar_noon_datetime_utc := as.POSIXct(
  solar_noon_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)]

places[, sunrise_datetime_utc := as.POSIXct(
  sunrise_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)]

places[, sunset_datetime_utc := as.POSIXct(
  sunset_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)]


# =====================================================
# 9. ATTACH ASTRONOMICAL VALUES
# =====================================================

places[, solar_declination_degrees :=
         declination_results]

places[, equation_of_time_minutes :=
         equation_of_time_results]


# =====================================================
# 10. CREATE READABLE UTC TIMESTAMPS
# =====================================================

places[, sunrise_utc_readable := format(
  sunrise_datetime_utc,
  "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)]

places[, sunset_utc_readable := format(
  sunset_datetime_utc,
  "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)]


# =====================================================
# 11. CREATE READABLE LOCAL TIMESTAMPS
# =====================================================

sunrise_local_values <- vapply(
  seq_len(n),
  function(i) {
    
    if (is.na(sunrise_epoch[i])) {
      return(NA_character_)
    }
    
    format(
      places$sunrise_datetime_utc[i],
      "%Y-%m-%d %H:%M:%S",
      tz = places$timezone[i]
    )
  },
  character(1)
)

sunset_local_values <- vapply(
  seq_len(n),
  function(i) {
    
    if (is.na(sunset_epoch[i])) {
      return(NA_character_)
    }
    
    format(
      places$sunset_datetime_utc[i],
      "%Y-%m-%d %H:%M:%S",
      tz = places$timezone[i]
    )
  },
  character(1)
)

places[, sunrise_local := sunrise_local_values]

places[, sunset_local := sunset_local_values]


# =====================================================
# 12. ATTACH CALCULATION STATUS
# =====================================================

places[, calculation_status := calculation_status]


# =====================================================
# 13. SAVE RESULTS
# =====================================================

output_file <- paste0(
  "data/processed/",
  "global_sunrise_sunset_local_dates.csv"
)

fwrite(
  places,
  output_file
)


# =====================================================
# 14. CALCULATION SUMMARY
# =====================================================

cat("\n====================================\n")
cat("LOCAL-DATE-AWARE CALCULATION RESULTS\n")
cat("====================================\n")

cat("Total locations:", nrow(places), "\n")

cat(
  "Successful:",
  sum(places$calculation_status == "Successful"),
  "\n"
)

cat(
  "Sunrise/sunset unavailable:",
  sum(
    places$calculation_status ==
      "Sunrise/sunset unavailable"
  ),
  "\n"
)

cat(
  "Invalid time zones:",
  sum(
    places$calculation_status ==
      "Invalid time zone"
  ),
  "\n"
)

cat(
  "Invalid coordinates:",
  sum(
    places$calculation_status ==
      "Invalid coordinates"
  ),
  "\n"
)

cat(
  "Other calculation errors:",
  sum(
    grepl(
      "^Calculation error:",
      places$calculation_status
    )
  ),
  "\n"
)


# =====================================================
# 15. UTC DATE-BOUNDARY VALIDATION
# =====================================================

places[, solar_noon_difference_hours := as.numeric(
  difftime(
    solar_noon_datetime_utc,
    local_noon_datetime_utc,
    units = "hours"
  )
)]

date_boundary_suspects <- places[
  !is.na(solar_noon_difference_hours) &
    abs(solar_noon_difference_hours) > 12
]

cat("\n====================================\n")
cat("UTC DATE-BOUNDARY VALIDATION\n")
cat("====================================\n")

cat(
  "Potential date-boundary problems:",
  nrow(date_boundary_suspects),
  "\n"
)

if (nrow(date_boundary_suspects) > 0) {
  
  print(
    date_boundary_suspects[
      ,
      .(
        name,
        country_code,
        timezone,
        local_noon_datetime_utc,
        solar_noon_datetime_utc,
        solar_noon_difference_hours
      )
    ][1:min(20, .N)]
  )
  
} else {
  
  cat("No locations exceeded the 12-hour check.\n")
  
}


# =====================================================
# 16. DISPLAY FIRST 10 RESULTS
# =====================================================

cat("\nFIRST 10 RESULTS\n")

print(
  places[
    1:min(10, .N),
    .(
      name,
      country_code,
      latitude,
      longitude,
      timezone,
      sunrise_utc_readable,
      sunrise_local,
      sunset_utc_readable,
      sunset_local,
      calculation_status
    )
  ]
)


# =====================================================
# 17. FINAL DATA QUALITY CHECK
# =====================================================

cat("\n====================================\n")
cat("DATA QUALITY CHECK\n")
cat("====================================\n")

cat(
  "Missing sunrise timestamps:",
  sum(is.na(places$sunrise_datetime_utc)),
  "\n"
)

cat(
  "Missing sunset timestamps:",
  sum(is.na(places$sunset_datetime_utc)),
  "\n"
)

cat(
  "Missing local sunrise:",
  sum(is.na(places$sunrise_local)),
  "\n"
)

cat(
  "Missing local sunset:",
  sum(is.na(places$sunset_local)),
  "\n"
)


# =====================================================
# 18. COMPLETION
# =====================================================

cat("\n====================================\n")
cat("SCRIPT 24 COMPLETED\n")
cat("====================================\n")

cat("Saved file:", output_file, "\n")
