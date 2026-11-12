
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 21: CALCULATE SUNRISE AND SUNSET
# =====================================================

# Load previous astronomy functions
source("R/solar_position.R")
source("R/equation_of_time.R")
source("R/solar_noon.R")
source("R/prayer_config.R")


# Create sunrise and sunset function
calculate_sunrise_sunset <- function(
    datetime_utc,
    latitude,
    longitude
) {
  
  # Calculate solar declination
  declination <- calculate_solar_declination(
    datetime_utc
  )
  
  # Convert degrees to radians
  lat_rad <- latitude * pi / 180
  dec_rad <- declination * pi / 180
  zenith_rad <- prayer_config$sunrise_sunset_zenith *
    pi / 180
  
  # Calculate cosine of solar hour angle
  cos_hour_angle <- (
    cos(zenith_rad) -
      sin(lat_rad) * sin(dec_rad)
  ) / (
    cos(lat_rad) * cos(dec_rad)
  )
  
  # Check whether sunrise and sunset are possible
  if (cos_hour_angle < -1 ||
      cos_hour_angle > 1) {
    
    return(list(
      sunrise_utc_minutes = NA_real_,
      sunset_utc_minutes = NA_real_,
      message = "Sunrise or sunset is not available"
    ))
  }
  
  # Calculate hour angle in degrees
  hour_angle <- acos(cos_hour_angle) *
    180 / pi
  
  # Calculate solar noon
  solar_noon <- calculate_solar_noon(
    datetime_utc,
    longitude
  )
  
  # Calculate sunrise and sunset
  sunrise <- solar_noon - 4 * hour_angle
  sunset <- solar_noon + 4 * hour_angle
  
  return(list(
    sunrise_utc_minutes = sunrise,
    sunset_utc_minutes = sunset,
    message = "Calculation successful"
  ))
}


# =====================================================
# TEST WITH LAGOS, NIGERIA
# =====================================================

test_date <- as.POSIXct(
  "2026-06-21 12:00:00",
  tz = "UTC"
)

lagos_result <- calculate_sunrise_sunset(
  datetime_utc = test_date,
  latitude = 6.52,
  longitude = 3.38
)


# Convert minutes into readable UTC times
format_utc_time <- function(minutes) {
  
  total_seconds <- round(minutes * 60)
  
  hours <- floor(total_seconds / 3600)
  remaining_seconds <- total_seconds %% 3600
  
  minutes_part <- floor(remaining_seconds / 60)
  
  sprintf(
    "%02d:%02d UTC",
    hours,
    minutes_part
  )
}


# Display results
cat("\n====================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SUNRISE AND SUNSET TEST\n")
cat("====================================\n")

cat("Location: Lagos, Nigeria\n")
cat("Date: June 21, 2026\n")

cat(
  "Sunrise:",
  format_utc_time(
    lagos_result$sunrise_utc_minutes
  ),
  "\n"
)

cat(
  "Sunset:",
  format_utc_time(
    lagos_result$sunset_utc_minutes
  ),
  "\n"
)

cat(
  "Status:",
  lagos_result$message,
  "\n"
)
