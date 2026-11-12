# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 20: CALCULATE SOLAR NOON
# =====================================================


# Load our previous astronomy functions

source("R/solar_position.R")

source("R/equation_of_time.R")


# Create solar noon function

calculate_solar_noon <- function(
    datetime_utc,
    longitude
) {
  
  equation_of_time <- calculate_equation_of_time(
    datetime_utc
  )
  
  solar_noon_minutes <- (
    720 -
      4 * longitude -
      equation_of_time
  )
  
  return(solar_noon_minutes)
  
}


# Test with Lagos coordinates

test_date <- as.POSIXct(
  "2026-06-21 12:00:00",
  tz = "UTC"
)

lagos_longitude <- 3.38


lagos_solar_noon <- calculate_solar_noon(
  test_date,
  lagos_longitude
)


# Convert minutes into hours and minutes

solar_noon_hour <- floor(
  lagos_solar_noon / 60
)

solar_noon_minute <- round(
  lagos_solar_noon %% 60
)


# Display results

cat("\n====================================\n")

cat("GLOBAL ADHAN CLOCK\n")

cat("SOLAR NOON TEST\n")

cat("====================================\n")

cat("Location: Lagos, Nigeria\n")

cat(
  "Solar noon (UTC):",
  solar_noon_hour,
  "hours",
  solar_noon_minute,
  "minutes\n"
)