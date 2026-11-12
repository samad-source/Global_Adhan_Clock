# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 18: CALCULATE SOLAR DECLINATION
# =====================================================


# 1. Create a solar declination function

calculate_solar_declination <- function(datetime_utc) {
  
  # Extract date and time information
  
  date_parts <- as.POSIXlt(
    datetime_utc,
    tz = "UTC"
  )
  
  day_of_year <- date_parts$yday + 1
  
  hour_utc <- date_parts$hour
  
  minute_utc <- date_parts$min
  
  second_utc <- date_parts$sec
  
  
  # Calculate the number of days in the year
  
  year <- date_parts$year + 1900
  
  days_in_year <- ifelse(
    ((year %% 4 == 0) & (year %% 100 != 0)) |
      (year %% 400 == 0),
    366,
    365
  )
  
  
  # 2. Calculate fractional year angle
  
  decimal_hour <- hour_utc +
    minute_utc / 60 +
    second_utc / 3600
  
  gamma <- (
    2 * pi / days_in_year
  ) * (
    day_of_year - 1 +
      (decimal_hour - 12) / 24
  )
  
  
  # 3. Calculate solar declination in radians
  
  declination <- (
    0.006918 -
      0.399912 * cos(gamma) +
      0.070257 * sin(gamma) -
      0.006758 * cos(2 * gamma) +
      0.000907 * sin(2 * gamma) -
      0.002697 * cos(3 * gamma) +
      0.001480 * sin(3 * gamma)
  )
  
  
  # 4. Convert radians into degrees
  
  declination_degrees <- declination * 180 / pi
  
  
  # 5. Return the result
  
  return(declination_degrees)
  
}


# =====================================================
# TEST OUR FUNCTION
# =====================================================


# June solstice test

june_date <- as.POSIXct(
  "2026-06-21 12:00:00",
  tz = "UTC"
)


# December solstice test

december_date <- as.POSIXct(
  "2026-12-21 12:00:00",
  tz = "UTC"
)


june_declination <- calculate_solar_declination(
  june_date
)

december_declination <- calculate_solar_declination(
  december_date
)


# Display results

cat("\n====================================\n")

cat("GLOBAL ADHAN CLOCK\n")

cat("SOLAR DECLINATION TEST\n")

cat("====================================\n")

cat(
  "June 21 solar declination:",
  round(june_declination, 3),
  "degrees\n"
)

cat(
  "December 21 solar declination:",
  round(december_declination, 3),
  "degrees\n"
)