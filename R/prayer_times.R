
# ============================================================
# GLOBAL ADHAN CLOCK
# PRAYER-TIME CALCULATION ENGINE
# File: R/prayer_times.R
# ============================================================


# 1. Load our existing astronomy functions

source("R/solar_position.R")
source("R/equation_of_time.R")
source("R/prayer_config.R")


# 2. Convert solar altitude into an hour angle
#
# solar_altitude is measured in degrees.
# Negative values mean the Sun is below the horizon.

calculate_hour_angle <- function(
    latitude,
    declination,
    solar_altitude
) {
  
  lat_rad <- latitude * pi / 180
  
  dec_rad <- declination * pi / 180
  
  altitude_rad <- solar_altitude * pi / 180
  
  
  numerator <- sin(altitude_rad) -
    sin(lat_rad) * sin(dec_rad)
  
  denominator <- cos(lat_rad) * cos(dec_rad)
  
  
  if (abs(denominator) < 1e-10) {
    return(NA_real_)
  }
  
  
  cos_hour_angle <- numerator / denominator
  
  
  # The Sun does not reach the requested altitude
  # when the cosine value is outside this range.
  
  if (
    cos_hour_angle < -1 - 1e-10 ||
    cos_hour_angle > 1 + 1e-10
  ) {
    return(NA_real_)
  }
  
  
  # Protect against tiny floating-point errors.
  
  cos_hour_angle <- max(
    -1,
    min(1, cos_hour_angle)
  )
  
  
  hour_angle_rad <- acos(cos_hour_angle)
  
  
  hour_angle_degrees <- hour_angle_rad * 180 / pi
  
  
  return(hour_angle_degrees)
}



# 3. Calculate the Sun's altitude for Asr

calculate_asr_altitude <- function(
    latitude,
    declination,
    shadow_factor = 1
) {
  
  if (!shadow_factor %in% c(1, 2)) {
    stop("Shadow factor must be 1 or 2.")
  }
  
  
  latitude_rad <- latitude * pi / 180
  
  declination_rad <- declination * pi / 180
  
  
  latitude_difference <- abs(
    latitude_rad - declination_rad
  )
  
  
  shadow_angle <- atan(
    1 / (
      shadow_factor +
        tan(latitude_difference)
    )
  )
  
  
  asr_altitude_degrees <- shadow_angle * 180 / pi
  
  
  return(asr_altitude_degrees)
}



# 4. Convert an absolute UTC timestamp into readable time

format_utc_time <- function(timestamp) {
  
  if (is.na(timestamp)) {
    return(NA_character_)
  }
  
  
  format(
    as.POSIXct(
      timestamp,
      origin = "1970-01-01",
      tz = "UTC"
    ),
    "%Y-%m-%d %H:%M:%S UTC",
    tz = "UTC"
  )
}



# 5. Convert an absolute timestamp into local time

format_local_time <- function(
    timestamp,
    timezone
) {
  
  if (is.na(timestamp)) {
    return(NA_character_)
  }
  
  
  local_datetime <- as.POSIXct(
    timestamp,
    origin = "1970-01-01",
    tz = "UTC"
  )
  
  
  format(
    local_datetime,
    "%Y-%m-%d %H:%M:%S %Z",
    tz = timezone
  )
}



# 6. Main function:
# Calculate all five prayer times for one location.

calculate_prayer_times_for_place <- function(
    research_date,
    latitude,
    longitude,
    timezone,
    config = prayer_config
) {
  
  
  # -------------------------------
  # A. Validate the input
  # -------------------------------
  
  if (!timezone %in% OlsonNames()) {
    stop("Invalid timezone.")
  }
  
  
  if (
    is.na(latitude) ||
    latitude < -90 ||
    latitude > 90
  ) {
    stop("Invalid latitude.")
  }
  
  
  if (
    is.na(longitude) ||
    longitude < -180 ||
    longitude > 180
  ) {
    stop("Invalid longitude.")
  }
  
  
  # -------------------------------
  # B. Find local noon
  # -------------------------------
  
  local_noon <- as.POSIXct(
    paste(research_date, "12:00:00"),
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )
  
  
  if (is.na(local_noon)) {
    stop("Could not determine local noon.")
  }
  
  
  # Convert local noon into its corresponding UTC date.
  
  utc_date <- format(
    local_noon,
    "%Y-%m-%d",
    tz = "UTC"
  )
  
  
  utc_midnight <- as.POSIXct(
    paste(utc_date, "00:00:00"),
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  
  
  local_noon_epoch <- as.numeric(local_noon)
  
  
  # -------------------------------
  # C. Calculate solar position
  # -------------------------------
  
  declination <- calculate_solar_declination(
    local_noon
  )
  
  
  equation_of_time <- calculate_equation_of_time(
    local_noon
  )
  
  
  # Solar noon in minutes from UTC midnight.
  
  solar_noon_minutes <- (
    720 -
      4 * longitude -
      equation_of_time
  )
  
  
  # Convert the calculated solar noon into
  # an absolute UTC timestamp.
  
  solar_noon_epoch <- as.numeric(utc_midnight) +
    solar_noon_minutes * 60
  
  
  # Correct the UTC day if necessary.
  # Solar noon must be the occurrence closest
  # to the local noon instant.
  
  while (
    solar_noon_epoch - local_noon_epoch > 43200
  ) {
    solar_noon_epoch <- solar_noon_epoch - 86400
  }
  
  
  while (
    local_noon_epoch - solar_noon_epoch > 43200
  ) {
    solar_noon_epoch <- solar_noon_epoch + 86400
  }
  
  
  # -------------------------------
  # D. Calculate sunrise and sunset
  # -------------------------------
  
  sunrise_altitude <- 90 - config$sunrise_sunset_zenith
  
  
  sunrise_hour_angle <- calculate_hour_angle(
    latitude,
    declination,
    sunrise_altitude
  )
  
  
  if (is.na(sunrise_hour_angle)) {
    
    sunrise_epoch <- NA_real_
    sunset_epoch <- NA_real_
    
  } else {
    
    sunrise_epoch <- solar_noon_epoch -
      (4 * sunrise_hour_angle * 60)
    
    sunset_epoch <- solar_noon_epoch +
      (4 * sunrise_hour_angle * 60)
    
  }
  
  
  # -------------------------------
  # E. Calculate Fajr
  # -------------------------------
  
  fajr_altitude <- -config$fajr_angle
  
  
  fajr_hour_angle <- calculate_hour_angle(
    latitude,
    declination,
    fajr_altitude
  )
  
  
  fajr_epoch <- if (is.na(fajr_hour_angle)) {
    
    NA_real_
    
  } else {
    
    solar_noon_epoch -
      (4 * fajr_hour_angle * 60)
    
  }
  
  
  # -------------------------------
  # F. Calculate Dhuhr
  # -------------------------------
  
  dhuhr_epoch <- solar_noon_epoch
  
  
  # -------------------------------
  # G. Calculate Asr
  # -------------------------------
  
  asr_altitude <- calculate_asr_altitude(
    latitude,
    declination,
    config$asr_shadow_factor
  )
  
  
  asr_hour_angle <- calculate_hour_angle(
    latitude,
    declination,
    asr_altitude
  )
  
  
  asr_epoch <- if (is.na(asr_hour_angle)) {
    
    NA_real_
    
  } else {
    
    solar_noon_epoch +
      (4 * asr_hour_angle * 60)
    
  }
  
  
  # -------------------------------
  # H. Calculate Maghrib
  # -------------------------------
  
  maghrib_epoch <- sunset_epoch
  
  
  # -------------------------------
  # I. Calculate Isha
  # -------------------------------
  
  isha_altitude <- -config$isha_angle
  
  
  isha_hour_angle <- calculate_hour_angle(
    latitude,
    declination,
    isha_altitude
  )
  
  
  isha_epoch <- if (is.na(isha_hour_angle)) {
    
    NA_real_
    
  } else {
    
    solar_noon_epoch +
      (4 * isha_hour_angle * 60)
    
  }
  
  
  # -------------------------------
  # J. Check event availability
  # -------------------------------
  
  prayer_epochs <- c(
    Fajr = fajr_epoch,
    Dhuhr = dhuhr_epoch,
    Asr = asr_epoch,
    Maghrib = maghrib_epoch,
    Isha = isha_epoch
  )
  
  
  unavailable_prayers <- names(
    prayer_epochs[is.na(prayer_epochs)]
  )
  
  
  prayer_status <- if (
    length(unavailable_prayers) == 0
  ) {
    
    "complete"
    
  } else if (
    length(unavailable_prayers) == 5
  ) {
    
    "unavailable"
    
  } else {
    
    "partial"
    
  }
  
  
  # -------------------------------
  # K. Return the results
  # -------------------------------
  
  result <- list(
    
    solar_declination = declination,
    
    equation_of_time = equation_of_time,
    
    solar_noon_epoch = solar_noon_epoch,
    
    sunrise_epoch = sunrise_epoch,
    
    fajr_epoch = fajr_epoch,
    
    dhuhr_epoch = dhuhr_epoch,
    
    asr_epoch = asr_epoch,
    
    maghrib_epoch = maghrib_epoch,
    
    isha_epoch = isha_epoch,
    
    sunset_epoch = sunset_epoch,
    
    prayer_status = prayer_status,
    
    unavailable_prayers = paste(
      unavailable_prayers,
      collapse = ", "
    )
    
  )
  
  
  # Add readable UTC and local timestamps.
  
  event_names <- c(
    "sunrise",
    "fajr",
    "dhuhr",
    "asr",
    "maghrib",
    "isha",
    "sunset",
    "solar_noon"
  )
  
  
  for (event in event_names) {
    
    timestamp <- result[[paste0(event, "_epoch")]]
    
    
    result[[paste0(event, "_utc")]] <-
      format_utc_time(timestamp)
    
    
    result[[paste0(event, "_local")]] <-
      format_local_time(timestamp, timezone)
    
  }
  
  
  return(result)
  
}
