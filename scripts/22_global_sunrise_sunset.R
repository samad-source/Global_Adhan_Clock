
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 22: GLOBAL SUNRISE AND SUNSET
# USING REAL GEONAMES DATA
# =====================================================


# 1. Load required packages and functions

library(data.table)

source("R/solar_position.R")
source("R/equation_of_time.R")
source("R/prayer_config.R")


# 2. Define our research date

research_date <- "2026-11-10"

datetime_utc <- as.POSIXct(
  paste(research_date, "12:00:00"),
  tz = "UTC"
)

day_start_utc <- as.POSIXct(
  paste(research_date, "00:00:00"),
  tz = "UTC"
)


# 3. Read real geographical data

cat("\n====================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("LOADING REAL GEOGRAPHICAL DATA\n")
cat("====================================\n")

places <- fread(
  "data/processed/application_places.csv",
  select = c(
    "geoname_id",
    "name",
    "country_code",
    "latitude",
    "longitude",
    "population",
    "feature_class",
    "timezone",
    "clock_eligible"
  )
)

cat("Total records loaded:", nrow(places), "\n")


# 4. Select major populated places

major_places <- places[
  feature_class == "P" &
    population >= 100000 &
    !is.na(timezone) &
    clock_eligible == TRUE &
    !is.na(latitude) &
    !is.na(longitude)
]

cat(
  "Major populated places selected:",
  nrow(major_places),
  "\n"
)


# 5. Calculate solar declination

declination <- calculate_solar_declination(
  datetime_utc
)

declination_rad <- declination * pi / 180


# 6. Calculate equation of time

equation_of_time <- calculate_equation_of_time(
  datetime_utc
)


# 7. Convert geographical coordinates to radians

latitude_rad <- major_places$latitude * pi / 180

zenith_rad <- prayer_config$sunrise_sunset_zenith *
  pi / 180


# 8. Calculate solar hour angle

cos_hour_angle <- (
  cos(zenith_rad) -
    sin(latitude_rad) * sin(declination_rad)
) / (
  cos(latitude_rad) * cos(declination_rad)
)


# 9. Identify locations with calculable sunrise and sunset

valid_locations <- (
  is.finite(cos_hour_angle) &
    cos_hour_angle >= -1 &
    cos_hour_angle <= 1
)

hour_angle <- rep(
  NA_real_,
  nrow(major_places)
)

hour_angle[valid_locations] <- acos(
  cos_hour_angle[valid_locations]
) * 180 / pi


# 10. Calculate solar noon in UTC minutes

solar_noon_minutes <- (
  720 -
    4 * major_places$longitude -
    equation_of_time
)


# 11. Calculate sunrise and sunset

major_places[, solar_noon_utc_minutes :=
               solar_noon_minutes
]

major_places[, sunrise_utc_minutes :=
               solar_noon_minutes - 4 * hour_angle
]

major_places[, sunset_utc_minutes :=
               solar_noon_minutes + 4 * hour_angle
]


# 12. Add readable UTC times

format_utc_time <- function(minutes) {
  
  result <- rep(
    NA_character_,
    length(minutes)
  )
  
  valid <- !is.na(minutes) & is.finite(minutes)
  
  timestamps <- day_start_utc +
    minutes[valid] * 60
  
  result[valid] <- format(
    timestamps,
    format = "%H:%M",
    tz = "UTC"
  )
  
  return(result)
}


major_places[, sunrise_utc :=
               format_utc_time(sunrise_utc_minutes)
]

major_places[, sunset_utc :=
               format_utc_time(sunset_utc_minutes)
]


# 13. Add calculation status

major_places[, calculation_status :=
               ifelse(
                 valid_locations,
                 "Successful",
                 "Sunrise/sunset unavailable"
               )
]


# 14. Select final output columns

global_sunrise_sunset <- major_places[, .(
  geoname_id,
  name,
  country_code,
  latitude,
  longitude,
  population,
  timezone,
  solar_noon_utc_minutes,
  sunrise_utc_minutes,
  sunset_utc_minutes,
  sunrise_utc,
  sunset_utc,
  calculation_status
)]


# 15. Create output folder if necessary

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)


# 16. Save results

fwrite(
  global_sunrise_sunset,
  "data/processed/global_sunrise_sunset.csv"
)


# 17. Display research summary

cat("\n====================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("GLOBAL SUNRISE AND SUNSET RESULTS\n")
cat("====================================\n")

cat("Research date:", research_date, "\n")

cat(
  "Locations selected:",
  nrow(global_sunrise_sunset),
  "\n"
)

cat(
  "Successful calculations:",
  sum(valid_locations),
  "\n"
)

cat(
  "Unavailable calculations:",
  sum(!valid_locations),
  "\n"
)

cat(
  "Results saved to:",
  "data/processed/global_sunrise_sunset.csv",
  "\n"
)


# 18. Display first 10 results

cat("\nFIRST 10 LOCATIONS\n")

print(
  global_sunrise_sunset[
    1:min(10, .N),
    .(
      name,
      country_code,
      latitude,
      longitude,
      sunrise_utc,
      sunset_utc,
      calculation_status
    )
  ]
)

cat("\nSCRIPT 22 COMPLETED\n")
