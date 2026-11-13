
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 39: HIGH-LATITUDE INVESTIGATION
# VERSION 2: GEOGRAPHY JOIN CORRECTION
# ============================================================

library(data.table)
library(ggplot2)

cat("\n============================================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 39: HIGH-LATITUDE INVESTIGATION\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 1. LOAD ASTRONOMY FUNCTIONS
# ------------------------------------------------------------

source("R/solar_position.R")
source("R/equation_of_time.R")
source("R/prayer_config.R")
source("R/prayer_times.R")

# ------------------------------------------------------------
# 2. LOAD SCRIPT 37 RESULTS
# ------------------------------------------------------------

partial_file <- paste0(
  "data/processed/four_season_experiment/",
  "diagnostics/script37_partial_location_details.csv"
)

geography_file <- "data/processed/application_places.csv"

if (!file.exists(partial_file)) {
  stop("Script 37 partial location file not found.")
}

if (!file.exists(geography_file)) {
  stop("Application geography file not found.")
}

partial_data <- fread(partial_file)
geography <- fread(geography_file)

cat("Partial records:", nrow(partial_data), "\n")
cat("Geography records:", nrow(geography), "\n")

# ------------------------------------------------------------
# 3. VERIFY REQUIRED COLUMNS
# ------------------------------------------------------------

required_partial <- c(
  "season",
  "place_name",
  "country_code",
  "latitude",
  "longitude",
  "local_date",
  "unavailable_prayers"
)

required_geography <- c(
  "geoname_id",
  "name",
  "country_code",
  "latitude",
  "longitude",
  "timezone"
)

if (!all(required_partial %in% names(partial_data))) {
  stop("Unexpected columns in Script 37 partial location file.")
}

if (!all(required_geography %in% names(geography))) {
  stop("Required columns missing from application geography file.")
}

# ------------------------------------------------------------
# 4. RESTORE GEOGRAPHICAL IDENTIFIERS AND TIMEZONES
# ------------------------------------------------------------

# Match by name, country and rounded coordinates.
# Coordinate matching prevents ambiguous city-name matches.

partial_data[, latitude_key := round(as.numeric(latitude), 5)]
partial_data[, longitude_key := round(as.numeric(longitude), 5)]

geography[, latitude_key := round(as.numeric(latitude), 5)]
geography[, longitude_key := round(as.numeric(longitude), 5)]

setnames(
  geography,
  "name",
  "place_name"
)

geography_lookup <- unique(
  geography[
    ,
    .(
      place_name,
      country_code,
      latitude_key,
      longitude_key,
      geoname_id,
      timezone
    )
  ]
)

investigation <- merge(
  partial_data,
  geography_lookup,
  by = c(
    "place_name",
    "country_code",
    "latitude_key",
    "longitude_key"
  ),
  all.x = TRUE
)

cat("\nGEOGRAPHY MATCHING\n")
cat("Total records:", nrow(investigation), "\n")
cat("Matched IDs:", sum(!is.na(investigation$geoname_id)), "\n")
cat("Missing IDs:", sum(is.na(investigation$geoname_id)), "\n")
cat("Missing timezones:", sum(is.na(investigation$timezone)), "\n")

if (anyDuplicated(investigation[
  !is.na(geoname_id),
  .(season, geoname_id, local_date)
]) > 0) {
  cat("Warning: repeated location-date combinations exist.\n")
}

# Do not silently discard unmatched locations.
unmatched <- investigation[
  is.na(geoname_id) | is.na(timezone)
]

if (nrow(unmatched) > 0) {
  fwrite(
    unmatched,
    "data/processed/high_latitude_unmatched_locations.csv"
  )
  cat("\nUnmatched locations saved for investigation.\n")
}

investigation <- investigation[
  !is.na(geoname_id) &
    !is.na(timezone) &
    timezone %in% OlsonNames()
]

cat("Usable records:", nrow(investigation), "\n")

# ------------------------------------------------------------
# 5. EXPAND UNAVAILABLE PRAYERS
# ------------------------------------------------------------

investigation[, unavailable_prayers := as.character(unavailable_prayers)]

prayer_records <- investigation[
  ,
  .(
    prayer = trimws(
      unlist(
        strsplit(
          unavailable_prayers,
          "\\s*[,;|]\\s*"
        )
      )
    )
  ),
  by = .(
    season,
    geoname_id,
    place_name,
    country_code,
    latitude,
    longitude,
    timezone,
    local_date
  )
]

prayer_records <- prayer_records[
  !is.na(prayer) &
    prayer != "" &
    tolower(prayer) != "none"
]

prayer_records[, prayer := tools::toTitleCase(tolower(prayer))]

cat("\nUNAVAILABLE PRAYER RECORDS\n")
print(
  prayer_records[
    ,
    .N,
    by = .(season, prayer)
  ][order(season, prayer)]
)

# ------------------------------------------------------------
# 6. SOLAR GEOMETRY FUNCTION
# ------------------------------------------------------------

calculate_solar_geometry <- function(
    latitude,
    declination,
    target_altitude
) {
  
  lat_rad <- latitude * pi / 180
  dec_rad <- declination * pi / 180
  alt_rad <- target_altitude * pi / 180
  
  denominator <- cos(lat_rad) * cos(dec_rad)
  
  if (abs(denominator) < 1e-10) {
    return(list(
      hour_angle_cosine = NA_real_,
      crossing_possible = NA,
      maximum_solar_altitude = NA_real_,
      minimum_solar_altitude = NA_real_
    ))
  }
  
  cos_hour_angle <- (
    sin(alt_rad) -
      sin(lat_rad) * sin(dec_rad)
  ) / denominator
  
  maximum_altitude <- 90 - abs(latitude - declination)
  
  minimum_altitude <- -90 + abs(latitude + declination)
  
  crossing_possible <- (
    target_altitude >= minimum_altitude - 1e-8 &&
      target_altitude <= maximum_altitude + 1e-8
  )
  
  list(
    hour_angle_cosine = cos_hour_angle,
    crossing_possible = crossing_possible,
    maximum_solar_altitude = maximum_altitude,
    minimum_solar_altitude = minimum_altitude
  )
}

# ------------------------------------------------------------
# 7. REQUIRED SOLAR ALTITUDES
# ------------------------------------------------------------

get_target_altitude <- function(prayer, latitude, declination) {
  
  if (prayer == "Fajr") {
    return(-prayer_config$fajr_angle)
  }
  
  if (prayer == "Isha") {
    return(-prayer_config$isha_angle)
  }
  
  if (prayer == "Maghrib") {
    return(90 - prayer_config$sunrise_sunset_zenith)
  }
  
  if (prayer == "Asr") {
    return(
      calculate_asr_altitude(
        latitude,
        declination,
        prayer_config$asr_shadow_factor
      )
    )
  }
  
  NA_real_
}

# ------------------------------------------------------------
# 8. INVESTIGATE SOLAR GEOMETRY
# ------------------------------------------------------------

results <- vector("list", nrow(prayer_records))

for (i in seq_len(nrow(prayer_records))) {
  
  record <- prayer_records[i]
  
  local_noon <- as.POSIXct(
    paste(record$local_date, "12:00:00"),
    tz = record$timezone
  )
  
  if (is.na(local_noon)) next
  
  local_noon_utc <- as.POSIXct(
    format(
      local_noon,
      tz = "UTC",
      format = "%Y-%m-%d %H:%M:%S"
    ),
    tz = "UTC"
  )
  
  declination <- calculate_solar_declination(local_noon_utc)
  
  target_altitude <- get_target_altitude(
    record$prayer,
    record$latitude,
    declination
  )
  
  if (!is.finite(target_altitude)) next
  
  geometry <- calculate_solar_geometry(
    record$latitude,
    declination,
    target_altitude
  )
  
  classification <- if (is.na(geometry$crossing_possible)) {
    "Unable to determine"
  } else if (!geometry$crossing_possible &&
             target_altitude > geometry$maximum_solar_altitude) {
    "Sun never reaches required altitude"
  } else if (!geometry$crossing_possible &&
             target_altitude < geometry$minimum_solar_altitude) {
    "Sun never descends to required altitude"
  } else if (!geometry$crossing_possible) {
    "Required altitude is not crossed"
  } else {
    "Required altitude is crossed"
  }
  
  results[[i]] <- data.table(
    season = record$season,
    geoname_id = record$geoname_id,
    place_name = record$place_name,
    country_code = record$country_code,
    latitude = record$latitude,
    longitude = record$longitude,
    timezone = record$timezone,
    local_date = record$local_date,
    prayer = record$prayer,
    solar_declination = declination,
    target_solar_altitude = target_altitude,
    minimum_solar_altitude = geometry$minimum_solar_altitude,
    maximum_solar_altitude = geometry$maximum_solar_altitude,
    hour_angle_cosine = geometry$hour_angle_cosine,
    crossing_possible = geometry$crossing_possible,
    geometry_classification = classification
  )
  
  if (i %% 100 == 0) {
    cat("Investigated", i, "of", nrow(prayer_records), "\n")
  }
}

geometry_results <- rbindlist(
  results,
  use.names = TRUE,
  fill = TRUE
)

# ------------------------------------------------------------
# 9. RECHECK ORIGINAL PRAYER CALCULATOR
# ------------------------------------------------------------

unique_locations <- unique(
  prayer_records[
    ,
    .(
      season,
      geoname_id,
      place_name,
      latitude,
      longitude,
      timezone,
      local_date
    )
  ]
)

calculator_checks <- vector("list", nrow(unique_locations))

for (i in seq_len(nrow(unique_locations))) {
  
  place <- unique_locations[i]
  
  calculator_checks[[i]] <- tryCatch({
    
    calculated <- calculate_prayer_times_for_place(
      research_date = place$local_date,
      latitude = place$latitude,
      longitude = place$longitude,
      timezone = place$timezone,
      config = prayer_config
    )
    
    data.table(
      geoname_id = place$geoname_id,
      season = place$season,
      local_date = place$local_date,
      prayer_status = calculated$prayer_status,
      unavailable_prayers = paste(
        calculated$unavailable_prayers,
        collapse = ", "
      ),
      calculator_error = NA_character_
    )
    
  }, error = function(e) {
    
    data.table(
      geoname_id = place$geoname_id,
      season = place$season,
      local_date = place$local_date,
      prayer_status = NA_character_,
      unavailable_prayers = NA_character_,
      calculator_error = conditionMessage(e)
    )
    
  })
}

calculator_results <- rbindlist(
  calculator_checks,
  fill = TRUE
)

# ------------------------------------------------------------
# 10. SUMMARY TABLES
# ------------------------------------------------------------

geometry_summary <- geometry_results[
  ,
  .(
    affected_records = .N,
    locations = uniqueN(geoname_id),
    possible_crossings = sum(crossing_possible == TRUE, na.rm = TRUE),
    impossible_crossings = sum(crossing_possible == FALSE, na.rm = TRUE)
  ),
  by = .(season, prayer, geometry_classification)
]

country_summary <- geometry_results[
  ,
  .(
    affected_records = .N,
    locations = uniqueN(geoname_id)
  ),
  by = .(season, country_code, prayer)
][order(season, -affected_records)]

cat("\n============================================\n")
cat("SOLAR GEOMETRY SUMMARY\n")
cat("============================================\n")
print(geometry_summary)

cat("\nAFFECTED COUNTRY SUMMARY\n")
print(head(country_summary, 30))

cat("\nCALCULATOR RECHECK\n")
print(
  calculator_results[
    ,
    .N,
    by = .(season, prayer_status)
  ]
)

# ------------------------------------------------------------
# 11. SAVE OUTPUTS
# ------------------------------------------------------------

output_dir <- "data/processed/high_latitude_investigation"

plot_dir <- "plots/high_latitude_investigation"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

fwrite(
  investigation,
  file.path(output_dir, "high_latitude_investigation_locations.csv")
)

fwrite(
  prayer_records,
  file.path(output_dir, "high_latitude_unavailable_prayers.csv")
)

fwrite(
  geometry_results,
  file.path(output_dir, "high_latitude_solar_geometry.csv")
)

fwrite(
  geometry_summary,
  file.path(output_dir, "high_latitude_geometry_summary.csv")
)

fwrite(
  country_summary,
  file.path(output_dir, "high_latitude_country_summary.csv")
)

fwrite(
  calculator_results,
  file.path(output_dir, "high_latitude_calculator_recheck.csv")
)

# ------------------------------------------------------------
# 12. VISUALIZATIONS
# ------------------------------------------------------------

if (nrow(geometry_results) > 0) {
  
  p1 <- ggplot(
    geometry_results,
    aes(
      x = latitude,
      y = minimum_solar_altitude,
      color = season
    )
  ) +
    geom_point(alpha = 0.65) +
    geom_hline(yintercept = -18, linetype = "dashed") +
    geom_hline(yintercept = -17, linetype = "dotted") +
    labs(
      title = "Minimum Solar Altitude at Affected Locations",
      subtitle = "Comparison with Fajr and Isha angles",
      x = "Latitude (degrees)",
      y = "Minimum solar altitude (degrees)",
      color = "Season"
    ) +
    theme_minimal()
  
  ggsave(
    file.path(plot_dir, "22_high_latitude_minimum_solar_altitude.png"),
    p1,
    width = 12,
    height = 7,
    dpi = 300
  )
  
  p2 <- ggplot(
    geometry_summary,
    aes(
      x = prayer,
      y = affected_records,
      fill = geometry_classification
    )
  ) +
    geom_col(position = "dodge") +
    facet_wrap(~season) +
    labs(
      title = "High-Latitude Solar Geometry Classification",
      x = "Prayer",
      y = "Affected records",
      fill = "Classification"
    ) +
    theme_minimal()
  
  ggsave(
    file.path(plot_dir, "23_high_latitude_geometry_classification.png"),
    p2,
    width = 12,
    height = 7,
    dpi = 300
  )
}

# ------------------------------------------------------------
# 13. FINAL REPORT
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("SCRIPT 39 COMPLETED\n")
cat("============================================================\n")

cat("Original partial records:", nrow(partial_data), "\n")
cat("Matched geography records:", nrow(investigation), "\n")
cat("Unmatched geography records:", nrow(unmatched), "\n")
cat("Unavailable prayer records:", nrow(prayer_records), "\n")
cat("Solar geometry results:", nrow(geometry_results), "\n")
cat("Unique affected locations:",
    uniqueN(geometry_results$geoname_id), "\n")

cat("\nResults saved in:\n")
cat(output_dir, "\n")
cat(plot_dir, "\n")

cat("\nNo missing prayer times were automatically filled.\n")
cat("============================================================\n")
