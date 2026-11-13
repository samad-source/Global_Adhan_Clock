
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 41A: PRAYER-TIME ANOMALY INVESTIGATION
#
# PURPOSE:
# Investigate large differences between our astronomical
# prayer-time engine and the AlAdhan reference results.
#
# IMPORTANT:
# No new API requests.
# No changes to the astronomical engine.
# ============================================================

library(data.table)

cat("\n")
cat("====================================================\n")
cat("GLOBAL ADHAN CLOCK - SCRIPT 41A\n")
cat("PRAYER-TIME ANOMALY INVESTIGATION\n")
cat("====================================================\n\n")

# ------------------------------------------------------------
# 1. FILE LOCATIONS
# ------------------------------------------------------------

input_file <- file.path(
  "data",
  "processed",
  "astronomical_validation",
  "script41_all_validation_results.csv"
)

output_directory <- file.path(
  "data",
  "processed",
  "astronomical_validation",
  "anomaly_investigation"
)

if (!file.exists(input_file)) {
  stop(
    "Validation results not found: ",
    input_file,
    "\nRun Script 41 first."
  )
}

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 2. LOAD PREVIOUSLY SAVED RESULTS
# ------------------------------------------------------------

validation_data <- fread(input_file)

cat("DATA LOADED\n")
cat("Total comparison records:", nrow(validation_data), "\n")

required_columns <- c(
  "geoname_id",
  "place_name",
  "country_code",
  "latitude",
  "longitude",
  "latitude_band",
  "timezone",
  "season",
  "research_date",
  "prayer",
  "our_time_local",
  "api_time_local",
  "our_minutes",
  "api_minutes",
  "difference_minutes",
  "comparison_status"
)

missing_columns <- setdiff(
  required_columns,
  names(validation_data)
)

if (length(missing_columns) > 0) {
  stop(
    "Required columns missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 3. SEPARATE COMPARABLE AND UNAVAILABLE RECORDS
# ------------------------------------------------------------

comparable_data <- validation_data[
  comparison_status == "Comparable" &
    !is.na(difference_minutes)
]

unavailable_data <- validation_data[
  comparison_status != "Comparable"
]

cat("\nCOMPARISON QUALITY\n")
cat("Comparable records:", nrow(comparable_data), "\n")
cat("Unavailable records:", nrow(unavailable_data), "\n")

# ------------------------------------------------------------
# 4. CALCULATE SIGNED TIME DIFFERENCES
#
# Positive = our calculated time is later.
# Negative = our calculated time is earlier.
#
# Circular calculation handles midnight correctly.
# ------------------------------------------------------------

comparable_data[
  ,
  signed_difference_minutes :=
    ((our_minutes - api_minutes + 720) %% 1440) - 720
]

# ------------------------------------------------------------
# 5. IDENTIFY ALL DIFFERENCES ABOVE 2 MINUTES
# ------------------------------------------------------------

anomalies <- comparable_data[
  difference_minutes > 2
]

setorder(
  anomalies,
  -difference_minutes
)

fwrite(
  anomalies,
  file.path(
    output_directory,
    "script41A_all_differences_above_2_minutes.csv"
  )
)

cat("\nANOMALIES ABOVE 2 MINUTES\n")
cat("Records:", nrow(anomalies), "\n")

# ------------------------------------------------------------
# 6. IDENTIFY SEVERE DIFFERENCES
# ------------------------------------------------------------

severe_anomalies <- comparable_data[
  difference_minutes >= 10
]

setorder(
  severe_anomalies,
  -difference_minutes
)

fwrite(
  severe_anomalies,
  file.path(
    output_directory,
    "script41A_severe_differences_10_minutes_or_more.csv"
  )
)

cat("\nSEVERE ANOMALIES: 10 MINUTES OR MORE\n")
cat("Records:", nrow(severe_anomalies), "\n")

# ------------------------------------------------------------
# 7. SUMMARIZE ANOMALIES BY PRAYER
# ------------------------------------------------------------

prayer_anomaly_summary <- comparable_data[
  ,
  .(
    comparisons = .N,
    differences_above_2_minutes =
      sum(difference_minutes > 2),
    differences_10_minutes_or_more =
      sum(difference_minutes >= 10),
    mean_difference =
      mean(difference_minutes),
    maximum_difference =
      max(difference_minutes)
  ),
  by = prayer
]

prayer_order <- c(
  "Fajr",
  "Dhuhr",
  "Asr",
  "Maghrib",
  "Isha"
)

prayer_anomaly_summary[
  ,
  prayer_order := match(prayer, prayer_order)
]

setorder(
  prayer_anomaly_summary,
  prayer_order
)

prayer_anomaly_summary[
  ,
  prayer_order := NULL
]

fwrite(
  prayer_anomaly_summary,
  file.path(
    output_directory,
    "script41A_anomalies_by_prayer.csv"
  )
)

# ------------------------------------------------------------
# 8. SUMMARIZE ANOMALIES BY SEASON
# ------------------------------------------------------------

season_anomaly_summary <- comparable_data[
  ,
  .(
    comparisons = .N,
    differences_above_2_minutes =
      sum(difference_minutes > 2),
    differences_10_minutes_or_more =
      sum(difference_minutes >= 10),
    mean_difference =
      mean(difference_minutes),
    maximum_difference =
      max(difference_minutes)
  ),
  by = season
]

fwrite(
  season_anomaly_summary,
  file.path(
    output_directory,
    "script41A_anomalies_by_season.csv"
  )
)

# ------------------------------------------------------------
# 9. INVESTIGATE AFFECTED LOCATIONS
# ------------------------------------------------------------

location_anomaly_summary <- comparable_data[
  ,
  .(
    comparisons = .N,
    differences_above_2_minutes =
      sum(difference_minutes > 2),
    differences_10_minutes_or_more =
      sum(difference_minutes >= 10),
    mean_difference =
      mean(difference_minutes),
    maximum_difference =
      max(difference_minutes)
  ),
  by = .(
    geoname_id,
    place_name,
    country_code,
    latitude,
    longitude,
    latitude_band,
    timezone
  )
]

location_anomaly_summary[
  ,
  anomaly_percentage :=
    100 * differences_above_2_minutes / comparisons
]

setorder(
  location_anomaly_summary,
  -maximum_difference
)

fwrite(
  location_anomaly_summary,
  file.path(
    output_directory,
    "script41A_anomalies_by_location.csv"
  )
)

# ------------------------------------------------------------
# 10. INVESTIGATE LOCAL DATE BOUNDARIES
#
# Our time contains a full local date.
# Check whether that date differs from the research date.
# This is a diagnostic flag, not proof of an error.
# ------------------------------------------------------------

comparable_data[
  ,
  our_local_date :=
    as.Date(substr(our_time_local, 1, 10))
]

comparable_data[
  ,
  research_date_parsed :=
    as.Date(research_date)
]

comparable_data[
  ,
  local_date_differs :=
    our_local_date != research_date_parsed
]

date_boundary_summary <- comparable_data[
  ,
  .(
    comparisons = .N,
    date_differs = sum(local_date_differs, na.rm = TRUE)
  ),
  by = .(
    prayer,
    season
  )
]

fwrite(
  date_boundary_summary,
  file.path(
    output_directory,
    "script41A_local_date_boundary_summary.csv"
  )
)

date_boundary_records <- comparable_data[
  local_date_differs == TRUE
]

fwrite(
  date_boundary_records,
  file.path(
    output_directory,
    "script41A_local_date_boundary_records.csv"
  )
)

# ------------------------------------------------------------
# 11. SAVE ALL UNAVAILABLE PRAYER RECORDS
# ------------------------------------------------------------

setorder(
  unavailable_data,
  latitude_band,
  season,
  place_name,
  prayer
)

fwrite(
  unavailable_data,
  file.path(
    output_directory,
    "script41A_unavailable_prayer_records.csv"
  )
)

unavailable_summary <- unavailable_data[
  ,
  .(
    records = .N,
    affected_places = uniqueN(geoname_id)
  ),
  by = .(
    latitude_band,
    season,
    prayer,
    comparison_status
  )
]

fwrite(
  unavailable_summary,
  file.path(
    output_directory,
    "script41A_unavailable_prayer_summary.csv"
  )
)

# ------------------------------------------------------------
# 12. DISPLAY THE MOST IMPORTANT RESULTS
# ------------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("ANOMALY INVESTIGATION RESULTS\n")
cat("====================================================\n")

cat("\n1. PRAYER ANOMALY SUMMARY\n")
print(prayer_anomaly_summary)

cat("\n2. SEASONAL ANOMALY SUMMARY\n")
print(season_anomaly_summary)

cat("\n3. SEVERE ANOMALIES\n")

if (nrow(severe_anomalies) > 0) {
  print(
    severe_anomalies[
      ,
      .(
        place_name,
        country_code,
        latitude,
        longitude,
        timezone,
        season,
        research_date,
        prayer,
        our_time_local,
        api_time_local,
        signed_difference_minutes,
        difference_minutes
      )
    ]
  )
} else {
  cat("No differences of 10 minutes or more.\n")
}

cat("\n4. LOCATION SUMMARY\n")

print(
  location_anomaly_summary[
    differences_above_2_minutes > 0
  ][1:min(20, .N)]
)

cat("\n5. LOCAL DATE BOUNDARY SUMMARY\n")
print(date_boundary_summary)

cat("\n6. UNAVAILABLE PRAYER RECORDS\n")
print(unavailable_summary)

# ------------------------------------------------------------
# 13. FINAL REPORT
# ------------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("SCRIPT 41A COMPLETED\n")
cat("====================================================\n")

cat("Original comparison records:", nrow(validation_data), "\n")
cat("Comparable records:", nrow(comparable_data), "\n")
cat("Unavailable records:", nrow(unavailable_data), "\n")
cat("Differences above 2 minutes:", nrow(anomalies), "\n")
cat("Differences of 10 minutes or more:",
    nrow(severe_anomalies), "\n")

cat("\nOUTPUT DIRECTORY:\n")
cat(output_directory, "\n")

cat("\nNo API requests were made.\n")
cat("No astronomical formulas were changed.\n")
cat("====================================================\n")
