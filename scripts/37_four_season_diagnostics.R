
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 37: FOUR-SEASON DIAGNOSTIC INVESTIGATION
# CORRECTED VERSION
# ============================================================

library(data.table)

cat("\n")
cat("====================================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 37: FOUR-SEASON DIAGNOSTICS\n")
cat("====================================================\n")


# 1. PROJECT PATHS
# ============================================================

project_root <- getwd()

data_directory <- file.path(
  project_root,
  "data",
  "processed",
  "four_season_experiment"
)

output_directory <- file.path(
  data_directory,
  "diagnostics"
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# 2. LOAD DATA
# ============================================================

minute_coverage <- fread(
  file.path(
    data_directory,
    "four_season_minute_coverage_all_durations.csv"
  )
)

audit <- fread(
  file.path(
    data_directory,
    "four_season_calculation_audit.csv"
  )
)

places <- fread(
  file.path(
    project_root,
    "data",
    "processed",
    "application_places.csv"
  )
)

cat("\nDATA LOADED\n")
cat("Minute records:", nrow(minute_coverage), "\n")
cat("Audit records:", nrow(audit), "\n")
cat("Geographical records:", nrow(places), "\n")


# 3. CHECK REQUIRED COLUMNS
# ============================================================

required_minute <- c(
  "season",
  "duration_minutes",
  "utc_timestamp",
  "active_events",
  "covered"
)

required_audit <- c(
  "season",
  "prayer_status",
  "unavailable_prayers",
  "geoname_id",
  "place_name",
  "country_code",
  "local_date"
)

if (!all(required_minute %in% names(minute_coverage))) {
  stop("Required minute coverage columns are missing.")
}

if (!all(required_audit %in% names(audit))) {
  stop("Required audit columns are missing.")
}


# 4. WEAKEST UTC MINUTES
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PART A: WEAKEST UTC MINUTES\n")
cat("====================================================\n")

one_minute_data <- minute_coverage[
  duration_minutes == 1
]

weakest_results <- list()

for (season_name in unique(one_minute_data$season)) {
  
  cat("\nSEASON:", season_name, "\n")
  
  season_data <- one_minute_data[
    season == season_name
  ]
  
  weakest_support <- min(
    season_data$active_events,
    na.rm = TRUE
  )
  
  weakest_minutes <- season_data[
    active_events == weakest_support
  ]
  
  print(
    weakest_minutes[, .(
      utc_timestamp,
      active_events,
      covered
    )]
  )
  
  weakest_results[[season_name]] <- copy(
    weakest_minutes
  )
}

weakest_table <- rbindlist(
  weakest_results,
  use.names = TRUE,
  fill = TRUE
)

fwrite(
  weakest_table,
  file.path(
    output_directory,
    "script37_weakest_minutes.csv"
  )
)


# 5. SEASONAL COVERAGE SUMMARY
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PART B: SEASONAL COVERAGE SUMMARY\n")
cat("====================================================\n")

coverage_summary <- one_minute_data[
  ,
  .(
    total_minutes = .N,
    covered_minutes = sum(covered, na.rm = TRUE),
    uncovered_minutes = sum(!covered, na.rm = TRUE),
    coverage_percentage = round(
      mean(covered, na.rm = TRUE) * 100,
      4
    ),
    minimum_active_events = min(
      active_events,
      na.rm = TRUE
    ),
    maximum_active_events = max(
      active_events,
      na.rm = TRUE
    )
  ),
  by = season
]

print(coverage_summary)

fwrite(
  coverage_summary,
  file.path(
    output_directory,
    "script37_seasonal_coverage_summary.csv"
  )
)


# 6. PARTIAL CALCULATIONS
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PART C: PARTIAL CALCULATIONS\n")
cat("====================================================\n")

partial_results <- audit[
  !is.na(prayer_status) &
    prayer_status != "complete"
]

partial_by_season <- partial_results[
  ,
  .N,
  by = season
][order(season)]

print(partial_by_season)

fwrite(
  partial_by_season,
  file.path(
    output_directory,
    "script37_partial_calculations_by_season.csv"
  )
)


# 7. UNAVAILABLE PRAYERS
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PART D: UNAVAILABLE PRAYERS\n")
cat("====================================================\n")

unavailable_summary <- partial_results[
  ,
  .N,
  by = .(season, unavailable_prayers)
][order(season, -N)]

print(unavailable_summary)

fwrite(
  unavailable_summary,
  file.path(
    output_directory,
    "script37_unavailable_prayers_summary.csv"
  )
)


# 8. GEOGRAPHICAL INVESTIGATION
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PART E: AFFECTED GEOGRAPHICAL LOCATIONS\n")
cat("====================================================\n")

# Only join geographical columns that are not
# already present in the audit dataset.

geographical_columns <- intersect(
  c("latitude", "longitude"),
  names(places)
)

columns_to_join <- setdiff(
  geographical_columns,
  names(partial_results)
)

place_lookup <- unique(
  places[
    ,
    c("geoname_id", columns_to_join),
    with = FALSE
  ],
  by = "geoname_id"
)

if (length(columns_to_join) > 0) {
  
  partial_locations <- merge(
    partial_results,
    place_lookup,
    by = "geoname_id",
    all.x = TRUE,
    sort = FALSE
  )
  
} else {
  
  partial_locations <- copy(partial_results)
  
}


# Select only columns that actually exist.

desired_columns <- c(
  "season",
  "place_name",
  "country_code",
  "latitude",
  "longitude",
  "local_date",
  "prayer_status",
  "unavailable_prayers"
)

available_columns <- intersect(
  desired_columns,
  names(partial_locations)
)

partial_locations_output <- partial_locations[
  ,
  ..available_columns
]

cat("\nFIRST 20 AFFECTED LOCATIONS\n")

print(
  head(partial_locations_output, 20)
)

fwrite(
  partial_locations_output,
  file.path(
    output_directory,
    "script37_partial_location_details.csv"
  )
)


# 9. PARTIAL CALCULATIONS BY COUNTRY
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PART F: PARTIAL CALCULATIONS BY COUNTRY\n")
cat("====================================================\n")

partial_country_summary <- partial_locations[
  ,
  .(
    partial_calculations = .N,
    affected_places = uniqueN(geoname_id)
  ),
  by = .(season, country_code)
][order(season, -partial_calculations)]

print(
  head(partial_country_summary, 30)
)

fwrite(
  partial_country_summary,
  file.path(
    output_directory,
    "script37_partial_calculations_by_country.csv"
  )
)


# 10. GEOGRAPHICAL DATA QUALITY
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PART G: GEOGRAPHICAL DATA QUALITY\n")
cat("====================================================\n")

if (all(c("latitude", "longitude") %in%
        names(partial_locations))) {
  
  missing_coordinates <- partial_locations[
    is.na(latitude) | is.na(longitude)
  ]
  
  cat(
    "Records with missing coordinates:",
    nrow(missing_coordinates),
    "\n"
  )
  
} else {
  
  cat("Latitude/longitude columns unavailable.\n")
  
}


# 11. FINAL SUMMARY
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PART H: FINAL DIAGNOSTIC SUMMARY\n")
cat("====================================================\n")

cat(
  "Seasons investigated:",
  uniqueN(one_minute_data$season),
  "\n"
)

cat(
  "One-minute coverage records:",
  nrow(one_minute_data),
  "\n"
)

cat(
  "Partial calculation records:",
  nrow(partial_results),
  "\n"
)

cat(
  "Affected unique locations:",
  uniqueN(partial_locations$geoname_id),
  "\n"
)

cat(
  "Weakest minute records:",
  nrow(weakest_table),
  "\n"
)


# 12. OUTPUT FILES
# ============================================================

cat("\nOUTPUT DIRECTORY:\n")
cat(output_directory, "\n")

cat("\nFILES CREATED:\n")

cat("1. script37_weakest_minutes.csv\n")
cat("2. script37_seasonal_coverage_summary.csv\n")
cat("3. script37_partial_calculations_by_season.csv\n")
cat("4. script37_unavailable_prayers_summary.csv\n")
cat("5. script37_partial_location_details.csv\n")
cat("6. script37_partial_calculations_by_country.csv\n")


cat("\n")
cat("====================================================\n")
cat("SCRIPT 37 COMPLETED SUCCESSFULLY\n")
cat("====================================================\n")


# ============================================================
# END OF SCRIPT 37
# ============================================================
