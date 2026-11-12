
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 28: BUILD GLOBAL UTC PRAYER-EVENT TIMELINE
# CORRECTED VERSION
# ============================================================

library(data.table)

cat("\n============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("BUILDING GLOBAL UTC PRAYER-EVENT TIMELINE\n")
cat("============================================\n\n")


# 1. Define the UTC research window

research_start <- as.POSIXct(
  "2026-11-10 00:00:00",
  tz = "UTC"
)

research_end <- as.POSIXct(
  "2026-11-17 00:00:00",
  tz = "UTC"
)

cat("UTC research start:",
    format(research_start), "\n")

cat("UTC research end:",
    format(research_end), "\n\n")


# 2. Load date-aware prayer calculations

input_file <- "data/processed/global_prayer_times_date_aware.csv"

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

prayer_data <- fread(input_file)

cat("Location-date records loaded:",
    nrow(prayer_data), "\n\n")


# 3. Define prayer event columns and their order

prayer_columns <- c(
  Fajr = "fajr_epoch",
  Dhuhr = "dhuhr_epoch",
  Asr = "asr_epoch",
  Maghrib = "maghrib_epoch",
  Isha = "isha_epoch"
)


# 4. Extract events inside the UTC research window

event_tables <- list()

for (prayer_name in names(prayer_columns)) {
  
  epoch_column <- prayer_columns[[prayer_name]]
  
  cat("Extracting", prayer_name, "events...\n")
  
  selected <- prayer_data[
    !is.na(get(epoch_column)) &
      get(epoch_column) >= as.numeric(research_start) &
      get(epoch_column) < as.numeric(research_end),
    .(
      geoname_id,
      name,
      country_code,
      latitude,
      longitude,
      timezone,
      local_date,
      event_epoch = get(epoch_column)
    )
  ]
  
  selected[, prayer := prayer_name]
  
  event_tables[[prayer_name]] <- selected
}


# 5. Combine all prayer events

global_events <- rbindlist(
  event_tables,
  use.names = TRUE,
  fill = TRUE
)


# 6. Convert Unix timestamps to UTC

global_events[, event_utc := as.POSIXct(
  event_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)]


# 7. Create readable UTC timestamps

global_events[, event_utc_text := format(
  event_utc,
  "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)]


# 8. Create unique event identifiers

global_events[, event_id := paste(
  geoname_id,
  local_date,
  prayer,
  sep = "_"
)]


# 9. Assign prayer order

global_events[, prayer_order := match(
  prayer,
  names(prayer_columns)
)]


# 10. Sort events chronologically

setorder(
  global_events,
  event_epoch,
  prayer_order
)


# 11. Check duplicate events

duplicate_events <- global_events[
  duplicated(event_id)
]

cat("\n============================================\n")
cat("EVENT QUALITY CHECKS\n")
cat("============================================\n")

cat("Total prayer events:",
    nrow(global_events), "\n")

cat("Duplicate event IDs:",
    nrow(duplicate_events), "\n")

if (nrow(duplicate_events) > 0) {
  warning("Duplicate event identifiers detected.")
}


# 12. Summarise events by prayer

event_summary <- global_events[
  ,
  .(
    event_count = .N,
    first_event_utc = min(event_utc),
    last_event_utc = max(event_utc)
  ),
  by = prayer
]


# Sort summary in the correct prayer order

event_summary[, prayer_order := match(
  prayer,
  names(prayer_columns)
)]

setorder(event_summary, prayer_order)

event_summary[, prayer_order := NULL]


# 13. Display event summary

cat("\n============================================\n")
cat("GLOBAL PRAYER EVENT SUMMARY\n")
cat("============================================\n")

print(event_summary)


# 14. Display first 20 chronological events

cat("\n============================================\n")
cat("FIRST 20 GLOBAL UTC EVENTS\n")
cat("============================================\n")

print(
  global_events[
    1:min(20, .N),
    .(
      name,
      country_code,
      prayer,
      local_date,
      event_utc_text,
      timezone
    )
  ]
)


# 15. Save global event timeline

output_file <- "data/processed/global_prayer_events_utc.csv"

fwrite(
  global_events,
  output_file
)


# 16. Save event summary

summary_file <- "data/processed/global_prayer_events_summary.csv"

fwrite(
  event_summary,
  summary_file
)


# 17. Final report

cat("\n============================================\n")
cat("SCRIPT 28 COMPLETED\n")
cat("============================================\n")

cat("Total events:",
    nrow(global_events), "\n")

cat("Duplicate events:",
    nrow(duplicate_events), "\n")

cat("\nEvent timeline saved to:\n")
cat(output_file, "\n")

cat("\nEvent summary saved to:\n")
cat(summary_file, "\n")

cat("\nIMPORTANT SCIENTIFIC NOTE:\n")
cat("These are calculated prayer-time events.\n")
cat("They are NOT verified actual Adhan broadcasts.\n")
