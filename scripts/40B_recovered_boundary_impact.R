
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 40B: RECOVERED BOUNDARY-EVENT IMPACT
# ============================================================

library(data.table)

cat("\n============================================\n")
cat("SCRIPT 40B: RECOVERED BOUNDARY-EVENT IMPACT\n")
cat("============================================\n\n")

# 1. File paths

base_dir <- "data/processed/four_season_experiment"

original_file <- file.path(
  base_dir,
  "four_season_prayer_events.csv"
)

recovered_file <- file.path(
  base_dir,
  "four_season_recovered_boundary_events.csv"
)

corrected_file <- file.path(
  base_dir,
  "four_season_prayer_events_boundary_corrected.csv"
)

output_dir <- file.path(
  base_dir,
  "boundary_impact_analysis"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. Check files

stopifnot(
  file.exists(original_file),
  file.exists(recovered_file),
  file.exists(corrected_file)
)

# 3. Load datasets

original <- fread(original_file)
recovered <- fread(recovered_file)
corrected <- fread(corrected_file)

cat("DATA LOADED\n")
cat("Original events:", nrow(original), "\n")
cat("Recovered events:", nrow(recovered), "\n")
cat("Corrected events:", nrow(corrected), "\n\n")

# 4. Check required columns

required_columns <- c(
  "season",
  "research_date",
  "geoname_id",
  "local_date",
  "prayer",
  "event_utc"
)

for (dataset_name in c("original", "recovered", "corrected")) {
  
  dataset <- get(dataset_name)
  
  missing_columns <- setdiff(
    required_columns,
    names(dataset)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste(
        dataset_name,
        "is missing:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
}

# 5. Create stable event IDs

make_event_id <- function(data) {
  
  data[, event_id := paste(
    geoname_id,
    as.character(local_date),
    prayer,
    sep = "_"
  )]
  
  data
}

original <- make_event_id(original)
recovered <- make_event_id(recovered)
corrected <- make_event_id(corrected)

# 6. Confirm every recovered event is in corrected dataset

recovered_ids <- unique(recovered$event_id)
corrected_ids <- unique(corrected$event_id)

missing_from_corrected <- setdiff(
  recovered_ids,
  corrected_ids
)

cat("RECOVERY VERIFICATION\n")
cat("Recovered IDs:", length(recovered_ids), "\n")
cat(
  "Recovered IDs missing from corrected data:",
  length(missing_from_corrected),
  "\n\n"
)

if (length(missing_from_corrected) > 0) {
  stop("Some recovered events are missing from corrected dataset.")
}

# 7. Convert event timestamps to UTC epoch seconds

recovered[, event_utc_parsed := as.POSIXct(
  as.character(event_utc),
  tz = "UTC"
)]

if (anyNA(recovered$event_utc_parsed)) {
  stop("Some recovered event timestamps could not be parsed.")
}

recovered[, event_epoch := as.numeric(event_utc_parsed)]

# 8. Test whether recovered events overlap their research UTC day

durations <- c(1, 5)

impact_results <- list()
overlap_records <- list()

for (duration in durations) {
  
  temp <- copy(recovered)
  
  temp[, day_start := as.POSIXct(
    paste(as.character(research_date), "00:00:00"),
    tz = "UTC"
  )]
  
  temp[, day_end := day_start + 86400]
  
  temp[, interval_end := event_epoch + duration * 60]
  
  # End-exclusive overlap rule:
  # event start < day end AND event end > day start
  
  temp[, overlaps_research_day :=
         event_epoch < as.numeric(day_end) &
         interval_end > as.numeric(day_start)
  ]
  
  # Specifically identify events that begin before UTC midnight
  # and continue into the research UTC day.
  
  temp[, crosses_utc_midnight :=
         event_epoch < as.numeric(day_start) &
         interval_end > as.numeric(day_start)
  ]
  
  temp[, duration_minutes := duration]
  
  overlap_records[[as.character(duration)]] <- temp
  
  summary <- temp[, .(
    recovered_events = .N,
    events_overlapping_day = sum(overlaps_research_day),
    events_crossing_midnight = sum(crosses_utc_midnight),
    events_not_overlapping_day = sum(!overlaps_research_day)
  ), by = season]
  
  summary[, duration_minutes := duration]
  
  impact_results[[as.character(duration)]] <- summary
}

# 9. Combine results

impact_summary <- rbindlist(
  impact_results,
  use.names = TRUE,
  fill = TRUE
)

all_overlap_records <- rbindlist(
  overlap_records,
  use.names = TRUE,
  fill = TRUE
)

setorder(
  impact_summary,
  duration_minutes,
  season
)

# 10. Save detailed outputs

fwrite(
  impact_summary,
  file.path(output_dir, "script40B_boundary_impact_summary.csv")
)

fwrite(
  all_overlap_records,
  file.path(output_dir, "script40B_all_recovered_event_checks.csv")
)

fwrite(
  all_overlap_records[
    crosses_utc_midnight == TRUE
  ],
  file.path(output_dir, "script40B_events_crossing_midnight.csv")
)

# 11. Display results

cat("\n============================================\n")
cat("BOUNDARY-EVENT IMPACT SUMMARY\n")
cat("============================================\n\n")

print(impact_summary)

cat("\n============================================\n")
cat("TOTAL IMPACT BY DURATION\n")
cat("============================================\n\n")

total_summary <- impact_summary[, .(
  recovered_events = sum(recovered_events),
  events_overlapping_day = sum(events_overlapping_day),
  events_crossing_midnight = sum(events_crossing_midnight),
  events_not_overlapping_day = sum(events_not_overlapping_day)
), by = duration_minutes]

print(total_summary)

cat("\n============================================\n")
cat("SAMPLE OF EVENTS CROSSING UTC MIDNIGHT\n")
cat("============================================\n\n")

print(
  all_overlap_records[
    duration_minutes == 5 &
      crosses_utc_midnight == TRUE,
    .(
      season,
      research_date,
      place_name,
      local_date,
      prayer,
      event_utc,
      duration_minutes
    )
  ][1:min(20, .N)]
)

cat("\n============================================\n")
cat("SCRIPT 40B COMPLETED\n")
cat("============================================\n")

cat(
  "\nFiles saved in:",
  output_dir,
  "\n"
)
