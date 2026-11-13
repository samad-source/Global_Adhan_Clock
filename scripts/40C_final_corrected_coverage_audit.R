
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 40C: FINAL CORRECTED FOUR-SEASON COVERAGE AUDIT
# ============================================================

library(data.table)

cat("\n============================================\n")
cat("SCRIPT 40C: FINAL CORRECTED COVERAGE AUDIT\n")
cat("============================================\n\n")

# 1. Paths

base_dir <- "data/processed/four_season_experiment"

event_file <- file.path(
  base_dir,
  "four_season_prayer_events_boundary_corrected.csv"
)

output_dir <- file.path(
  base_dir,
  "final_corrected_coverage_audit"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(event_file)) {
  stop("Script 38 corrected event file was not found.")
}

# 2. Load corrected events

events <- fread(event_file)

cat("DATA LOADED\n")
cat("Corrected event records:", nrow(events), "\n")

# 3. Validate required columns

required_columns <- c(
  "season",
  "research_date",
  "geoname_id",
  "local_date",
  "prayer",
  "event_utc"
)

missing_columns <- setdiff(
  required_columns,
  names(events)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# 4. Generate stable event IDs

events[, event_id := paste(
  geoname_id,
  as.character(local_date),
  prayer,
  sep = "_"
)]

cat("\nEVENT ID QUALITY\n")
cat("Unique event IDs:", uniqueN(events$event_id), "\n")
cat("Duplicate event IDs:",
    sum(duplicated(events$event_id)), "\n")

if (anyDuplicated(events$event_id)) {
  stop("Duplicate event IDs detected. Resolve before continuing.")
}

# 5. Parse event timestamps

events[, event_utc_parsed := as.POSIXct(
  as.character(event_utc),
  tz = "UTC"
)]

if (anyNA(events$event_utc_parsed)) {
  stop("Some event timestamps could not be parsed.")
}

events[, event_epoch := as.numeric(event_utc_parsed)]

# 6. Research dates and duration assumptions

season_dates <- unique(
  events[, .(season, research_date)]
)

setorder(season_dates, research_date)

durations <- c(1, 5)

minute_results <- list()
summary_results <- list()
boundary_results <- list()

# 7. Audit each season and duration

for (season_index in seq_len(nrow(season_dates))) {
  
  current_season <- season_dates$season[season_index]
  research_date <- season_dates$research_date[season_index]
  
  day_start_time <- as.POSIXct(
    paste(as.character(research_date), "00:00:00"),
    tz = "UTC"
  )
  
  day_start <- as.numeric(day_start_time)
  day_end <- day_start + 86400
  
  season_events <- events[
    season == current_season
  ]
  
  cat("\n--------------------------------------------\n")
  cat("SEASON:", current_season, "\n")
  cat("RESEARCH DATE:", as.character(research_date), "\n")
  cat("Available event records:", nrow(season_events), "\n")
  cat("--------------------------------------------\n")
  
  for (duration in durations) {
    
    temp <- copy(season_events)
    
    temp[, interval_start := event_epoch]
    temp[, interval_end := event_epoch + duration * 60]
    
    # An interval overlaps the research day if:
    # start < day end AND end > day start.
    
    temp <- temp[
      interval_start < day_end &
        interval_end > day_start
    ]
    
    # Clip intervals to the UTC research day.
    
    temp[, clipped_start := pmax(
      interval_start,
      day_start
    )]
    
    temp[, clipped_end := pmin(
      interval_end,
      day_end
    )]
    
    # Identify first and last UTC minute touched.
    # End-exclusive intervals use ceiling(end) - 1.
    
    temp[, first_minute := floor(
      (clipped_start - day_start) / 60
    )]
    
    temp[, last_minute := ceiling(
      (clipped_end - day_start) / 60
    ) - 1]
    
    # Keep only valid minute indices.
    
    temp <- temp[
      first_minute <= last_minute &
        first_minute >= 0 &
        last_minute <= 1439
    ]
    
    # Expand each interval into the UTC minutes it overlaps.
    
    minute_links <- temp[
      ,
      .(minute_index = seq.int(
        first_minute,
        last_minute
      )),
      by = .(event_id)
    ]
    
    # Count distinct events overlapping each minute.
    
    minute_counts <- minute_links[
      ,
      .(active_events = uniqueN(event_id)),
      by = minute_index
    ]
    
    # Create all 1,440 minutes, including uncovered minutes.
    
    minute_table <- data.table(
      minute_index = 0:1439
    )
    
    minute_table <- merge(
      minute_table,
      minute_counts,
      by = "minute_index",
      all.x = TRUE
    )
    
    minute_table[
      is.na(active_events),
      active_events := 0L
    ]
    
    minute_table[, season := current_season]
    minute_table[, research_date := as.character(research_date)]
    minute_table[, duration_minutes := duration]
    
    minute_table[, utc_time := format(
      day_start_time + minute_index * 60,
      "%Y-%m-%d %H:%M",
      tz = "UTC"
    )]
    
    setcolorder(
      minute_table,
      c(
        "season",
        "research_date",
        "duration_minutes",
        "minute_index",
        "utc_time",
        "active_events"
      )
    )
    
    # Coverage summary.
    
    covered_minutes <- sum(
      minute_table$active_events > 0
    )
    
    uncovered_minutes <- 1440 - covered_minutes
    
    summary_row <- data.table(
      season = current_season,
      research_date = as.character(research_date),
      duration_minutes = duration,
      corrected_dataset_events = nrow(season_events),
      overlapping_events = nrow(temp),
      total_utc_minutes = 1440,
      covered_minutes = covered_minutes,
      uncovered_minutes = uncovered_minutes,
      coverage_percent = covered_minutes / 1440 * 100,
      minimum_active_events = min(minute_table$active_events),
      maximum_active_events = max(minute_table$active_events)
    )
    
    # Boundary diagnostic.
    
    boundary_row <- data.table(
      season = current_season,
      duration_minutes = duration,
      events_starting_before_midnight = sum(
        temp$interval_start < day_start
      ),
      events_overlapping_from_before_midnight = sum(
        temp$interval_start < day_start &
          temp$interval_end > day_start
      ),
      events_starting_inside_utc_day = sum(
        temp$interval_start >= day_start &
          temp$interval_start < day_end
      )
    )
    
    minute_results[[paste(season_index, duration, sep = "_")]] <-
      minute_table
    
    summary_results[[paste(season_index, duration, sep = "_")]] <-
      summary_row
    
    boundary_results[[paste(season_index, duration, sep = "_")]] <-
      boundary_row
    
    cat("\nDuration:", duration, "minute(s)\n")
    cat("Overlapping events:", nrow(temp), "\n")
    cat("Covered minutes:", covered_minutes, "/ 1440\n")
    cat("Uncovered minutes:", uncovered_minutes, "\n")
    cat("Coverage:", round(
      covered_minutes / 1440 * 100,
      4
    ), "%\n")
    cat("Minimum active events:", min(
      minute_table$active_events
    ), "\n")
    cat("Maximum active events:", max(
      minute_table$active_events
    ), "\n")
  }
}

# 8. Combine results

all_minutes <- rbindlist(
  minute_results,
  use.names = TRUE
)

all_summaries <- rbindlist(
  summary_results,
  use.names = TRUE
)

all_boundaries <- rbindlist(
  boundary_results,
  use.names = TRUE
)

# 9. Find weakest UTC minutes

weakest_minutes <- all_minutes[
  ,
  .SD[active_events == min(active_events)],
  by = .(season, duration_minutes)
]

setorder(
  all_summaries,
  research_date,
  duration_minutes
)

# 10. Save outputs

fwrite(
  all_minutes,
  file.path(
    output_dir,
    "script40C_corrected_minute_coverage.csv"
  )
)

fwrite(
  all_summaries,
  file.path(
    output_dir,
    "script40C_corrected_coverage_summary.csv"
  )
)

fwrite(
  all_boundaries,
  file.path(
    output_dir,
    "script40C_boundary_diagnostics.csv"
  )
)

fwrite(
  weakest_minutes,
  file.path(
    output_dir,
    "script40C_weakest_utc_minutes.csv"
  )
)

# 11. Final report

cat("\n============================================\n")
cat("FINAL CORRECTED COVERAGE SUMMARY\n")
cat("============================================\n\n")

print(all_summaries)

cat("\n============================================\n")
cat("UTC BOUNDARY DIAGNOSTICS\n")
cat("============================================\n\n")

print(all_boundaries)

cat("\n============================================\n")
cat("WEAKEST UTC MINUTES\n")
cat("============================================\n\n")

print(weakest_minutes)

cat("\n============================================\n")
cat("FINAL DATASET CHECK\n")
cat("============================================\n")

cat("Input event records:", nrow(events), "\n")
cat("Unique event IDs:", uniqueN(events$event_id), "\n")
cat("Duplicate event IDs:", sum(duplicated(events$event_id)), "\n")
cat("Minute records:", nrow(all_minutes), "\n")

cat("\nFiles saved in:\n")
cat(output_dir, "\n")

cat("\n============================================\n")
cat("SCRIPT 40C COMPLETED\n")
cat("============================================\n")
