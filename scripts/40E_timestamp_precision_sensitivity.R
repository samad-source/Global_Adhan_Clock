
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 40E: TIMESTAMP PRECISION SENSITIVITY
# ============================================================

library(data.table)

cat("\n============================================\n")
cat("SCRIPT 40E: TIMESTAMP PRECISION SENSITIVITY\n")
cat("============================================\n\n")

# 1. File paths

base_dir <- "data/processed/four_season_experiment"

event_file <- file.path(
  base_dir,
  "four_season_prayer_events_boundary_corrected.csv"
)

output_dir <- file.path(
  base_dir,
  "timestamp_precision_sensitivity"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(event_file)) {
  stop("Corrected event dataset not found.")
}

# 2. Load data

events <- fread(event_file)

cat("DATA LOADED\n")
cat("Event records:", nrow(events), "\n")

# 3. Validate data

required <- c(
  "season",
  "research_date",
  "geoname_id",
  "local_date",
  "prayer",
  "event_utc"
)

missing <- setdiff(required, names(events))

if (length(missing) > 0) {
  stop(paste("Missing columns:", paste(missing, collapse = ", ")))
}

events[, event_id := paste(
  geoname_id,
  as.character(local_date),
  prayer,
  sep = "_"
)]

if (anyDuplicated(events$event_id)) {
  stop("Duplicate event IDs detected.")
}

events[, event_epoch_raw := as.numeric(
  as.POSIXct(event_utc, tz = "UTC")
)]

if (anyNA(events$event_epoch_raw)) {
  stop("Invalid event timestamps detected.")
}

# 4. Define timestamp precision scenarios

precision_modes <- c(
  "raw",
  "floor_second",
  "nearest_second",
  "ceiling_second"
)

apply_precision <- function(epoch, mode) {
  
  if (mode == "raw") {
    return(epoch)
  }
  
  if (mode == "floor_second") {
    return(floor(epoch))
  }
  
  if (mode == "nearest_second") {
    return(round(epoch))
  }
  
  if (mode == "ceiling_second") {
    return(ceiling(epoch))
  }
  
  stop("Unknown precision mode.")
}

# 5. Prepare research dates

season_dates <- unique(
  events[, .(season, research_date)]
)

setorder(season_dates, research_date)

durations <- c(1, 5)

summary_results <- list()
minute_results <- list()
boundary_results <- list()

result_index <- 1

# 6. Run all precision scenarios

for (season_index in seq_len(nrow(season_dates))) {
  
  current_season <- season_dates$season[season_index]
  current_date <- season_dates$research_date[season_index]
  
  day_start_time <- as.POSIXct(
    paste(as.character(current_date), "00:00:00"),
    tz = "UTC"
  )
  
  day_start <- as.numeric(day_start_time)
  day_end <- day_start + 86400
  
  season_events <- events[
    season == current_season
  ]
  
  for (precision_mode in precision_modes) {
    
    temp_events <- copy(season_events)
    
    temp_events[, adjusted_epoch := apply_precision(
      event_epoch_raw,
      precision_mode
    )]
    
    for (duration in durations) {
      
      temp <- copy(temp_events)
      
      temp[, interval_start := adjusted_epoch]
      temp[, interval_end := adjusted_epoch + duration * 60]
      
      # End-exclusive overlap test.
      
      temp[, overlaps_day :=
             interval_start < day_end &
             interval_end > day_start
      ]
      
      # Events from before UTC midnight that enter the day.
      
      temp[, crosses_midnight :=
             interval_start < day_start &
             interval_end > day_start
      ]
      
      overlapping <- temp[
        overlaps_day == TRUE
      ]
      
      # Clip events to the UTC day.
      
      overlapping[, clipped_start := pmax(
        interval_start,
        day_start
      )]
      
      overlapping[, clipped_end := pmin(
        interval_end,
        day_end
      )]
      
      # Calculate first and last touched minute.
      # End-exclusive intervals.
      
      overlapping[, first_minute := floor(
        (clipped_start - day_start) / 60
      )]
      
      overlapping[, last_minute := ceiling(
        (clipped_end - day_start) / 60
      ) - 1]
      
      overlapping <- overlapping[
        first_minute <= last_minute &
          first_minute >= 0 &
          last_minute <= 1439
      ]
      
      # Expand events into minute bins.
      
      minute_links <- overlapping[
        ,
        .(minute_index = seq.int(
          first_minute,
          last_minute
        )),
        by = event_id
      ]
      
      minute_counts <- minute_links[
        ,
        .(active_events = uniqueN(event_id)),
        by = minute_index
      ]
      
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
      
      covered <- sum(minute_table$active_events > 0)
      
      # Summary row.
      
      summary_row <- data.table(
        season = current_season,
        research_date = as.character(current_date),
        precision_mode = precision_mode,
        duration_minutes = duration,
        total_events = nrow(temp),
        overlapping_events = nrow(overlapping),
        boundary_events = sum(temp$crosses_midnight),
        total_utc_minutes = 1440,
        covered_minutes = covered,
        uncovered_minutes = 1440 - covered,
        coverage_percent = covered / 1440 * 100,
        minimum_active_events = min(minute_table$active_events),
        maximum_active_events = max(minute_table$active_events)
      )
      
      minute_table[, season := current_season]
      minute_table[, research_date := as.character(current_date)]
      minute_table[, precision_mode := precision_mode]
      minute_table[, duration_minutes := duration]
      
      boundary_row <- data.table(
        season = current_season,
        precision_mode = precision_mode,
        duration_minutes = duration,
        events_before_midnight = sum(
          temp$adjusted_epoch < day_start
        ),
        events_crossing_midnight = sum(temp$crosses_midnight),
        events_not_crossing_midnight = sum(
          temp$adjusted_epoch < day_start &
            !temp$crosses_midnight
        )
      )
      
      summary_results[[result_index]] <- summary_row
      minute_results[[result_index]] <- minute_table
      boundary_results[[result_index]] <- boundary_row
      
      result_index <- result_index + 1
    }
  }
}

# 7. Combine outputs

all_summaries <- rbindlist(
  summary_results,
  use.names = TRUE
)

all_minutes <- rbindlist(
  minute_results,
  use.names = TRUE
)

all_boundaries <- rbindlist(
  boundary_results,
  use.names = TRUE
)

# 8. Compare precision modes

precision_comparison <- all_summaries[
  ,
  .(
    seasonal_dates = uniqueN(research_date),
    total_covered_minutes = sum(covered_minutes),
    total_possible_minutes = sum(total_utc_minutes),
    total_uncovered_minutes = sum(uncovered_minutes),
    average_coverage_percent = mean(coverage_percent),
    minimum_coverage_percent = min(coverage_percent)
  ),
  by = .(precision_mode, duration_minutes)
]

setorder(
  precision_comparison,
  duration_minutes,
  precision_mode
)

# 9. Identify minutes whose coverage changes between modes

minute_comparison <- all_minutes[
  precision_mode == "raw",
  .(
    season,
    research_date,
    duration_minutes,
    minute_index,
    raw_active_events = active_events
  )
]

for (mode in setdiff(precision_modes, "raw")) {
  
  other <- all_minutes[
    precision_mode == mode,
    .(
      season,
      research_date,
      duration_minutes,
      minute_index,
      adjusted_active_events = active_events
    )
  ]
  
  minute_comparison <- merge(
    minute_comparison,
    other,
    by = c(
      "season",
      "research_date",
      "duration_minutes",
      "minute_index"
    ),
    all = TRUE
  )
  
  setnames(
    minute_comparison,
    "adjusted_active_events",
    paste0(mode, "_active_events")
  )
}

# 10. Save results

fwrite(
  all_summaries,
  file.path(output_dir, "script40E_precision_summary.csv")
)

fwrite(
  precision_comparison,
  file.path(output_dir, "script40E_precision_comparison.csv")
)

fwrite(
  all_boundaries,
  file.path(output_dir, "script40E_boundary_comparison.csv")
)

fwrite(
  all_minutes,
  file.path(output_dir, "script40E_all_minute_results.csv")
)

fwrite(
  minute_comparison,
  file.path(output_dir, "script40E_minute_precision_comparison.csv")
)

# 11. Print results

cat("\n============================================\n")
cat("PRECISION COMPARISON\n")
cat("============================================\n\n")

print(precision_comparison)

cat("\n============================================\n")
cat("SEASONAL PRECISION RESULTS\n")
cat("============================================\n\n")

print(all_summaries)

cat("\n============================================\n")
cat("BOUNDARY EVENT COMPARISON\n")
cat("============================================\n\n")

print(all_boundaries)

cat("\n============================================\n")
cat("FINAL QUALITY CHECK\n")
cat("============================================\n")

cat("Input events:", nrow(events), "\n")
cat("Unique event IDs:", uniqueN(events$event_id), "\n")
cat("Duplicate event IDs:", anyDuplicated(events$event_id), "\n")
cat("Precision scenarios:", length(precision_modes), "\n")
cat("Summary records:", nrow(all_summaries), "\n")
cat("Minute records:", nrow(all_minutes), "\n")

cat("\nOutput directory:\n")
cat(output_dir, "\n")

cat("\n============================================\n")
cat("SCRIPT 40E COMPLETED\n")
cat("============================================\n")
