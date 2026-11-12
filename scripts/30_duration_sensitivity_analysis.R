
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 30: ADHAN DURATION SENSITIVITY ANALYSIS
# ============================================================

library(data.table)

cat("\n============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("DURATION SENSITIVITY ANALYSIS\n")
cat("============================================\n\n")


# 1. Research window

research_start <- as.POSIXct(
  "2026-11-10 00:00:00",
  tz = "UTC"
)

research_end <- as.POSIXct(
  "2026-11-17 00:00:00",
  tz = "UTC"
)

start_epoch <- as.numeric(research_start)
end_epoch <- as.numeric(research_end)


# 2. Define durations to investigate

durations <- c(1, 2, 3, 5, 7, 10)

cat("Durations being tested:\n")
print(durations)


# 3. Load calculated prayer events

input_file <- "data/processed/global_prayer_events_utc.csv"

if (!file.exists(input_file)) {
  stop("Prayer event file not found.")
}

events <- fread(input_file)

if (anyNA(events$event_epoch)) {
  stop("Missing event timestamps detected.")
}

if (anyDuplicated(events$event_id) > 0) {
  stop("Duplicate event identifiers detected.")
}

cat("\nEvents loaded:", nrow(events), "\n")


# 4. Create UTC minute bins

minute_epochs <- seq(
  start_epoch,
  end_epoch - 60,
  by = 60
)

minutes <- data.table(
  minute_epoch = minute_epochs
)

minutes[, minute_start_utc := as.POSIXct(
  minute_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)]

cat("UTC minutes:", nrow(minutes), "\n")


# 5. Prepare result containers

sensitivity_results <- list()
minute_results <- list()


# 6. Test each assumed duration

for (duration in durations) {
  
  cat("\n--------------------------------------------\n")
  cat("Testing duration:", duration, "minute(s)\n")
  cat("--------------------------------------------\n")
  
  # Calculate interval boundaries
  
  intervals <- events[
    ,
    .(
      event_id,
      interval_start = event_epoch,
      interval_end = event_epoch + duration * 60
    )
  ]
  
  # Restrict intervals to the research window
  
  intervals[, clipped_start := pmax(
    interval_start,
    start_epoch
  )]
  
  intervals[, clipped_end := pmin(
    interval_end,
    end_epoch
  )]
  
  intervals <- intervals[
    clipped_start < clipped_end
  ]
  
  # Identify first and last UTC minute overlapped
  
  intervals[, first_minute := floor(
    clipped_start / 60
  ) * 60]
  
  intervals[, last_minute := ceiling(
    clipped_end / 60
  ) * 60 - 60]
  
  # Expand each interval into its overlapping minutes
  
  links <- intervals[
    ,
    .(
      minute_epoch = seq(
        first_minute,
        last_minute,
        by = 60
      )
    ),
    by = event_id
  ]
  
  links <- links[
    minute_epoch >= start_epoch &
      minute_epoch < end_epoch
  ]
  
  # Count distinct events overlapping each minute
  
  minute_counts <- links[
    ,
    .(
      overlapping_event_count = uniqueN(event_id)
    ),
    by = minute_epoch
  ]
  
  # Attach counts to all research minutes
  
  coverage <- merge(
    minutes,
    minute_counts,
    by = "minute_epoch",
    all.x = TRUE
  )
  
  coverage[
    is.na(overlapping_event_count),
    overlapping_event_count := 0L
  ]
  
  coverage[, covered := overlapping_event_count > 0]
  
  coverage[, assumed_duration_minutes := duration]
  
  # Calculate coverage statistics
  
  total_minutes <- nrow(coverage)
  
  covered_minutes <- coverage[
    covered == TRUE,
    .N
  ]
  
  uncovered_minutes <- total_minutes - covered_minutes
  
  coverage_percentage <- (
    covered_minutes / total_minutes
  ) * 100
  
  # Find continuous uncovered periods
  
  coverage[, run_id := rleid(covered)]
  
  runs <- coverage[
    ,
    .(
      covered = first(covered),
      minutes_in_run = .N,
      run_start = min(minute_start_utc),
      run_end = max(minute_start_utc) + 60
    ),
    by = run_id
  ]
  
  uncovered_runs <- runs[covered == FALSE]
  
  if (nrow(uncovered_runs) > 0) {
    
    longest_gap <- max(
      uncovered_runs$minutes_in_run
    )
    
    number_of_gaps <- nrow(uncovered_runs)
    
  } else {
    
    longest_gap <- 0L
    number_of_gaps <- 0L
  }
  
  # Save summary for this duration
  
  sensitivity_results[[as.character(duration)]] <-
    data.table(
      assumed_duration_minutes = duration,
      total_minutes = total_minutes,
      covered_minutes = covered_minutes,
      uncovered_minutes = uncovered_minutes,
      coverage_percentage = coverage_percentage,
      uncovered_periods = number_of_gaps,
      longest_uncovered_period_minutes = longest_gap
    )
  
  minute_results[[as.character(duration)]] <- coverage
  
  cat("Covered minutes:", covered_minutes, "\n")
  cat("Uncovered minutes:", uncovered_minutes, "\n")
  cat("Coverage:", round(coverage_percentage, 2), "%\n")
  cat("Longest uncovered period:", longest_gap, "minute(s)\n")
}


# 7. Combine all duration results

sensitivity_summary <- rbindlist(
  sensitivity_results
)

all_minute_results <- rbindlist(
  minute_results
)


# 8. Display sensitivity results

cat("\n============================================\n")
cat("DURATION SENSITIVITY RESULTS\n")
cat("============================================\n\n")

print(sensitivity_summary)


# 9. Save sensitivity summary

summary_file <- paste0(
  "data/processed/duration_sensitivity_summary.csv"
)

fwrite(
  sensitivity_summary,
  summary_file
)


# 10. Save minute-level results

minute_file <- paste0(
  "data/processed/duration_sensitivity_minute_results.csv"
)

fwrite(
  all_minute_results,
  minute_file
)


# 11. Save one-minute uncovered periods

one_minute_gaps <- all_minute_results[
  assumed_duration_minutes == 1 &
    covered == FALSE
]

one_minute_gaps[, gap_group := rleid(
  minute_epoch
)]

# The complete one-minute timeline is also preserved
# in the minute-level results file.


# 12. Final report

cat("\n============================================\n")
cat("SCRIPT 30 COMPLETED\n")
cat("============================================\n")

cat("\nDurations tested:", length(durations), "\n")

cat("\nSensitivity summary saved to:\n")
cat(summary_file, "\n")

cat("\nMinute-level results saved to:\n")
cat(minute_file, "\n")

cat("\nIMPORTANT SCIENTIFIC NOTE:\n")
cat("All durations are modelling assumptions.\n")
cat("This is not evidence of actual broadcasts.\n")
