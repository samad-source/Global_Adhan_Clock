
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 29: ESTIMATED ADHAN INTERVALS AND UTC COVERAGE
# ============================================================

library(data.table)

cat("\n============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("ESTIMATED ADHAN INTERVALS AND UTC COVERAGE\n")
cat("============================================\n\n")


# 1. Research settings

research_start <- as.POSIXct(
  "2026-11-10 00:00:00",
  tz = "UTC"
)

research_end <- as.POSIXct(
  "2026-11-17 00:00:00",
  tz = "UTC"
)

assumed_duration_minutes <- 5

start_epoch <- as.numeric(research_start)
end_epoch <- as.numeric(research_end)

cat("Research start:", format(research_start), "\n")
cat("Research end:", format(research_end), "\n")
cat("Assumed interval duration:",
    assumed_duration_minutes, "minutes\n\n")


# 2. Load the global prayer-event timeline

input_file <- "data/processed/global_prayer_events_utc.csv"

if (!file.exists(input_file)) {
  stop("Event timeline not found: ", input_file)
}

events <- fread(input_file)

cat("Prayer events loaded:", nrow(events), "\n")


# 3. Validate event timestamps

if (!"event_epoch" %in% names(events)) {
  stop("The event_epoch column is missing.")
}

if (anyNA(events$event_epoch)) {
  stop("Missing event timestamps detected.")
}

if (anyDuplicated(events$event_id) > 0) {
  stop("Duplicate event IDs detected.")
}


# 4. Construct estimated intervals

# IMPORTANT:
# These are MODELLED intervals.
# They are NOT verified actual Adhan broadcasts.

events[, interval_start_epoch := event_epoch]

events[, interval_end_epoch :=
         event_epoch + assumed_duration_minutes * 60]

# Clip intervals to the research window

events[, clipped_start_epoch :=
         pmax(interval_start_epoch, start_epoch)]

events[, clipped_end_epoch :=
         pmin(interval_end_epoch, end_epoch)]

# Keep intervals that overlap the research window

intervals <- events[
  clipped_start_epoch < clipped_end_epoch
]

cat("Estimated intervals created:", nrow(intervals), "\n")


# 5. Create interval start and end timestamps

intervals[, interval_start_utc := as.POSIXct(
  clipped_start_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)]

intervals[, interval_end_utc := as.POSIXct(
  clipped_end_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)]


# 6. Create all UTC minute bins

minute_epochs <- seq(
  from = start_epoch,
  to = end_epoch - 60,
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

minutes[, minute_end_utc := as.POSIXct(
  minute_epoch + 60,
  origin = "1970-01-01",
  tz = "UTC"
)]

cat("\nUTC minute bins created:", nrow(minutes), "\n")


# 7. Find every minute overlapped by an interval

# A minute is covered if:
# interval start < minute end
# AND
# interval end > minute start

intervals[, first_minute_epoch :=
            floor(clipped_start_epoch / 60) * 60]

intervals[, last_minute_epoch :=
            ceiling(clipped_end_epoch / 60) * 60 - 60]

# Expand intervals into the UTC minutes they overlap

event_minute_links <- intervals[
  ,
  .(
    minute_epoch = seq(
      first_minute_epoch,
      last_minute_epoch,
      by = 60
    )
  ),
  by = .(event_id)
]

# Remove any minute outside the research window

event_minute_links <- event_minute_links[
  minute_epoch >= start_epoch &
    minute_epoch < end_epoch
]

cat("Event-minute overlap records:",
    nrow(event_minute_links), "\n")


# 8. Count overlapping events per UTC minute

minute_event_counts <- event_minute_links[
  ,
  .(
    overlapping_event_count = uniqueN(event_id)
  ),
  by = minute_epoch
]


# 9. Attach overlap counts to all 10,080 minutes

coverage <- merge(
  minutes,
  minute_event_counts,
  by = "minute_epoch",
  all.x = TRUE
)

coverage[
  is.na(overlapping_event_count),
  overlapping_event_count := 0L
]

coverage[, covered := overlapping_event_count > 0]


# 10. Calculate coverage statistics

total_minutes <- nrow(coverage)

covered_minutes <- coverage[
  covered == TRUE,
  .N
]

uncovered_minutes <- total_minutes - covered_minutes

coverage_percentage <- (
  covered_minutes / total_minutes
) * 100

summary_table <- data.table(
  research_start = format(research_start),
  research_end = format(research_end),
  assumed_duration_minutes = assumed_duration_minutes,
  total_minutes = total_minutes,
  covered_minutes = covered_minutes,
  uncovered_minutes = uncovered_minutes,
  coverage_percentage = coverage_percentage
)


# 11. Find the longest continuous uncovered period

coverage[, run_id := rleid(covered)]

coverage_runs <- coverage[
  ,
  .(
    covered = first(covered),
    minutes_in_run = .N,
    run_start = min(minute_start_utc),
    run_end = max(minute_end_utc)
  ),
  by = run_id
]

uncovered_runs <- coverage_runs[covered == FALSE]

if (nrow(uncovered_runs) > 0) {
  
  setorder(
    uncovered_runs,
    -minutes_in_run
  )
  
  longest_uncovered_run <- uncovered_runs[1]
  
} else {
  
  longest_uncovered_run <- data.table(
    minutes_in_run = 0L,
    run_start = as.POSIXct(character(), tz = "UTC"),
    run_end = as.POSIXct(character(), tz = "UTC")
  )
}


# 12. Display the research results

cat("\n============================================\n")
cat("SEVEN-DAY GLOBAL COVERAGE RESULTS\n")
cat("============================================\n\n")

print(summary_table)

cat("\nLongest continuous uncovered period:\n")

if (nrow(uncovered_runs) > 0) {
  print(longest_uncovered_run)
} else {
  cat("No uncovered period found in this model.\n")
}


# 13. Display sample coverage records

cat("\n============================================\n")
cat("FIRST 20 UTC MINUTES\n")
cat("============================================\n")

print(
  coverage[
    1:20,
    .(
      minute_start_utc,
      overlapping_event_count,
      covered
    )
  ]
)


# 14. Save estimated intervals

interval_output <- paste0(
  "data/processed/global_prayer_intervals_assumed_",
  assumed_duration_minutes,
  "min.csv"
)

fwrite(
  intervals,
  interval_output
)


# 15. Save minute-by-minute coverage

coverage_output <- paste0(
  "data/processed/global_minute_coverage_assumed_",
  assumed_duration_minutes,
  "min.csv"
)

fwrite(
  coverage,
  coverage_output
)


# 16. Save coverage summary

summary_output <- paste0(
  "data/processed/global_coverage_summary_assumed_",
  assumed_duration_minutes,
  "min.csv"
)

fwrite(
  summary_table,
  summary_output
)


# 17. Save uncovered periods

uncovered_output <- paste0(
  "data/processed/global_uncovered_periods_assumed_",
  assumed_duration_minutes,
  "min.csv"
)

fwrite(
  uncovered_runs,
  uncovered_output
)


# 18. Final report

cat("\n============================================\n")
cat("SCRIPT 29 COMPLETED\n")
cat("============================================\n")

cat("Total UTC minutes:", total_minutes, "\n")
cat("Covered minutes:", covered_minutes, "\n")
cat("Uncovered minutes:", uncovered_minutes, "\n")

cat(
  "Modelled coverage:",
  round(coverage_percentage, 2),
  "%\n"
)

cat("\nSaved files:\n")
cat(interval_output, "\n")
cat(coverage_output, "\n")
cat(summary_output, "\n")
cat(uncovered_output, "\n")

cat("\nIMPORTANT SCIENTIFIC WARNING:\n")
cat("Five minutes is an assumed duration.\n")
cat("Coverage represents modelled prayer-time intervals.\n")
cat("It does NOT establish actual Adhan broadcasts.\n")
