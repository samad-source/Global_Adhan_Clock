
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 15: BUILD THE COVERAGE CALCULATOR
# =====================================================


# 1. Create a function to calculate minute coverage

calculate_coverage <- function(
    events,
    day_start,
    day_end
) {
  
  # Create all minute intervals in the UTC day
  
  minute_starts <- seq(
    from = day_start,
    to = day_end - 60,
    by = "mins"
  )
  
  minute_ends <- minute_starts + 60
  
  
  # Count active Adhan intervals in each minute
  
  active_counts <- vapply(
    seq_along(minute_starts),
    
    function(i) {
      
      sum(
        events$start_utc < minute_ends[i] &
          events$end_utc > minute_starts[i]
      )
      
    },
    
    integer(1)
  )
  
  
  # Create the coverage results
  
  coverage <- data.frame(
    
    minute_utc = minute_starts,
    
    active_adhan_count = active_counts,
    
    coverage_status = ifelse(
      active_counts > 0,
      "Covered",
      "No recorded event"
    )
    
  )
  
  
  # Return the results
  
  return(coverage)
  
}


# 2. Create artificial test observations

# IMPORTANT:
# These are invented test records.
# They are NOT real Adhan observations.

test_events <- data.frame(
  
  location = c(
    "Test Location A",
    "Test Location B",
    "Test Location C"
  ),
  
  start_utc = as.POSIXct(
    c(
      "2026-01-01 08:00:00",
      "2026-01-01 08:03:00",
      "2026-01-01 08:10:00"
    ),
    tz = "UTC"
  ),
  
  end_utc = as.POSIXct(
    c(
      "2026-01-01 08:05:00",
      "2026-01-01 08:08:00",
      "2026-01-01 08:15:00"
    ),
    tz = "UTC"
  )
  
)


# 3. Define the UTC day

day_start <- as.POSIXct(
  "2026-01-01 00:00:00",
  tz = "UTC"
)

day_end <- as.POSIXct(
  "2026-01-02 00:00:00",
  tz = "UTC"
)


# 4. Calculate global minute coverage

coverage_results <- calculate_coverage(
  events = test_events,
  day_start = day_start,
  day_end = day_end
)


# 5. Calculate summary statistics

total_minutes <- nrow(coverage_results)

covered_minutes <- sum(
  coverage_results$active_adhan_count > 0
)

uncovered_minutes <- total_minutes - covered_minutes

coverage_percentage <- (
  covered_minutes / total_minutes
) * 100


# 6. Display the results

cat("\n====================================\n")

cat("GLOBAL ADHAN COVERAGE TEST\n")

cat("====================================\n")

cat("Total minutes:", total_minutes, "\n")

cat("Covered minutes:", covered_minutes, "\n")

cat("Minutes without recorded events:", uncovered_minutes, "\n")

cat(
  "Coverage percentage:",
  round(coverage_percentage, 2),
  "%\n"
)


# 7. Display the first 20 minutes with recorded events

cat("\nFIRST 20 MINUTES OF THE DAY\n")

print(head(coverage_results, 20))


# 8. Save the results

output_file <- "data/processed/coverage_test_results.csv"

write.csv(
  coverage_results,
  output_file,
  row.names = FALSE
)


# 9. Final message

cat("\n====================================\n")

cat("COVERAGE CALCULATOR TEST COMPLETED\n")

cat("====================================\n")

cat("Saved results:", output_file, "\n")
