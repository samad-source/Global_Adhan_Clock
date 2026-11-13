
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 40D: BOUNDARY TIMESTAMP PRECISION AUDIT
# ============================================================

library(data.table)

cat("\n============================================\n")
cat("SCRIPT 40D: TIMESTAMP PRECISION AUDIT\n")
cat("============================================\n\n")

base_dir <- "data/processed/four_season_experiment"

file <- file.path(
  base_dir,
  "four_season_recovered_boundary_events.csv"
)

output_dir <- file.path(
  base_dir,
  "boundary_impact_analysis"
)

recovered <- fread(file)

# Preserve the original timestamp precision.
# Convert directly to numeric POSIXct seconds.

if (!inherits(recovered$event_utc, "POSIXct")) {
  recovered[, event_utc := as.POSIXct(
    event_utc,
    tz = "UTC"
  )]
}

recovered[, event_epoch_exact := as.numeric(event_utc)]

# Calculate the next UTC midnight for each research date.

recovered[, day_start := as.numeric(
  as.POSIXct(
    paste(as.character(research_date), "00:00:00"),
    tz = "UTC"
  )
)]

recovered[, seconds_before_midnight :=
            day_start - event_epoch_exact
]

# Inspect both assumed durations.

for (duration in c(1, 5)) {
  
  recovered[, interval_duration_seconds :=
              duration * 60
  ]
  
  recovered[, exact_overlap :=
              event_epoch_exact < day_start &
              event_epoch_exact + interval_duration_seconds > day_start
  ]
  
  recovered[, boundary_difference_seconds :=
              seconds_before_midnight - interval_duration_seconds
  ]
  
  cat("\n--------------------------------------------\n")
  cat("DURATION:", duration, "MINUTE(S)\n")
  cat("--------------------------------------------\n")
  
  cat(
    "Events before midnight:",
    sum(recovered$event_epoch_exact < recovered$day_start),
    "\n"
  )
  
  cat(
    "Events overlapping next UTC day:",
    sum(recovered$exact_overlap),
    "\n"
  )
  
  cat(
    "Events ending exactly at midnight:",
    sum(
      abs(recovered$boundary_difference_seconds) < 0.000001
    ),
    "\n"
  )
  
  cat(
    "Events ending after midnight:",
    sum(recovered$boundary_difference_seconds < -0.000001),
    "\n"
  )
  
  cat(
    "Events ending before midnight:",
    sum(recovered$boundary_difference_seconds > 0.000001),
    "\n"
  )
  
  # Display events within one second of the duration boundary.
  
  near_boundary <- recovered[
    abs(boundary_difference_seconds) < 1,
    .(
      season,
      research_date,
      place_name,
      local_date,
      prayer,
      event_utc_precise = format(
        event_utc,
        "%Y-%m-%d %H:%M:%OS6",
        tz = "UTC"
      ),
      seconds_before_midnight,
      interval_duration_seconds,
      boundary_difference_seconds,
      exact_overlap
    )
  ]
  
  cat("\nEvents within one second of the boundary:\n")
  print(near_boundary)
  
  fwrite(
    near_boundary,
    file.path(
      output_dir,
      paste0(
        "script40D_precision_near_boundary_",
        duration,
        "min.csv"
      )
    )
  )
}

cat("\n============================================\n")
cat("SCRIPT 40D COMPLETED\n")
cat("============================================\n")
