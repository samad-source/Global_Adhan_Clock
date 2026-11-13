
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 38: EVENT ID AND OUTPUT REPAIR
# ============================================================

library(data.table)
library(ggplot2)

cat("\n============================================\n")
cat("GLOBAL ADHAN CLOCK")
cat("SCRIPT 38: EVENT ID REPAIR")
cat("============================================\n")

# ------------------------------------------------------------
# 1. STANDARDIZE ORIGINAL AND RECOVERED DATA
# ------------------------------------------------------------

events <- as.data.table(copy(events))
recovered <- as.data.table(copy(recovered))

required_columns <- c(
  "geoname_id",
  "local_date",
  "prayer",
  "event_epoch"
)

for (column in required_columns) {
  
  if (!column %in% names(events)) {
    stop(paste("Missing original event column:", column))
  }
  
  if (!column %in% names(recovered)) {
    stop(paste("Missing recovered event column:", column))
  }
}

# ------------------------------------------------------------
# 2. STANDARDIZE DATE AND ID FIELDS
# ------------------------------------------------------------

for (column in c("research_date", "local_date")) {
  
  if (column %in% names(events)) {
    set(events, j = column,
        value = as.character(events[[column]]))
  }
  
  if (column %in% names(recovered)) {
    set(recovered, j = column,
        value = as.character(recovered[[column]]))
  }
}

# ------------------------------------------------------------
# 3. GENERATE CONSISTENT EVENT IDS
# ------------------------------------------------------------

events[, event_id := paste(
  geoname_id,
  local_date,
  prayer,
  sep = "_"
)]

recovered[, event_id := paste(
  geoname_id,
  local_date,
  prayer,
  sep = "_"
)]

cat("\nEVENT ID GENERATION\n")

cat("Original events:", nrow(events), "\n")
cat("Recovered events:", nrow(recovered), "\n")

cat(
  "Original missing IDs:",
  sum(is.na(events$event_id)),
  "\n"
)

cat(
  "Recovered missing IDs:",
  sum(is.na(recovered$event_id)),
  "\n"
)

# ------------------------------------------------------------
# 4. CHECK EVENT ID VALIDITY
# ------------------------------------------------------------

if (anyNA(events$event_id) || anyNA(recovered$event_id)) {
  stop("Missing event IDs remain.")
}

if (any(events$event_id == "") ||
    any(recovered$event_id == "")) {
  stop("Empty event IDs detected.")
}

# ------------------------------------------------------------
# 5. COMBINE EVENTS
# ------------------------------------------------------------

combined_events <- rbindlist(
  list(events, recovered),
  use.names = TRUE,
  fill = TRUE
)

cat("\nCombined event records:", nrow(combined_events), "\n")

# ------------------------------------------------------------
# 6. INVESTIGATE DUPLICATE EVENT IDS
# ------------------------------------------------------------

duplicate_check <- combined_events[
  ,
  .N,
  by = "event_id"
][N > 1]

cat(
  "Duplicate event IDs:",
  nrow(duplicate_check),
  "\n"
)

if (nrow(duplicate_check) > 0) {
  
  duplicate_details <- combined_events[
    event_id %in% duplicate_check$event_id
  ][order(event_id)]
  
  # Check whether duplicated IDs have conflicting event data.
  identity_columns <- c(
    "geoname_id",
    "local_date",
    "prayer",
    "event_epoch"
  )
  
  conflict_check <- duplicate_details[
    ,
    lapply(.SD, function(x) uniqueN(x)),
    by = "event_id",
    .SDcols = identity_columns
  ]
  
  conflicting_ids <- conflict_check[
    geoname_id > 1 |
      local_date > 1 |
      prayer > 1 |
      event_epoch > 1
  ]$event_id
  
  if (length(conflicting_ids) > 0) {
    
    cat("\nCONFLICTING EVENTS FOUND:\n")
    
    print(
      duplicate_details[
        event_id %in% conflicting_ids
      ]
    )
    
    stop(
      "Conflicting event records found. ",
      "No events have been removed."
    )
  }
  
  # Identical event identities: retain one record per event.
  corrected_events <- unique(
    combined_events,
    by = "event_id"
  )
  
} else {
  
  corrected_events <- copy(combined_events)
  
}

setorder(corrected_events, event_epoch)

cat("\nCorrected unique events:", nrow(corrected_events), "\n")

stopifnot(
  anyDuplicated(corrected_events$event_id) == 0
)

# ------------------------------------------------------------
# 7. REPAIR MINUTE COVERAGE DATA
# ------------------------------------------------------------

duration_order <- rep(
  assumed_durations,
  times = nrow(season_dates)
)

if (length(all_minutes) != length(duration_order)) {
  stop("Minute result count does not match expected scenarios.")
}

for (i in seq_along(all_minutes)) {
  
  all_minutes[[i]] <- as.data.table(all_minutes[[i]])
  
  all_minutes[[i]][
    ,
    duration_minutes := duration_order[i]
  ]
}

minute_coverage <- rbindlist(
  all_minutes,
  use.names = TRUE,
  fill = TRUE
)

if (!"covered" %in% names(minute_coverage)) {
  stop("The covered column is missing.")
}

if (!"season" %in% names(minute_coverage)) {
  stop("The season column is missing.")
}

# ------------------------------------------------------------
# 8. BUILD COVERAGE SUMMARY
# ------------------------------------------------------------

coverage_summary <- minute_coverage[
  ,
  .(
    total_minutes = .N,
    covered_minutes = sum(covered, na.rm = TRUE),
    uncovered_minutes = sum(!covered, na.rm = TRUE),
    coverage_percentage = mean(covered, na.rm = TRUE) * 100
  ),
  by = .(season, duration_minutes)
]

# ------------------------------------------------------------
# 9. SAVE CORRECTED DATA
# ------------------------------------------------------------

output_dir <- "data/processed/four_season_experiment"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

fwrite(
  corrected_events,
  file.path(
    output_dir,
    "four_season_prayer_events_boundary_corrected.csv"
  )
)

fwrite(
  recovered,
  file.path(
    output_dir,
    "four_season_recovered_boundary_events.csv"
  )
)

fwrite(
  minute_coverage,
  file.path(
    output_dir,
    "four_season_minute_coverage_boundary_corrected.csv"
  )
)

fwrite(
  coverage_summary,
  file.path(
    output_dir,
    "four_season_coverage_summary_boundary_corrected.csv"
  )
)

# ------------------------------------------------------------
# 10. REBUILD COVERAGE VISUALIZATION
# ------------------------------------------------------------

p1 <- ggplot(
  coverage_summary,
  aes(
    x = season,
    y = coverage_percentage,
    fill = factor(duration_minutes)
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Four-Season Global UTC Minute Coverage",
    subtitle = "Astronomically calculated events with assumed durations",
    x = "Season",
    y = "Coverage (%)",
    fill = "Duration (minutes)"
  ) +
  theme_minimal()

plot_dir <- "plots/four_season_experiment"

dir.create(
  plot_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  file.path(
    plot_dir,
    "20_four_season_boundary_corrected_coverage.png"
  ),
  p1,
  width = 11,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 11. FINAL REPORT
# ------------------------------------------------------------

cat("\n============================================\n")
cat("SCRIPT 38 FINAL RESULTS")
cat("\n============================================\n")

cat("Original events:", nrow(events), "\n")
cat("Recovered events:", nrow(recovered), "\n")
cat("Corrected unique events:", nrow(corrected_events), "\n")

cat(
  "Remaining duplicate IDs:",
  anyDuplicated(corrected_events$event_id),
  "\n"
)

cat("\nCOVERAGE SUMMARY:\n")

print(coverage_summary)

cat("\n============================================\n")
cat("SCRIPT 38 REPAIR COMPLETED")
cat("\n============================================\n")
