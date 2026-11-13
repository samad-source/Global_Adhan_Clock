
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 40: FINAL UTC BOUNDARY AUDIT
# ============================================================
#
# PURPOSE:
# Recheck all four seasonal UTC research days.
# Examine every local calendar date that could contribute
# an event interval overlapping the UTC research day.
#
# IMPORTANT:
# These are calculated prayer events, not verified broadcasts.
# The 1- and 5-minute durations are modelling assumptions.
#
# ============================================================

library(data.table)
library(ggplot2)

cat("\n")
cat("============================================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 40: FINAL UTC BOUNDARY AUDIT\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 1. LOAD OUR CALCULATION FUNCTIONS
# ------------------------------------------------------------

source("R/prayer_config.R")
source("R/solar_position.R")
source("R/equation_of_time.R")
source("R/solar_noon.R")
source("R/sunrise_sunset.R")
source("R/prayer_times.R")

# ------------------------------------------------------------
# 2. FILE PATHS
# ------------------------------------------------------------

event_file <- file.path(
  "data", "processed", "four_season_experiment",
  "four_season_prayer_events.csv"
)

location_file <- file.path(
  "data", "processed",
  "global_sunrise_sunset_local_dates.csv"
)

output_dir <- file.path(
  "data", "processed", "four_season_experiment",
  "final_boundary_audit"
)

plot_dir <- file.path(
  "plots", "four_season_experiment",
  "final_boundary_audit"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(event_file))
stopifnot(file.exists(location_file))

# ------------------------------------------------------------
# 3. LOAD ORIGINAL EVENTS AND LOCATIONS
# ------------------------------------------------------------

original_events <- fread(event_file)
locations <- fread(location_file)

required_event_columns <- c(
  "season", "research_date", "geoname_id",
  "place_name", "country_code", "timezone",
  "local_date", "prayer", "event_epoch", "event_utc"
)

required_location_columns <- c(
  "geoname_id", "name", "country_code",
  "latitude", "longitude", "timezone"
)

if (!all(required_event_columns %in% names(original_events))) {
  stop("The original event file is missing required columns.")
}

if (!all(required_location_columns %in% names(locations))) {
  stop("The location file is missing required columns.")
}

locations <- unique(
  locations[, ..required_location_columns]
)

setnames(locations, "name", "place_name")

if (anyDuplicated(locations$geoname_id)) {
  stop("Duplicate GeoNames IDs found in the location dataset.")
}

cat("ORIGINAL DATA\n")
cat("Original events:", nrow(original_events), "\n")
cat("Research locations:", nrow(locations), "\n")

# ------------------------------------------------------------
# 4. DEFINE THE FOUR SEASONAL RESEARCH DAYS
# ------------------------------------------------------------

season_dates <- data.table(
  season = c(
    "March Equinox",
    "June Solstice",
    "September Equinox",
    "December Solstice"
  ),
  research_date = as.Date(c(
    "2026-03-20",
    "2026-06-21",
    "2026-09-23",
    "2026-12-21"
  ))
)

# The largest assumed interval is five minutes.
MAX_DURATION_SECONDS <- 5 * 60

# ------------------------------------------------------------
# 5. HELPER: GET LOCAL DATE FROM A UTC INSTANT
# ------------------------------------------------------------

get_local_date <- function(epoch, timezone) {
  
  instant <- as.POSIXct(
    epoch,
    origin = "1970-01-01",
    tz = "UTC"
  )
  
  as.Date(
    format(
      instant,
      tz = timezone,
      format = "%Y-%m-%d"
    )
  )
}

# ------------------------------------------------------------
# 6. HELPER: CALCULATE EVENTS FOR ONE LOCATION AND DATE
# ------------------------------------------------------------

calculate_location_events <- function(
    location,
    local_date,
    season,
    research_date,
    day_start,
    day_end
) {
  
  result <- tryCatch({
    
    prayer_result <- calculate_prayer_times_for_place(
      research_date = as.character(local_date),
      latitude = location$latitude,
      longitude = location$longitude,
      timezone = location$timezone,
      config = prayer_config
    )
    
    prayer_names <- c(
      "Fajr", "Dhuhr", "Asr", "Maghrib", "Isha"
    )
    
    rows <- list()
    
    for (prayer_name in prayer_names) {
      
      epoch_column <- paste0(
        tolower(prayer_name),
        "_epoch"
      )
      
      if (!epoch_column %in% names(prayer_result)) {
        next
      }
      
      event_epoch <- prayer_result[[epoch_column]]
      
      if (
        length(event_epoch) != 1 ||
        is.na(event_epoch) ||
        !is.finite(event_epoch)
      ) {
        next
      }
      
      # Include only events that could overlap the UTC day
      # under the maximum five-minute duration assumption.
      if (
        event_epoch >= day_start - MAX_DURATION_SECONDS &&
        event_epoch < day_end
      ) {
        
        rows[[length(rows) + 1]] <- data.table(
          season = season,
          research_date = as.character(research_date),
          geoname_id = location$geoname_id,
          place_name = location$place_name,
          country_code = location$country_code,
          timezone = location$timezone,
          local_date = as.character(local_date),
          prayer = prayer_name,
          event_epoch = as.numeric(event_epoch),
          event_utc = format(
            as.POSIXct(
              event_epoch,
              origin = "1970-01-01",
              tz = "UTC"
            ),
            tz = "UTC",
            format = "%Y-%m-%d %H:%M:%S"
          )
        )
      }
    }
    
    if (length(rows) == 0) {
      return(NULL)
    }
    
    rbindlist(rows)
    
  }, error = function(e) {
    
    NULL
    
  })
  
  result
}

# ------------------------------------------------------------
# 7. RECOVER ALL POSSIBLE UTC BOUNDARY EVENTS
# ------------------------------------------------------------

recovered_list <- list()

total_checks <- nrow(locations) * nrow(season_dates)
check_number <- 0L

for (s in seq_len(nrow(season_dates))) {
  
  season_name <- season_dates$season[s]
  research_date <- season_dates$research_date[s]
  
  day_start <- as.numeric(as.POSIXct(
    paste(research_date, "00:00:00"),
    tz = "UTC"
  ))
  
  day_end <- day_start + 86400
  
  cat("\n")
  cat("PROCESSING:", season_name, "\n")
  cat("Research date:", as.character(research_date), "\n")
  
  for (i in seq_len(nrow(locations))) {
    
    location <- locations[i]
    
    # Examine local dates at both ends of the possible
    # five-minute event-overlap window.
    first_date <- get_local_date(
      day_start - MAX_DURATION_SECONDS,
      location$timezone
    )
    
    last_date <- get_local_date(
      day_end - 1,
      location$timezone
    )
    
    candidate_dates <- seq.Date(
      first_date,
      last_date,
      by = "day"
    )
    
    for (candidate_date in candidate_dates) {
      
      calculated <- calculate_location_events(
        location = location,
        local_date = candidate_date,
        season = season_name,
        research_date = research_date,
        day_start = day_start,
        day_end = day_end
      )
      
      if (!is.null(calculated) && nrow(calculated) > 0) {
        
        recovered_list[[length(recovered_list) + 1]] <- calculated
      }
    }
    
    check_number <- check_number + 1L
    
    if (check_number %% 100 == 0) {
      cat(
        "Locations checked:",
        check_number, "/", total_checks, "\n"
      )
    }
  }
}

if (length(recovered_list) > 0) {
  recovered_events <- rbindlist(
    recovered_list,
    use.names = TRUE,
    fill = TRUE
  )
} else {
  recovered_events <- data.table(
    season = character(),
    research_date = character(),
    geoname_id = numeric(),
    place_name = character(),
    country_code = character(),
    timezone = character(),
    local_date = character(),
    prayer = character(),
    event_epoch = numeric(),
    event_utc = character()
  )
}

cat("\nBOUNDARY RECOVERY\n")
cat("Recovered candidate events:", nrow(recovered_events), "\n")

# ------------------------------------------------------------
# 8. CREATE STABLE EVENT IDs
# ------------------------------------------------------------

original_events[, event_id := paste(
  geoname_id, local_date, prayer, sep = "_"
)]

recovered_events[, event_id := paste(
  geoname_id, local_date, prayer, sep = "_"
)]

# ------------------------------------------------------------
# 9. COMBINE ORIGINAL AND RECOVERED EVENTS
# ------------------------------------------------------------

combined_events <- rbindlist(
  list(original_events, recovered_events),
  use.names = TRUE,
  fill = TRUE
)

# Check whether a repeated event ID has conflicting timestamps.
conflicts <- combined_events[
  ,
  .(unique_epochs = uniqueN(event_epoch)),
  by = event_id
][unique_epochs > 1]

if (nrow(conflicts) > 0) {
  fwrite(
    conflicts,
    file.path(output_dir, "event_id_timestamp_conflicts.csv")
  )
  
  stop(
    "Some event IDs have conflicting timestamps. ",
    "Inspect event_id_timestamp_conflicts.csv."
  )
}

# Remove repeated records using the stable event ID.
setorder(combined_events, event_id)

corrected_events <- unique(
  combined_events,
  by = "event_id"
)

# Retain events that can overlap the relevant UTC research day.
# Events outside that range are not needed for this audit.
corrected_events[, research_date := as.character(research_date)]

corrected_events[, day_start := as.numeric(as.POSIXct(
  paste(research_date, "00:00:00"),
  tz = "UTC"
))]

corrected_events <- corrected_events[
  event_epoch >= day_start - MAX_DURATION_SECONDS &
    event_epoch < day_start + 86400
]

corrected_events[, day_start := NULL]

# ------------------------------------------------------------
# 10. EVENT ID QUALITY CHECK
# ------------------------------------------------------------

duplicate_ids <- corrected_events[
  ,
  .N,
  by = event_id
][N > 1]

missing_ids <- corrected_events[
  is.na(event_id) | event_id == ""
]

cat("\nEVENT ID QUALITY\n")
cat("Corrected events:", nrow(corrected_events), "\n")
cat("Duplicate IDs:", nrow(duplicate_ids), "\n")
cat("Missing IDs:", nrow(missing_ids), "\n")

if (nrow(duplicate_ids) > 0 || nrow(missing_ids) > 0) {
  stop("Event ID quality checks failed.")
}

# ------------------------------------------------------------
# 11. SAVE EVENT DATA
# ------------------------------------------------------------

fwrite(
  recovered_events,
  file.path(
    output_dir,
    "script40_recovered_boundary_candidates.csv"
  )
)

fwrite(
  corrected_events,
  file.path(
    output_dir,
    "script40_corrected_prayer_events.csv"
  )
)

# ------------------------------------------------------------
# 12. RECALCULATE UTC MINUTE COVERAGE
# ------------------------------------------------------------

duration_values <- c(1, 5)

minute_results <- list()
coverage_summaries <- list()

for (s in seq_len(nrow(season_dates))) {
  
  season_name <- season_dates$season[s]
  research_date <- as.character(season_dates$research_date[s])
  
  day_start <- as.numeric(as.POSIXct(
    paste(research_date, "00:00:00"),
    tz = "UTC"
  ))
  
  day_end <- day_start + 86400
  
  day_events <- corrected_events[
    season == season_name &
      research_date == research_date
  ]
  
  # Avoid ambiguous data.table column lookup.
  day_events <- corrected_events[
    corrected_events$season == season_name &
      corrected_events$research_date == research_date
  ]
  
  for (duration in duration_values) {
    
    duration_seconds <- duration * 60
    
    minute_start <- day_start + (0:1439) * 60
    minute_end <- minute_start + 60
    
    event_support <- integer(1440)
    
    for (j in seq_len(nrow(day_events))) {
      
      event_start <- day_events$event_epoch[j]
      event_end <- event_start + duration_seconds
      
      # Clip the assumed event interval to the UTC day.
      clipped_start <- max(event_start, day_start)
      clipped_end <- min(event_end, day_end)
      
      if (clipped_start >= clipped_end) {
        next
      }
      
      first_minute <- max(
        0,
        floor((clipped_start - day_start) / 60)
      )
      
      last_minute <- min(
        1439,
        ceiling((clipped_end - day_start) / 60) - 1
      )
      
      if (first_minute <= last_minute) {
        
        event_support[
          (first_minute:last_minute) + 1
        ] <- event_support[
          (first_minute:last_minute) + 1
        ] + 1L
      }
    }
    
    minute_table <- data.table(
      season = season_name,
      research_date = research_date,
      duration_minutes = duration,
      minute_utc = 0:1439,
      minute_start_utc = as.POSIXct(
        minute_start,
        origin = "1970-01-01",
        tz = "UTC"
      ),
      overlapping_events = event_support,
      covered = event_support > 0
    )
    
    minute_results[[length(minute_results) + 1]] <- minute_table
    
    uncovered <- sum(!minute_table$covered)
    
    coverage_summaries[[length(coverage_summaries) + 1]] <-
      data.table(
        season = season_name,
        research_date = research_date,
        duration_minutes = duration,
        total_minutes = 1440,
        covered_minutes = sum(minute_table$covered),
        uncovered_minutes = uncovered,
        coverage_percent = mean(minute_table$covered) * 100,
        minimum_event_support = min(event_support),
        maximum_event_support = max(event_support)
      )
  }
}

minute_coverage <- rbindlist(minute_results)
coverage_summary <- rbindlist(coverage_summaries)

# ------------------------------------------------------------
# 13. SAVE COVERAGE RESULTS
# ------------------------------------------------------------

fwrite(
  minute_coverage,
  file.path(
    output_dir,
    "script40_minute_coverage.csv"
  )
)

fwrite(
  coverage_summary,
  file.path(
    output_dir,
    "script40_coverage_summary.csv"
  )
)

# ------------------------------------------------------------
# 14. FINAL QUALITY REPORT
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("FINAL UTC BOUNDARY AUDIT SUMMARY\n")
cat("============================================================\n\n")

cat("Original events:", nrow(original_events), "\n")
cat("Recovered candidates:", nrow(recovered_events), "\n")
cat("Corrected unique events:", nrow(corrected_events), "\n")
cat("Duplicate event IDs:", nrow(duplicate_ids), "\n")
cat("Missing event IDs:", nrow(missing_ids), "\n")

cat("\nCOVERAGE RESULTS\n")
print(coverage_summary)

# ------------------------------------------------------------
# 15. PLOT SEASONAL COVERAGE
# ------------------------------------------------------------

coverage_plot <- ggplot(
  coverage_summary,
  aes(
    x = season,
    y = coverage_percent,
    fill = factor(duration_minutes)
  )
) +
  geom_col(
    position = "dodge"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    title = "Final Four-Season UTC Minute Coverage",
    subtitle = paste(
      "Calculated prayer events with assumed",
      "1-minute and 5-minute durations"
    ),
    x = "Season",
    y = "Covered UTC minutes (%)",
    fill = "Assumed duration"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

ggsave(
  file.path(
    plot_dir,
    "22_final_utc_boundary_coverage.png"
  ),
  coverage_plot,
  width = 11,
  height = 7,
  dpi = 300
)

support_plot <- ggplot(
  minute_coverage,
  aes(
    x = minute_utc,
    y = overlapping_events,
    color = factor(duration_minutes)
  )
) +
  geom_line() +
  facet_wrap(
    ~ season,
    ncol = 2
  ) +
  labs(
    title = "UTC Minute Event Support After Boundary Audit",
    x = "Minute of UTC day",
    y = "Overlapping calculated events",
    color = "Assumed duration"
  ) +
  theme_minimal()

ggsave(
  file.path(
    plot_dir,
    "23_final_utc_boundary_event_support.png"
  ),
  support_plot,
  width = 13,
  height = 9,
  dpi = 300
)

# ------------------------------------------------------------
# 16. SAVE AUDIT METADATA
# ------------------------------------------------------------

audit_metadata <- data.table(
  item = c(
    "original_events",
    "recovered_boundary_candidates",
    "corrected_unique_events",
    "duplicate_event_ids",
    "missing_event_ids",
    "research_locations",
    "seasonal_research_days",
    "assumed_durations_minutes"
  ),
  value = c(
    nrow(original_events),
    nrow(recovered_events),
    nrow(corrected_events),
    nrow(duplicate_ids),
    nrow(missing_ids),
    nrow(locations),
    nrow(season_dates),
    paste(duration_values, collapse = ", ")
  )
)

fwrite(
  audit_metadata,
  file.path(
    output_dir,
    "script40_audit_metadata.csv"
  )
)

cat("\n")
cat("============================================================\n")
cat("SCRIPT 40 COMPLETED\n")
cat("============================================================\n")
cat("\nResults saved in:\n")
cat(output_dir, "\n")
cat(plot_dir, "\n")
cat("\nRemember: coverage is modelled, not observed Adhan activity.\n")
cat("============================================================\n")
