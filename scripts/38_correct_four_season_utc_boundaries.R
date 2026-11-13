
# =============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 38: FAST FOUR-SEASON UTC BOUNDARY CORRECTION
# Reuses existing research results.
# Does NOT recalculate every location and local date.
# =============================================================

library(data.table)
library(ggplot2)

cat("\n")
cat("=============================================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 38: UTC BOUNDARY CORRECTION\n")
cat("=============================================================\n\n")


# =============================================================
# 1. LOAD FUNCTIONS
# =============================================================

source("R/solar_position.R")
source("R/equation_of_time.R")
source("R/prayer_config.R")
source("R/prayer_times.R")


# =============================================================
# 2. SETTINGS
# =============================================================

base_dir <- "data/processed/four_season_experiment"

places_file <- "data/processed/global_sunrise_sunset_local_dates.csv"

events_file <- file.path(
  base_dir,
  "four_season_prayer_events.csv"
)

audit_file <- file.path(
  base_dir,
  "four_season_calculation_audit.csv"
)

assumed_durations <- c(1, 5)

max_duration <- max(assumed_durations)

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

plot_directory <- "plots/four_season_experiment"

dir.create(
  plot_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================
# 3. LOAD EXISTING RESULTS
# =============================================================

if (!file.exists(events_file) ||
    !file.exists(audit_file) ||
    !file.exists(places_file)) {
  stop("One or more required existing files are missing.")
}

events <- fread(events_file)

audit <- fread(audit_file)

places <- fread(places_file)

cat("EXISTING FILES LOADED\n")

cat("Saved events:", nrow(events), "\n")

cat("Saved audit records:", nrow(audit), "\n")

cat("Geographical records:", nrow(places), "\n")


# =============================================================
# 4. PREPARE RESEARCH LOCATIONS
# =============================================================

required_place_columns <- c(
  "geoname_id",
  "name",
  "country_code",
  "latitude",
  "longitude",
  "timezone"
)

if (!all(required_place_columns %in% names(places))) {
  stop("Required geographical columns are missing.")
}

places <- unique(
  places[, ..required_place_columns],
  by = "geoname_id"
)

places <- places[
  !is.na(timezone) &
    !is.na(latitude) &
    !is.na(longitude)
]


# =============================================================
# 5. FIND ONLY BOUNDARY LOCAL DATES
# =============================================================

# For a maximum duration of five minutes, only events
# beginning in the five minutes before UTC midnight
# can affect the beginning of the research day.
#
# We identify the local calendar dates containing those
# boundary windows for each location.

boundary_dates <- list()

boundary_counter <- 0

for (s in seq_len(nrow(season_dates))) {
  
  season_name <- season_dates$season[s]
  
  research_date <- season_dates$research_date[s]
  
  day_start <- as.numeric(
    as.POSIXct(
      paste(research_date, "00:00:00"),
      tz = "UTC"
    )
  )
  
  boundary_start <- as.POSIXct(
    day_start - max_duration * 60,
    origin = "1970-01-01",
    tz = "UTC"
  )
  
  for (p in seq_len(nrow(places))) {
    
    place <- places[p]
    
    boundary_local_date <- as.Date(
      format(
        boundary_start,
        tz = place$timezone,
        format = "%Y-%m-%d"
      )
    )
    
    boundary_counter <- boundary_counter + 1
    
    boundary_dates[[boundary_counter]] <- data.table(
      season = season_name,
      research_date = as.character(research_date),
      geoname_id = place$geoname_id,
      local_date = as.character(boundary_local_date)
    )
  }
}

boundary_dates <- unique(
  rbindlist(boundary_dates),
  by = c("season", "geoname_id", "local_date")
)

cat("\nBOUNDARY DATE TARGETS\n")

cat("Target local dates:", nrow(boundary_dates), "\n")


# =============================================================
# 6. RECALCULATE ONLY BOUNDARY DATES
# =============================================================

boundary_events <- list()

boundary_event_counter <- 0

prayer_columns <- c(
  fajr = "fajr_epoch",
  dhuhr = "dhuhr_epoch",
  asr = "asr_epoch",
  maghrib = "maghrib_epoch",
  isha = "isha_epoch"
)

for (i in seq_len(nrow(boundary_dates))) {
  
  target <- boundary_dates[i]
  
  place <- places[
    geoname_id == target$geoname_id
  ][1]
  
  result <- tryCatch({
    
    calculate_prayer_times_for_place(
      research_date = target$local_date,
      latitude = place$latitude,
      longitude = place$longitude,
      timezone = place$timezone,
      config = prayer_config
    )
    
  }, error = function(e) {
    NULL
  })
  
  if (is.null(result)) {
    next
  }
  
  research_date <- as.Date(target$research_date)
  
  day_start <- as.numeric(
    as.POSIXct(
      paste(research_date, "00:00:00"),
      tz = "UTC"
    )
  )
  
  day_end <- day_start + 86400
  
  for (j in seq_along(prayer_columns)) {
    
    prayer_name <- names(prayer_columns)[j]
    
    event_epoch <- result[[prayer_columns[j]]]
    
    if (
      is.null(event_epoch) ||
      length(event_epoch) != 1 ||
      is.na(event_epoch) ||
      !is.finite(event_epoch)
    ) {
      next
    }
    
    event_epoch <- as.numeric(event_epoch)
    
    event_end <- event_epoch + max_duration * 60
    
    # Only recover events that begin before midnight
    # but whose assumed interval overlaps the UTC day.
    
    if (
      event_epoch < day_start &&
      event_end > day_start
    ) {
      
      boundary_event_counter <- boundary_event_counter + 1
      
      boundary_events[[boundary_event_counter]] <- data.table(
        event_id = paste(
          target$season,
          target$geoname_id,
          target$local_date,
          prayer_name,
          sep = "_"
        ),
        season = target$season,
        research_date = target$research_date,
        geoname_id = target$geoname_id,
        place_name = place$name,
        country_code = place$country_code,
        timezone = place$timezone,
        local_date = target$local_date,
        prayer = prayer_name,
        event_epoch = event_epoch,
        event_utc = as.POSIXct(
          event_epoch,
          origin = "1970-01-01",
          tz = "UTC"
        )
      )
    }
  }
  
  if (i %% 500 == 0) {
    cat(
      "Boundary targets processed:",
      i,
      "of",
      nrow(boundary_dates),
      "\n"
    )
  }
}


# =============================================================
# 7. COMBINE ORIGINAL AND RECOVERED EVENTS
# =============================================================

if (length(boundary_events) > 0) {
  
  recovered <- rbindlist(
    boundary_events,
    use.names = TRUE,
    fill = TRUE
  )
  
} else {
  
  recovered <- data.table(
    event_id = character(),
    season = character(),
    research_date = character(),
    geoname_id = numeric(),
    place_name = character(),
    country_code = character(),
    timezone = character(),
    local_date = character(),
    prayer = character(),
    event_epoch = numeric(),
    event_utc = as.POSIXct(character(), tz = "UTC")
  )
}

# Ensure the original event data has event IDs.

if (!"event_id" %in% names(events)) {
  
  events[, event_id := paste(
    season,
    geoname_id,
    local_date,
    prayer,
    sep = "_"
  )]
}

# Remove any recovered event already present.

recovered <- recovered[
  !event_id %in% events$event_id
]

corrected_events <- rbindlist(
  list(events, recovered),
  use.names = TRUE,
  fill = TRUE
)

setorder(
  corrected_events,
  season,
  event_epoch
)

if (anyDuplicated(corrected_events$event_id) > 0) {
  stop("Duplicate event IDs detected.")
}

cat("\nEVENT RECOVERY RESULTS\n")

cat("Original events:", nrow(events), "\n")

cat("New boundary events:", nrow(recovered), "\n")

cat("Corrected event total:", nrow(corrected_events), "\n")


# =============================================================
# 8. CALCULATE COVERAGE
# =============================================================

calculate_coverage <- function(
    season_events,
    day_start,
    duration_minutes,
    season_name) {
  
  total_minutes <- 1440
  
  support <- integer(total_minutes)
  
  day_end <- day_start + 86400
  
  for (i in seq_len(nrow(season_events))) {
    
    event_start <- season_events$event_epoch[i]
    
    event_end <- event_start + duration_minutes * 60
    
    if (
      event_start >= day_end ||
      event_end <= day_start
    ) {
      next
    }
    
    first_minute <- max(
      1L,
      as.integer(floor(
        (event_start - day_start) / 60
      ) + 1)
    )
    
    last_minute <- min(
      total_minutes,
      as.integer(ceiling(
        (event_end - day_start) / 60
      ))
    )
    
    if (first_minute <= last_minute) {
      
      support[first_minute:last_minute] <-
        support[first_minute:last_minute] + 1L
    }
  }
  
  minute_results <- data.table(
    season = season_name,
    minute_number = 0:1439,
    active_events = support,
    covered = support > 0
  )
  
  minute_results[, utc_timestamp := as.POSIXct(
    day_start + minute_number * 60,
    origin = "1970-01-01",
    tz = "UTC"
  )]
  
  summary <- data.table(
    season = season_name,
    duration_minutes = duration_minutes,
    total_minutes = total_minutes,
    covered_minutes = sum(support > 0),
    uncovered_minutes = sum(support == 0),
    coverage_percentage = mean(support > 0) * 100,
    minimum_active_events = min(support),
    maximum_active_events = max(support)
  )
  
  list(
    summary = summary,
    minutes = minute_results
  )
}


# =============================================================
# 9. RUN COVERAGE FOR ALL SEASONS
# =============================================================

all_summaries <- list()

all_minutes <- list()

counter <- 0

for (s in seq_len(nrow(season_dates))) {
  
  season_name <- season_dates$season[s]
  
  research_date <- season_dates$research_date[s]
  
  day_start <- as.numeric(
    as.POSIXct(
      paste(research_date, "00:00:00"),
      tz = "UTC"
    )
  )
  
  season_events <- corrected_events[
    season == season_name
  ]
  
  for (duration in assumed_durations) {
    
    result <- calculate_coverage(
      season_events,
      day_start,
      duration,
      season_name
    )
    
    counter <- counter + 1
    
    all_summaries[[counter]] <- result$summary
    
    all_minutes[[counter]] <- result$minutes
  }
}

coverage_summary <- rbindlist(all_summaries)

minute_coverage <- rbindlist(all_minutes)


# =============================================================
# 10. SAVE CORRECTED OUTPUTS
# =============================================================

fwrite(
  corrected_events,
  file.path(
    base_dir,
    "four_season_prayer_events_boundary_corrected.csv"
  )
)

fwrite(
  recovered,
  file.path(
    base_dir,
    "four_season_recovered_boundary_events.csv"
  )
)

fwrite(
  coverage_summary,
  file.path(
    base_dir,
    "four_season_coverage_summary_boundary_corrected.csv"
  )
)

fwrite(
  minute_coverage,
  file.path(
    base_dir,
    "four_season_minute_coverage_boundary_corrected.csv"
  )
)


# =============================================================
# 11. SAVE VISUALIZATIONS
# =============================================================

plot_coverage <- ggplot(
  coverage_summary,
  aes(
    x = season,
    y = coverage_percentage,
    fill = factor(duration_minutes)
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Boundary-Corrected Global Modelled Coverage",
    subtitle = "Four seasonal dates in 2026",
    x = "Season",
    y = "UTC minute coverage (%)",
    fill = "Assumed duration (minutes)"
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
    plot_directory,
    "20_four_season_boundary_corrected_coverage.png"
  ),
  plot_coverage,
  width = 11,
  height = 6,
  dpi = 300
)


plot_support <- ggplot(
  minute_coverage,
  aes(
    x = season,
    y = active_events,
    fill = factor(duration_minutes)
  )
) +
  geom_boxplot() +
  labs(
    title = "Boundary-Corrected Minute Support",
    subtitle = "Four seasonal dates in 2026",
    x = "Season",
    y = "Overlapping calculated events",
    fill = "Assumed duration (minutes)"
  ) +
  theme_minimal()

ggsave(
  file.path(
    plot_directory,
    "21_four_season_boundary_corrected_support.png"
  ),
  plot_support,
  width = 11,
  height = 6,
  dpi = 300
)


# =============================================================
# 12. FINAL RESULTS
# =============================================================

cat("\n")
cat("=============================================================\n")
cat("SCRIPT 38: FINAL RESULTS\n")
cat("=============================================================\n")

cat("\nRECOVERED BOUNDARY EVENTS\n")
cat("Original events:", nrow(events), "\n")
cat("Recovered events:", nrow(recovered), "\n")
cat("Corrected events:", nrow(corrected_events), "\n")

cat("\nCORRECTED COVERAGE SUMMARY\n")
print(coverage_summary)

cat("\nBOUNDARY EVENT DETAILS\n")
if (nrow(recovered) > 0) {
  print(recovered)
} else {
  cat("No additional boundary events were found.\n")
}

cat("\n")
cat("IMPORTANT:\n")
cat(
  "These are calculated prayer times with assumed durations,\n",
  "not independently verified actual Adhan broadcasts.\n"
)

cat("\n")
cat("SCRIPT 38 COMPLETED SUCCESSFULLY\n")
cat("=============================================================\n")
