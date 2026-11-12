
# =============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 36: FOUR-SEASON GLOBAL COVERAGE EXPERIMENT
# =============================================================

library(data.table)
library(ggplot2)

cat("\n")
cat("=============================================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 36: FOUR-SEASON GLOBAL COVERAGE EXPERIMENT\n")
cat("=============================================================\n\n")


# =============================================================
# 1. LOAD CALCULATION FUNCTIONS
# =============================================================

source("R/solar_position.R")
source("R/equation_of_time.R")
source("R/prayer_config.R")
source("R/prayer_times.R")


# =============================================================
# 2. DEFINE RESEARCH DATES
# =============================================================

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

print(season_dates)


# =============================================================
# 3. DEFINE RESEARCH SETTINGS
# =============================================================

assumed_durations <- c(1, 5)

prayer_columns <- c(
  fajr = "fajr_epoch",
  dhuhr = "dhuhr_epoch",
  asr = "asr_epoch",
  maghrib = "maghrib_epoch",
  isha = "isha_epoch"
)

output_directory <- "data/processed/four_season_experiment"

plot_directory <- "plots/four_season_experiment"

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  plot_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================
# 4. LOAD THE RESEARCH LOCATIONS
# =============================================================

places_file <- "data/processed/global_sunrise_sunset_local_dates.csv"

if (!file.exists(places_file)) {
  stop("Research location dataset not found: ", places_file)
}

places <- fread(places_file)

required_columns <- c(
  "geoname_id",
  "name",
  "country_code",
  "latitude",
  "longitude",
  "timezone"
)

missing_columns <- setdiff(
  required_columns,
  names(places)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required location columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

places <- unique(
  places[, ..required_columns],
  by = "geoname_id"
)

places <- places[
  !is.na(latitude) &
    !is.na(longitude) &
    !is.na(timezone) &
    latitude >= -90 &
    latitude <= 90 &
    longitude >= -180 &
    longitude <= 180
]

cat("\nRESEARCH LOCATIONS\n")
cat("Number of locations:", nrow(places), "\n")
cat("Countries:", uniqueN(places$country_code), "\n")


# =============================================================
# 5. HELPER: CONVERT UTC DATE TO UNIX TIME
# =============================================================

get_utc_epoch <- function(date, time = "00:00:00") {
  
  as.numeric(
    as.POSIXct(
      paste(as.character(date), time),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  )
}


# =============================================================
# 6. HELPER: CALCULATE COVERAGE
# =============================================================

calculate_season_coverage <- function(
    events,
    day_start,
    duration_minutes,
    season_name) {
  
  total_minutes <- 1440
  
  support <- integer(total_minutes)
  
  if (nrow(events) > 0) {
    
    for (i in seq_len(nrow(events))) {
      
      event_start <- events$event_epoch[i]
      
      event_end <- event_start +
        duration_minutes * 60
      
      first_minute <- floor(
        (event_start - day_start) / 60
      ) + 1
      
      last_minute <- ceiling(
        (event_end - day_start) / 60
      )
      
      first_minute <- max(1, first_minute)
      
      last_minute <- min(
        total_minutes,
        last_minute
      )
      
      if (first_minute <= last_minute) {
        
        support[first_minute:last_minute] <-
          support[first_minute:last_minute] + 1L
      }
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
    covered_minutes = sum(minute_results$covered),
    uncovered_minutes = sum(!minute_results$covered),
    coverage_percentage = mean(minute_results$covered) * 100,
    minimum_active_events = min(support),
    maximum_active_events = max(support)
  )
  
  list(
    summary = summary,
    minutes = minute_results
  )
}


# =============================================================
# 7. PREPARE RESULT COLLECTIONS
# =============================================================

all_events <- list()

all_audits <- list()

all_coverage <- list()

all_minute_results <- list()

event_counter <- 0

audit_counter <- 0

coverage_counter <- 0

minute_counter <- 0


# =============================================================
# 8. FOUR-SEASON CALCULATION
# =============================================================

for (season_index in seq_len(nrow(season_dates))) {
  
  season_name <- season_dates$season[season_index]
  
  research_date <- season_dates$research_date[season_index]
  
  cat("\n\n")
  cat("=============================================================\n")
  cat("SEASON:", season_name, "\n")
  cat("RESEARCH DATE:", as.character(research_date), "\n")
  cat("=============================================================\n")
  
  day_start <- get_utc_epoch(research_date)
  
  day_end <- day_start + 86400
  
  
  # -----------------------------------------------------------
  # Generate local dates around the UTC research day
  # -----------------------------------------------------------
  
  for (place_index in seq_len(nrow(places))) {
    
    place <- places[place_index]
    
    if (place_index %% 500 == 0) {
      cat(
        "Processing location",
        place_index,
        "of",
        nrow(places),
        "\n"
      )
    }
    
    # Find the local dates that could overlap the UTC day.
    
    utc_before <- as.POSIXct(
      day_start - 86400,
      origin = "1970-01-01",
      tz = "UTC"
    )
    
    utc_after <- as.POSIXct(
      day_end + 86400,
      origin = "1970-01-01",
      tz = "UTC"
    )
    
    local_start <- as.Date(
      format(
        utc_before,
        tz = place$timezone,
        format = "%Y-%m-%d"
      )
    )
    
    local_end <- as.Date(
      format(
        utc_after,
        tz = place$timezone,
        format = "%Y-%m-%d"
      )
    )
    
    local_dates <- seq.Date(
      local_start,
      local_end,
      by = "day"
    )
    
    
    # ---------------------------------------------------------
    # Calculate prayer times for each local date
    # ---------------------------------------------------------
    
    for (date_index in seq_along(local_dates)) {
      
      # IMPORTANT:
      # Indexed iteration preserves the Date class.
      
      local_date <- local_dates[date_index]
      
      result <- tryCatch({
        
        calculate_prayer_times_for_place(
          research_date = as.character(local_date),
          latitude = place$latitude,
          longitude = place$longitude,
          timezone = place$timezone,
          config = prayer_config
        )
        
      }, error = function(e) {
        
        NULL
        
      })
      
      
      # -------------------------------------------------------
      # Record calculation audit
      # -------------------------------------------------------
      
      audit_counter <- audit_counter + 1
      
      if (is.null(result)) {
        
        all_audits[[audit_counter]] <- data.table(
          season = season_name,
          research_date = as.character(research_date),
          geoname_id = place$geoname_id,
          place_name = place$name,
          country_code = place$country_code,
          timezone = place$timezone,
          local_date = as.character(local_date),
          prayer_status = "error",
          unavailable_prayers = NA_character_,
          calculation_success = FALSE
        )
        
        next
      }
      
      all_audits[[audit_counter]] <- data.table(
        season = season_name,
        research_date = as.character(research_date),
        geoname_id = place$geoname_id,
        place_name = place$name,
        country_code = place$country_code,
        timezone = place$timezone,
        local_date = as.character(local_date),
        prayer_status = result$prayer_status,
        unavailable_prayers = result$unavailable_prayers,
        calculation_success = TRUE
      )
      
      
      # -------------------------------------------------------
      # Extract the five prayer events
      # -------------------------------------------------------
      
      for (prayer_index in seq_along(prayer_columns)) {
        
        prayer_name <- names(prayer_columns)[prayer_index]
        
        epoch_column <- prayer_columns[prayer_index]
        
        event_epoch <- result[[epoch_column]]
        
        # Missing or invalid prayer calculations are preserved
        # as missing. They are never replaced with invented times.
        
        if (
          is.null(event_epoch) ||
          length(event_epoch) != 1 ||
          is.na(event_epoch) ||
          !is.finite(event_epoch)
        ) {
          next
        }
        
        # Keep only events inside the UTC research day.
        
        if (
          event_epoch < day_start ||
          event_epoch >= day_end
        ) {
          next
        }
        
        event_counter <- event_counter + 1
        
        all_events[[event_counter]] <- data.table(
          season = season_name,
          research_date = as.character(research_date),
          geoname_id = place$geoname_id,
          place_name = place$name,
          country_code = place$country_code,
          timezone = place$timezone,
          local_date = as.character(local_date),
          prayer = prayer_name,
          event_epoch = as.numeric(event_epoch),
          event_utc = as.POSIXct(
            event_epoch,
            origin = "1970-01-01",
            tz = "UTC"
          )
        )
      }
    }
  }
  
  cat(
    "\nCompleted location calculations for:",
    season_name,
    "\n"
  )
}


# =============================================================
# 9. COMBINE EVENTS AND AUDIT RECORDS
# =============================================================

if (length(all_events) == 0) {
  stop("No prayer events were generated. Check calculation functions.")
}

events <- rbindlist(
  all_events,
  use.names = TRUE,
  fill = TRUE
)

audit <- rbindlist(
  all_audits,
  use.names = TRUE,
  fill = TRUE
)

setorder(
  events,
  season,
  event_epoch
)

fwrite(
  events,
  file.path(
    output_directory,
    "four_season_prayer_events.csv"
  )
)

fwrite(
  audit,
  file.path(
    output_directory,
    "four_season_calculation_audit.csv"
  )
)


# =============================================================
# 10. CALCULATE COVERAGE FOR EACH SEASON
# =============================================================

for (season_index in seq_len(nrow(season_dates))) {
  
  season_name <- season_dates$season[season_index]
  
  research_date <- season_dates$research_date[season_index]
  
  day_start <- get_utc_epoch(research_date)
  
  season_events <- events[
    season == season_name
  ]
  
  cat("\n")
  cat("COVERAGE:", season_name, "\n")
  cat("Events inside UTC day:", nrow(season_events), "\n")
  
  for (duration in assumed_durations) {
    
    coverage_result <- calculate_season_coverage(
      events = season_events,
      day_start = day_start,
      duration_minutes = duration,
      season_name = season_name
    )
    
    coverage_counter <- coverage_counter + 1
    
    all_coverage[[coverage_counter]] <-
      coverage_result$summary
    
    minute_counter <- minute_counter + 1
    
    all_minute_results[[minute_counter]] <-
      coverage_result$minutes
  }
}


# =============================================================
# 11. SAVE COVERAGE RESULTS
# =============================================================

coverage_summary <- rbindlist(
  all_coverage,
  use.names = TRUE,
  fill = TRUE
)

minute_coverage <- rbindlist(
  all_minute_results,
  use.names = TRUE,
  fill = TRUE
)

coverage_1min <- coverage_summary[
  duration_minutes == 1
]

coverage_5min <- coverage_summary[
  duration_minutes == 5
]

fwrite(
  coverage_summary,
  file.path(
    output_directory,
    "four_season_coverage_summary.csv"
  )
)

fwrite(
  minute_coverage,
  file.path(
    output_directory,
    "four_season_minute_coverage_all_durations.csv"
  )
)

fwrite(
  coverage_1min,
  file.path(
    output_directory,
    "four_season_minute_coverage_1min.csv"
  )
)

fwrite(
  coverage_5min,
  file.path(
    output_directory,
    "four_season_minute_coverage_5min.csv"
  )
)


# =============================================================
# 12. DURATION COMPARISON
# =============================================================

duration_comparison <- dcast(
  coverage_summary,
  season ~ duration_minutes,
  value.var = "coverage_percentage"
)

fwrite(
  duration_comparison,
  file.path(
    output_directory,
    "four_season_duration_comparison.csv"
  )
)


# =============================================================
# 13. CALCULATION QUALITY SUMMARY
# =============================================================

audit_summary <- audit[, .(
  total_calculations = .N,
  successful_calculations = sum(calculation_success),
  failed_calculations = sum(!calculation_success),
  complete_calculations = sum(
    prayer_status == "complete",
    na.rm = TRUE
  ),
  partial_calculations = sum(
    prayer_status != "complete" &
      prayer_status != "error",
    na.rm = TRUE
  )
), by = season]

event_summary <- events[, .(
  total_prayer_events = .N,
  unique_locations = uniqueN(geoname_id),
  unique_countries = uniqueN(country_code)
), by = season]

quality_summary <- merge(
  audit_summary,
  event_summary,
  by = "season",
  all = TRUE
)

fwrite(
  quality_summary,
  file.path(
    output_directory,
    "four_season_quality_summary.csv"
  )
)


# =============================================================
# 14. VISUALIZATION: FOUR-SEASON COVERAGE
# =============================================================

plot_coverage <- ggplot(
  coverage_summary,
  aes(
    x = season,
    y = coverage_percentage,
    fill = factor(duration_minutes)
  )
) +
  
  geom_col(
    position = "dodge"
  ) +
  
  labs(
    title = "Global Modelled Prayer-Time Coverage",
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
    "18_four_season_coverage.png"
  ),
  plot_coverage,
  width = 11,
  height = 6,
  dpi = 300
)


# =============================================================
# 15. VISUALIZATION: WEAKEST MINUTE SUPPORT
# =============================================================

plot_weakest <- ggplot(
  minute_coverage,
  aes(
    x = season,
    y = active_events,
    fill = factor(duration_minutes)
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "Minute-by-Minute Event Support",
    subtitle = "Distribution of modelled event overlaps",
    x = "Season",
    y = "Overlapping calculated events",
    fill = "Assumed duration (minutes)"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    plot_directory,
    "19_four_season_weakest_support.png"
  ),
  plot_weakest,
  width = 11,
  height = 6,
  dpi = 300
)


# =============================================================
# 16. FINAL RESEARCH REPORT
# =============================================================

cat("\n\n")
cat("=============================================================\n")
cat("FOUR-SEASON EXPERIMENT: FINAL RESULTS\n")
cat("=============================================================\n\n")

cat("Research locations:", nrow(places), "\n")

cat(
  "Calculation records:",
  nrow(audit),
  "\n"
)

cat(
  "Successful calculations:",
  sum(audit$calculation_success),
  "\n"
)

cat(
  "Failed calculations:",
  sum(!audit$calculation_success),
  "\n"
)

cat(
  "Generated prayer events:",
  nrow(events),
  "\n"
)

cat("\nQUALITY SUMMARY\n")
print(quality_summary)

cat("\nCOVERAGE SUMMARY\n")
print(coverage_summary)

cat("\nDURATION COMPARISON\n")
print(duration_comparison)

cat("\n")
cat("OUTPUT FILES SAVED SUCCESSFULLY.\n")

cat("Research results:", output_directory, "\n")
cat("Research plots:", plot_directory, "\n")

cat("\n")
cat("IMPORTANT SCIENTIFIC LIMITATION:\n")
cat(
  "These results represent calculated prayer times with assumed\n",
  "event durations. They are NOT verified actual Adhan broadcasts.\n"
)

cat("\n")
cat("=============================================================\n")
cat("SCRIPT 36 COMPLETED\n")
cat("=============================================================\n")
