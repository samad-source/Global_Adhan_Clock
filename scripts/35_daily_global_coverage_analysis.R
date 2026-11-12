
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 35: DAILY GLOBAL COVERAGE ANALYSIS
# ============================================================
#
# OBJECTIVES:
# 1. Analyse global coverage separately for each UTC day.
# 2. Calculate covered and uncovered minutes per day.
# 3. Identify the weakest minute of each day.
# 4. Calculate minimum, maximum and average event support.
# 5. Compare 1-minute and 5-minute duration assumptions.
# 6. Visualize daily global coverage.
#
# IMPORTANT:
# These are calculated prayer events with assumed durations.
# They are NOT verified actual Adhan broadcasts.
#
# ============================================================


# 1. LOAD PACKAGES
# ============================================================

library(data.table)
library(ggplot2)


# 2. DEFINE PROJECT PATHS
# ============================================================

project_root <- "C:/Users/CDT WO AZEEZ SAMAD/Downloads/Global_Adhan_Clock"

event_file <- file.path(
  project_root,
  "data/processed/global_prayer_events_utc.csv"
)

places_file <- file.path(
  project_root,
  "data/processed/application_places.csv"
)

output_dir <- file.path(
  project_root,
  "data/processed"
)

plot_dir <- file.path(
  project_root,
  "plots/hypothesis_visualizations"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  plot_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. DEFINE RESEARCH WINDOW
# ============================================================

research_start <- as.POSIXct(
  "2026-11-10 00:00:00",
  tz = "UTC"
)

research_end <- as.POSIXct(
  "2026-11-17 00:00:00",
  tz = "UTC"
)

window_start_epoch <- as.numeric(research_start)
window_end_epoch <- as.numeric(research_end)

total_days <- as.integer(
  difftime(
    research_end,
    research_start,
    units = "days"
  )
)

minutes_per_day <- 1440

total_minutes <- total_days * minutes_per_day

cat("\n============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 35: DAILY GLOBAL COVERAGE ANALYSIS\n")
cat("============================================\n")

cat("\nResearch start:", format(research_start, tz = "UTC"))
cat("\nResearch end:", format(research_end, tz = "UTC"))
cat("\nResearch days:", total_days)
cat("\nExpected UTC minutes:", total_minutes, "\n")


# 4. LOAD DATA
# ============================================================

if (!file.exists(event_file)) {
  stop("Global prayer events file was not found.")
}

if (!file.exists(places_file)) {
  stop("Application places file was not found.")
}

events <- fread(event_file)

places <- fread(places_file)

cat("\nDATA LOADED SUCCESSFULLY\n")

cat("Prayer event records:", nrow(events), "\n")
cat("Geographical records:", nrow(places), "\n")


# 5. CHECK REQUIRED COLUMNS
# ============================================================

required_event_columns <- c(
  "geoname_id",
  "event_epoch",
  "local_date",
  "prayer"
)

missing_event_columns <- setdiff(
  required_event_columns,
  names(events)
)

if (length(missing_event_columns) > 0) {
  stop(
    paste(
      "Missing event columns:",
      paste(missing_event_columns, collapse = ", ")
    )
  )
}

required_place_columns <- c(
  "geoname_id",
  "country_code"
)

missing_place_columns <- setdiff(
  required_place_columns,
  names(places)
)

if (length(missing_place_columns) > 0) {
  stop(
    paste(
      "Missing geographical columns:",
      paste(missing_place_columns, collapse = ", ")
    )
  )
}


# 6. FILTER EVENTS TO THE UTC RESEARCH WINDOW
# ============================================================

events[, event_epoch := as.numeric(event_epoch)]

events <- events[
  !is.na(event_epoch) &
    event_epoch >= window_start_epoch &
    event_epoch < window_end_epoch
]

if (nrow(events) == 0) {
  stop("No events were found inside the research window.")
}

# Give every calculated event a unique ID.

events[, event_id := paste(
  geoname_id,
  local_date,
  tolower(as.character(prayer)),
  sep = "_"
)]

cat("\nEVENTS INSIDE RESEARCH WINDOW:", nrow(events), "\n")

cat(
  "UNIQUE RESEARCH LOCATIONS:",
  uniqueN(events$geoname_id),
  "\n"
)



# 7. ATTACH COUNTRY INFORMATION — CORRECTED
# ============================================================

research_place_ids <- unique(events$geoname_id)

# Get one country code per research location.

research_places <- unique(
  places[
    geoname_id %in% research_place_ids,
    .(geoname_id, country_code)
  ]
)

# Remove any existing country_code from events
# before merging to prevent .x and .y columns.

if ("country_code" %in% names(events)) {
  events[, country_code := NULL]
}

if ("country_code.x" %in% names(events)) {
  events[, country_code.x := NULL]
}

if ("country_code.y" %in% names(events)) {
  events[, country_code.y := NULL]
}

# Attach the correct country code.

events <- merge(
  events,
  research_places,
  by = "geoname_id",
  all.x = TRUE
)

# Handle missing country codes.

events[
  is.na(country_code) | country_code == "",
  country_code := "Unknown"
]

# Confirm that the correct column exists.

stopifnot("country_code" %in% names(events))

cat("\nCOUNTRY INFORMATION ATTACHED SUCCESSFULLY\n")

cat(
  "Unique research locations:",
  uniqueN(events$geoname_id),
  "\n"
)

cat(
  "Unique country codes:",
  uniqueN(events$country_code),
  "\n"
)



# 8. DEFINE COVERAGE CALCULATION FUNCTION
# ============================================================

calculate_daily_coverage <- function(
    event_data,
    duration_minutes,
    research_start_epoch,
    research_end_epoch,
    days,
    minutes_per_day
) {
  
  cat(
    "\nCalculating coverage with assumed duration:",
    duration_minutes,
    "minute(s)\n"
  )
  
  duration_seconds <- duration_minutes * 60
  
  window_seconds <- research_end_epoch -
    research_start_epoch
  
  total_minutes <- as.integer(window_seconds / 60)
  
  # Calculate interval start and end minute indexes.
  #
  # Intervals are end-exclusive.
  #
  # An event starting at 10:00:30 and ending at 10:05:30
  # overlaps UTC minute bins 10:00 through 10:05.
  
  start_index <- floor(
    (event_data$event_epoch - research_start_epoch) / 60
  )
  
  end_index <- floor(
    (
      event_data$event_epoch +
        duration_seconds -
        1 -
        research_start_epoch
    ) / 60
  )
  
  # Clip intervals to the research window.
  
  start_index <- pmax(0, start_index)
  
  end_index <- pmin(
    total_minutes - 1,
    end_index
  )
  
  valid <- !is.na(start_index) &
    !is.na(end_index) &
    start_index <= end_index
  
  start_index <- as.integer(start_index[valid])
  end_index <- as.integer(end_index[valid])
  
  valid_events <- event_data[valid]
  
  # Number of minute bins overlapped by each event.
  
  overlap_lengths <- end_index - start_index + 1
  
  # Expand event-to-minute relationships.
  
  event_row_index <- rep(
    seq_len(nrow(valid_events)),
    times = overlap_lengths
  )
  
  minute_index <- rep(
    start_index,
    times = overlap_lengths
  ) + sequence(overlap_lengths) - 1
  
  overlap_data <- data.table(
    minute_index = minute_index,
    event_id = valid_events$event_id[event_row_index],
    geoname_id = valid_events$geoname_id[event_row_index],
    country_code = valid_events$country_code[event_row_index]
  )
  
  # Count unique events, places and countries per minute.
  
  minute_support <- overlap_data[
    ,
    .(
      active_events = uniqueN(event_id),
      active_places = uniqueN(geoname_id),
      active_countries = uniqueN(country_code)
    ),
    by = minute_index
  ]
  
  # Create all minute bins, including uncovered minutes.
  
  minute_results <- data.table(
    minute_index = 0:(total_minutes - 1)
  )
  
  minute_results <- merge(
    minute_results,
    minute_support,
    by = "minute_index",
    all.x = TRUE
  )
  
  minute_results[
    is.na(active_events),
    active_events := 0L
  ]
  
  minute_results[
    is.na(active_places),
    active_places := 0L
  ]
  
  minute_results[
    is.na(active_countries),
    active_countries := 0L
  ]
  
  # Convert indexes into UTC timestamps.
  
  minute_results[
    ,
    minute_utc_epoch := research_start_epoch +
      minute_index * 60
  ]
  
  minute_results[
    ,
    minute_utc := as.POSIXct(
      minute_utc_epoch,
      origin = "1970-01-01",
      tz = "UTC"
    )
  ]
  
  minute_results[
    ,
    utc_date := as.Date(minute_utc, tz = "UTC")
  ]
  
  minute_results[
    ,
    utc_hour := as.integer(
      format(minute_utc, "%H", tz = "UTC")
    )
  ]
  
  minute_results[
    ,
    covered := active_events > 0
  ]
  
  # Calculate daily summaries.
  
  daily_summary <- minute_results[
    ,
    .(
      total_minutes = .N,
      covered_minutes = sum(covered),
      uncovered_minutes = sum(!covered),
      coverage_percentage = mean(covered) * 100,
      minimum_active_events = min(active_events),
      maximum_active_events = max(active_events),
      average_active_events = mean(active_events),
      minimum_active_places = min(active_places),
      minimum_active_countries = min(active_countries)
    ),
    by = utc_date
  ]
  
  daily_summary[
    ,
    assumed_duration_minutes := duration_minutes
  ]
  
  daily_summary[
    ,
    utc_day_name := weekdays(utc_date)
  ]
  
  setorder(daily_summary, utc_date)
  
  # Identify the weakest minute in each day.
  
  weakest_minutes <- minute_results[
    ,
    {
      weakest_count <- min(active_events)
      
      weakest_rows <- which(
        active_events == weakest_count
      )
      
      .(
        weakest_event_count = weakest_count,
        weakest_minute_count = length(weakest_rows),
        first_weakest_utc_minute =
          format(
            minute_utc[weakest_rows[1]],
            "%Y-%m-%d %H:%M",
            tz = "UTC"
          ),
        weakest_active_places =
          active_places[weakest_rows[1]],
        weakest_active_countries =
          active_countries[weakest_rows[1]]
      )
    },
    by = utc_date
  ]
  
  daily_summary <- merge(
    daily_summary,
    weakest_minutes,
    by = "utc_date",
    all.x = TRUE
  )
  
  setorder(daily_summary, utc_date)
  
  list(
    minute_results = minute_results,
    daily_summary = daily_summary
  )
}


# 9. RUN ONE-MINUTE SCENARIO
# ============================================================

coverage_1min <- calculate_daily_coverage(
  event_data = events,
  duration_minutes = 1,
  research_start_epoch = window_start_epoch,
  research_end_epoch = window_end_epoch,
  days = total_days,
  minutes_per_day = minutes_per_day
)


# 10. RUN FIVE-MINUTE SCENARIO
# ============================================================

coverage_5min <- calculate_daily_coverage(
  event_data = events,
  duration_minutes = 5,
  research_start_epoch = window_start_epoch,
  research_end_epoch = window_end_epoch,
  days = total_days,
  minutes_per_day = minutes_per_day
)


# 11. SAVE DAILY RESULTS
# ============================================================

fwrite(
  coverage_1min$daily_summary,
  file.path(
    output_dir,
    "daily_coverage_summary_1min.csv"
  )
)

fwrite(
  coverage_5min$daily_summary,
  file.path(
    output_dir,
    "daily_coverage_summary_5min.csv"
  )
)

fwrite(
  coverage_1min$minute_results,
  file.path(
    output_dir,
    "daily_minute_support_1min.csv"
  )
)

fwrite(
  coverage_5min$minute_results,
  file.path(
    output_dir,
    "daily_minute_support_5min.csv"
  )
)


# 12. COMPARE THE TWO DURATION SCENARIOS
# ============================================================

scenario_comparison <- rbindlist(
  list(
    coverage_1min$daily_summary[
      ,
      .(
        utc_date,
        assumed_duration_minutes,
        coverage_percentage,
        covered_minutes,
        uncovered_minutes,
        minimum_active_events,
        maximum_active_events,
        average_active_events
      )
    ],
    coverage_5min$daily_summary[
      ,
      .(
        utc_date,
        assumed_duration_minutes,
        coverage_percentage,
        covered_minutes,
        uncovered_minutes,
        minimum_active_events,
        maximum_active_events,
        average_active_events
      )
    ]
  )
)

setorder(
  scenario_comparison,
  assumed_duration_minutes,
  utc_date
)

fwrite(
  scenario_comparison,
  file.path(
    output_dir,
    "daily_duration_scenario_comparison.csv"
  )
)


# 13. CREATE DAILY COVERAGE GRAPH
# ============================================================

coverage_plot <- ggplot(
  scenario_comparison,
  aes(
    x = utc_date,
    y = coverage_percentage,
    group = factor(assumed_duration_minutes),
    color = factor(assumed_duration_minutes)
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  scale_x_date(
    date_labels = "%b %d",
    date_breaks = "1 day"
  ) +
  labs(
    title = "Daily Global Modelled Minute Coverage",
    subtitle = "Comparison of one-minute and five-minute assumptions",
    x = "UTC Date",
    y = "Covered UTC minutes (%)",
    color = "Assumed interval\n(minutes)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = file.path(
    plot_dir,
    "16_daily_global_coverage_comparison.png"
  ),
  plot = coverage_plot,
  width = 11,
  height = 6,
  dpi = 300
)


# 14. CREATE DAILY MINIMUM EVENT SUPPORT GRAPH
# ============================================================

support_plot <- ggplot(
  coverage_5min$daily_summary,
  aes(
    x = utc_date,
    y = minimum_active_events
  )
) +
  geom_col() +
  geom_text(
    aes(label = minimum_active_events),
    vjust = -0.5
  ) +
  labs(
    title = "Weakest Minute of Each UTC Day",
    subtitle = "Minimum number of overlapping calculated events",
    x = "UTC Date",
    y = "Minimum active calculated events"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(
    plot_dir,
    "17_daily_weakest_minute_support.png"
  ),
  plot = support_plot,
  width = 11,
  height = 6,
  dpi = 300
)


# 15. PRINT ONE-MINUTE RESULTS
# ============================================================

cat("\n============================================\n")
cat("ONE-MINUTE SCENARIO RESULTS\n")
cat("============================================\n")

print(coverage_1min$daily_summary)


# 16. PRINT FIVE-MINUTE RESULTS
# ============================================================

cat("\n============================================\n")
cat("FIVE-MINUTE SCENARIO RESULTS\n")
cat("============================================\n")

print(coverage_5min$daily_summary)


# 17. PRINT DAILY SCENARIO COMPARISON
# ============================================================

cat("\n============================================\n")
cat("DAILY DURATION SCENARIO COMPARISON\n")
cat("============================================\n")

print(scenario_comparison)


# 18. FINAL RESEARCH SUMMARY
# ============================================================

cat("\n============================================\n")
cat("SCRIPT 35 FINAL REPORT\n")
cat("============================================\n")

cat("\nONE-MINUTE SCENARIO\n")

cat(
  "Fully covered days:",
  sum(coverage_1min$daily_summary$uncovered_minutes == 0),
  "out of",
  total_days,
  "\n"
)

cat(
  "Total uncovered minutes:",
  sum(coverage_1min$daily_summary$uncovered_minutes),
  "\n"
)

cat("\nFIVE-MINUTE SCENARIO\n")

cat(
  "Fully covered days:",
  sum(coverage_5min$daily_summary$uncovered_minutes == 0),
  "out of",
  total_days,
  "\n"
)

cat(
  "Total uncovered minutes:",
  sum(coverage_5min$daily_summary$uncovered_minutes),
  "\n"
)

cat(
  "Minimum event support across all minutes:",
  min(coverage_5min$minute_results$active_events),
  "\n"
)

cat(
  "Maximum event support across all minutes:",
  max(coverage_5min$minute_results$active_events),
  "\n"
)

cat("\nOUTPUT FILES CREATED:\n")

cat("\n1. daily_coverage_summary_1min.csv")
cat("\n2. daily_coverage_summary_5min.csv")
cat("\n3. daily_minute_support_1min.csv")
cat("\n4. daily_minute_support_5min.csv")
cat("\n5. daily_duration_scenario_comparison.csv")

cat("\n\nGRAPHS CREATED:\n")

cat("\n6. 16_daily_global_coverage_comparison.png")
cat("\n7. 17_daily_weakest_minute_support.png")

cat("\n\n============================================\n")
cat("SCRIPT 35 COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat(
  "\nSCIENTIFIC REMINDER:",
  "\nThese results represent modelled coverage.",
  "\nThey do not establish actual Adhan broadcasts.",
  "\nThe original hypothesis remains unproven.\n"
)
