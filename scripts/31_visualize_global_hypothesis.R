
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 31: VISUALIZE THE GLOBAL ADHAN HYPOTHESIS
# UPDATED VERSION
# ============================================================

library(data.table)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("maps", quietly = TRUE)) {
  install.packages("maps")
}

library(ggplot2)


cat("\n")
cat("============================================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 31: HYPOTHESIS VISUALIZATION\n")
cat("============================================================\n\n")


# ============================================================
# 1. DEFINE THE RESEARCH WINDOW
# ============================================================

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

total_minutes <- as.integer(
  (end_epoch - start_epoch) / 60
)

cat("Research start:", format(research_start), "UTC\n")
cat("Research end:", format(research_end), "UTC\n")
cat("Total UTC minutes:", total_minutes, "\n\n")


# ============================================================
# 2. LOAD GLOBAL EVENT DATA
# ============================================================

events <- fread(
  "data/processed/global_prayer_events_utc.csv"
)

required_columns <- c(
  "geoname_id",
  "name",
  "country_code",
  "latitude",
  "longitude",
  "event_epoch",
  "prayer",
  "event_id"
)

missing_columns <- setdiff(
  required_columns,
  names(events)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

if (anyDuplicated(events$event_id) > 0) {
  stop("Duplicate event IDs detected.")
}

if (anyNA(events$event_epoch)) {
  stop("Missing event timestamps detected.")
}

events[, event_epoch := as.numeric(event_epoch)]

cat("Events loaded:", nrow(events), "\n")
cat("Unique event IDs:", uniqueN(events$event_id), "\n")
cat("Countries represented:", uniqueN(events$country_code), "\n")
cat("Places represented:", uniqueN(events$geoname_id), "\n\n")


# ============================================================
# 3. COVERAGE CALCULATION FUNCTION
# ============================================================

start_epoch_global <- start_epoch
end_epoch_global <- end_epoch


calculate_minute_support <- function(events, duration_minutes) {
  
  cat(
    "Calculating coverage for",
    duration_minutes,
    "minute duration...\n"
  )
  
  intervals <- copy(
    events[, .(
      event_id,
      country_code,
      start_epoch = event_epoch
    )]
  )
  
  intervals[, end_epoch :=
              start_epoch + duration_minutes * 60
  ]
  
  # Clip events to the research window
  intervals[, clipped_start :=
              pmax(start_epoch, start_epoch_global)
  ]
  
  intervals[, clipped_end :=
              pmin(end_epoch, end_epoch_global)
  ]
  
  intervals <- intervals[
    clipped_end > clipped_start
  ]
  
  # Find first and last overlapping UTC minutes
  intervals[, first_minute :=
              floor(clipped_start / 60) * 60
  ]
  
  intervals[, last_minute :=
              ceiling(clipped_end / 60) * 60 - 60
  ]
  
  # Expand event intervals into UTC minute records
  minute_links <- intervals[
    ,
    .(
      minute_epoch = seq(
        first_minute,
        last_minute,
        by = 60
      )
    ),
    by = .(event_id, country_code)
  ]
  
  # Count distinct events and countries
  minute_support <- minute_links[
    ,
    .(
      active_events = uniqueN(event_id),
      active_countries = uniqueN(country_code)
    ),
    by = minute_epoch
  ]
  
  # Create complete UTC minute grid
  minute_grid <- data.table(
    minute_epoch = seq(
      start_epoch_global,
      end_epoch_global - 60,
      by = 60
    )
  )
  
  minute_grid <- merge(
    minute_grid,
    minute_support,
    by = "minute_epoch",
    all.x = TRUE
  )
  
  minute_grid[
    is.na(active_events),
    active_events := 0L
  ]
  
  minute_grid[
    is.na(active_countries),
    active_countries := 0L
  ]
  
  minute_grid[, covered :=
                active_events > 0
  ]
  
  minute_grid[, minute_time :=
                as.POSIXct(
                  minute_epoch,
                  origin = "1970-01-01",
                  tz = "UTC"
                )
  ]
  
  minute_grid[, utc_date :=
                as.Date(minute_time, tz = "UTC")
  ]
  
  minute_grid[, minute_of_day :=
                as.integer(format(minute_time, "%H", tz = "UTC")) * 60 +
                as.integer(format(minute_time, "%M", tz = "UTC"))
  ]
  
  minute_grid[, day_label :=
                format(utc_date, "%d %b")
  ]
  
  minute_grid[, duration_minutes :=
                duration_minutes
  ]
  
  summary <- data.table(
    assumed_duration_minutes = duration_minutes,
    total_minutes = nrow(minute_grid),
    covered_minutes = sum(minute_grid$covered),
    uncovered_minutes = sum(!minute_grid$covered),
    coverage_percentage =
      mean(minute_grid$covered) * 100,
    minimum_active_events =
      min(minute_grid$active_events),
    maximum_active_events =
      max(minute_grid$active_events),
    minimum_active_countries =
      min(minute_grid$active_countries)
  )
  
  list(
    minutes = minute_grid,
    summary = summary
  )
}


# ============================================================
# 4. CALCULATE ONE-MINUTE AND FIVE-MINUTE SCENARIOS
# ============================================================

one_minute_result <- calculate_minute_support(
  events,
  duration_minutes = 1
)

five_minute_result <- calculate_minute_support(
  events,
  duration_minutes = 5
)

minute_one <- one_minute_result$minutes
minute_five <- five_minute_result$minutes

sensitivity_summary <- rbind(
  one_minute_result$summary,
  five_minute_result$summary
)

cat("\nCOVERAGE SUMMARY\n")
print(sensitivity_summary)


# ============================================================
# 5. CREATE OUTPUT FOLDER
# ============================================================

output_folder <- "plots/hypothesis_visualizations"

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 6. VISUALIZATION A: UTC MINUTE COVERAGE HEATMAP
# ============================================================

minute_one[, day_label :=
             factor(
               day_label,
               levels = unique(day_label)
             )
]

plot_a <- ggplot(
  minute_one,
  aes(
    x = minute_of_day,
    y = day_label,
    fill = covered
  )
) +
  geom_tile() +
  scale_fill_manual(
    values = c(
      "FALSE" = "#D94B4B",
      "TRUE" = "#159D78"
    ),
    labels = c(
      "FALSE" = "No modelled coverage",
      "TRUE" = "Modelled coverage"
    ),
    name = "Minute status"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1320, by = 120),
    labels = function(x) {
      sprintf("%02d:00", as.integer(x / 60))
    }
  ) +
  coord_cartesian(
    xlim = c(-0.5, 1439.5)
  ) +
  labs(
    title = "Global Adhan Clock: UTC Minute Coverage",
    subtitle = paste(
      "One-minute assumed event duration |",
      "10–16 November 2026"
    ),
    x = "Time of day (UTC)",
    y = "UTC date",
    caption = paste(
      "Modelled intervals from calculated prayer times;",
      "not verified actual Adhan broadcasts."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

ggsave(
  file.path(
    output_folder,
    "01_utc_minute_coverage_heatmap.png"
  ),
  plot_a,
  width = 16,
  height = 5.5,
  dpi = 300
)


# ============================================================
# 7. VISUALIZATION B: GLOBAL EVENT ACTIVITY
# ============================================================

plot_b <- ggplot(
  minute_one,
  aes(
    x = minute_time,
    y = active_events
  )
) +
  geom_line(
    color = "#1677C8",
    linewidth = 0.35
  ) +
  labs(
    title = "Global Calculated Event Activity",
    subtitle = paste(
      "Number of one-minute event intervals",
      "overlapping each UTC minute"
    ),
    x = "Date and time (UTC)",
    y = "Number of overlapping events",
    caption = "Calculated events, not verified broadcasts."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    )
  )

ggsave(
  file.path(
    output_folder,
    "02_global_event_activity.png"
  ),
  plot_b,
  width = 16,
  height = 6,
  dpi = 300
)


# ============================================================
# 8. VISUALIZATION C: COUNTRY DIVERSITY
# ============================================================

plot_c <- ggplot(
  minute_one,
  aes(
    x = minute_time,
    y = active_countries
  )
) +
  geom_line(
    color = "#8E5AC7",
    linewidth = 0.4
  ) +
  labs(
    title = "Geographic Diversity of Minute Coverage",
    subtitle = paste(
      "Distinct countries represented by",
      "overlapping calculated events"
    ),
    x = "Date and time (UTC)",
    y = "Number of countries",
    caption = "Country counts describe modelled event coverage."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    )
  )

ggsave(
  file.path(
    output_folder,
    "03_country_diversity.png"
  ),
  plot_c,
  width = 16,
  height = 6,
  dpi = 300
)


# ============================================================
# 9. VISUALIZATION D: GLOBAL RESEARCH LOCATIONS
# ============================================================

places <- unique(
  events[, .(
    geoname_id,
    name,
    country_code,
    latitude,
    longitude
  )]
)

world_map <- ggplot2::map_data("world")

plot_d <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    fill = "grey95",
    color = "grey75",
    linewidth = 0.15
  ) +
  geom_point(
    data = places,
    aes(
      x = longitude,
      y = latitude
    ),
    color = "#1677C8",
    alpha = 0.55,
    size = 0.65
  ) +
  coord_quickmap() +
  labs(
    title = "Global Research Locations",
    subtitle = paste(
      nrow(places),
      "selected populated places used in the calculation"
    ),
    x = "Longitude",
    y = "Latitude",
    caption = paste(
      "Calculation locations, not confirmed mosque",
      "or broadcast locations."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    panel.grid = element_blank()
  )

ggsave(
  file.path(
    output_folder,
    "04_global_research_locations.png"
  ),
  plot_d,
  width = 14,
  height = 8,
  dpi = 300
)


# ============================================================
# 10. SAVE VISUALIZATION DATA
# ============================================================

fwrite(
  minute_one,
  "data/processed/visualization_minute_support_1min.csv"
)

fwrite(
  minute_five,
  "data/processed/visualization_minute_support_5min.csv"
)

fwrite(
  sensitivity_summary,
  "data/processed/visualization_coverage_summary.csv"
)


# ============================================================
# 11. FINAL REPORT
# ============================================================

cat("\n")
cat("============================================================\n")
cat("SCRIPT 31 COMPLETED\n")
cat("============================================================\n\n")

cat("Research locations:", nrow(places), "\n")
cat("Research events:", nrow(events), "\n")
cat("UTC minutes examined:", total_minutes, "\n\n")

cat("ONE-MINUTE SCENARIO\n")
cat("Covered:", sum(minute_one$covered), "\n")
cat("Uncovered:", sum(!minute_one$covered), "\n")
cat(
  "Coverage:",
  round(mean(minute_one$covered) * 100, 2),
  "%\n\n"
)

cat("FIVE-MINUTE SCENARIO\n")
cat("Covered:", sum(minute_five$covered), "\n")
cat("Uncovered:", sum(!minute_five$covered), "\n")
cat(
  "Coverage:",
  round(mean(minute_five$covered) * 100, 2),
  "%\n\n"
)

cat("VISUALIZATIONS SAVED IN:\n")
cat(output_folder, "\n\n")

cat("IMPORTANT SCIENTIFIC NOTE:\n")
cat(
  "These figures visualize calculated prayer-time events\n",
  "and assumed intervals. They do not establish actual\n",
  "Adhan broadcasts in every UTC minute.\n"
)

cat("\n============================================================\n")
