
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 32: GLOBAL COVERAGE WEAK-POINT INVESTIGATION
# ============================================================

library(data.table)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

cat("\n")
cat("============================================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 32: COVERAGE WEAK-POINT INVESTIGATION\n")
cat("============================================================\n\n")


# ============================================================
# 1. RESEARCH WINDOW AND SETTINGS
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

assumed_duration_minutes <- 1

total_minutes <- as.integer(
  (end_epoch - start_epoch) / 60
)

cat("Research start:", format(research_start), "UTC\n")
cat("Research end:", format(research_end), "UTC\n")
cat("UTC minutes:", total_minutes, "\n")
cat("Assumed event duration:", assumed_duration_minutes, "minute\n\n")


# ============================================================
# 2. LOAD EVENT DATA
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
cat("Unique places:", uniqueN(events$geoname_id), "\n")
cat("Countries:", uniqueN(events$country_code), "\n\n")


# ============================================================
# 3. CREATE ONE-MINUTE EVENT INTERVALS
# ============================================================

events[, interval_end :=
         event_epoch + assumed_duration_minutes * 60
]

events[, clipped_start :=
         pmax(event_epoch, start_epoch)
]

events[, clipped_end :=
         pmin(interval_end, end_epoch)
]

events <- events[
  clipped_end > clipped_start
]

events[, first_minute :=
         floor(clipped_start / 60) * 60
]

events[, last_minute :=
         ceiling(clipped_end / 60) * 60 - 60
]

cat("Events inside research window:", nrow(events), "\n")


# ============================================================
# 4. EXPAND EVENTS INTO UTC MINUTES
# ============================================================

cat("\nBuilding event-minute records...\n")

minute_links <- events[
  ,
  .(
    minute_epoch = seq(
      first_minute,
      last_minute,
      by = 60
    )
  ),
  by = .(
    event_id,
    geoname_id,
    name,
    country_code,
    prayer
  )
]

cat("Event-minute records:", nrow(minute_links), "\n")


# ============================================================
# 5. BUILD COMPLETE UTC MINUTE GRID
# ============================================================

minute_grid <- data.table(
  minute_epoch = seq(
    start_epoch,
    end_epoch - 60,
    by = 60
  )
)

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

minute_grid[, utc_clock :=
              format(minute_time, "%H:%M:%S", tz = "UTC")
]


# ============================================================
# 6. COUNT TOTAL EVENTS PER MINUTE
# ============================================================

cat("\nCalculating total event support per minute...\n")

total_support <- minute_links[
  ,
  .(
    active_events = uniqueN(event_id),
    active_countries = uniqueN(country_code),
    active_places = uniqueN(geoname_id)
  ),
  by = minute_epoch
]

minute_grid <- merge(
  minute_grid,
  total_support,
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

minute_grid[
  is.na(active_places),
  active_places := 0L
]

minute_grid[, covered :=
              active_events > 0
]

setorder(
  minute_grid,
  active_events,
  minute_epoch
)

cat("Minimum events per minute:",
    min(minute_grid$active_events), "\n")

cat("Maximum events per minute:",
    max(minute_grid$active_events), "\n")

cat("Uncovered minutes:",
    sum(!minute_grid$covered), "\n")


# ============================================================
# 7. IDENTIFY THE WEAKEST UTC MINUTES
# ============================================================

cat("\nIdentifying weakest UTC minutes...\n")

weakest_minutes <- minute_grid[
  order(active_events, minute_epoch)
][1:min(100, .N)]

cat("\nTOP 20 WEAKEST UTC MINUTES\n")

print(
  weakest_minutes[
    1:min(20, .N),
    .(
      minute_time,
      active_events,
      active_countries,
      active_places
    )
  ]
)


# ============================================================
# 8. IDENTIFY COUNTRIES SUPPORTING WEAK MINUTES
# ============================================================

cat("\nIdentifying contributing countries...\n")

weakest_country_support <- minute_links[
  minute_epoch %in% weakest_minutes$minute_epoch,
  .(
    supporting_events = uniqueN(event_id),
    supporting_places = uniqueN(geoname_id)
  ),
  by = .(minute_epoch, country_code)
]

weakest_country_support <- merge(
  weakest_country_support,
  weakest_minutes[
    ,
    .(
      minute_epoch,
      minute_time,
      active_events,
      active_countries
    )
  ],
  by = "minute_epoch",
  all.x = TRUE
)

setorder(
  weakest_country_support,
  minute_epoch,
  -supporting_events
)

cat("\nCOUNTRIES SUPPORTING THE WEAKEST MINUTES\n")

print(
  weakest_country_support[
    1:min(30, .N)
  ]
)


# ============================================================
# 9. COUNTRY REMOVAL EXPERIMENT
# ============================================================

cat("\n============================================================\n")
cat("COUNTRY REMOVAL EXPERIMENT\n")
cat("============================================================\n")

country_minute_support <- minute_links[
  ,
  .(
    country_events = uniqueN(event_id)
  ),
  by = .(minute_epoch, country_code)
]

country_minute_support <- merge(
  country_minute_support,
  total_support[
    ,
    .(
      minute_epoch,
      total_events = active_events
    )
  ],
  by = "minute_epoch"
)

country_minute_support[, remaining_events :=
                         total_events - country_events
]

country_removal_results <- country_minute_support[
  remaining_events == 0,
  .(
    minutes_becoming_uncovered = uniqueN(minute_epoch)
  ),
  by = country_code
]

all_countries <- unique(
  events$country_code
)

country_removal_results <- merge(
  data.table(country_code = all_countries),
  country_removal_results,
  by = "country_code",
  all.x = TRUE
)

country_removal_results[
  is.na(minutes_becoming_uncovered),
  minutes_becoming_uncovered := 0L
]

setorder(
  country_removal_results,
  -minutes_becoming_uncovered,
  country_code
)

cat("\nCOUNTRIES WITH THE LARGEST REMOVAL IMPACT\n")

print(
  country_removal_results[
    1:min(20, .N)
  ]
)


# ============================================================
# 10. POPULATED PLACE REMOVAL EXPERIMENT
# ============================================================

cat("\n============================================================\n")
cat("POPULATED PLACE REMOVAL EXPERIMENT\n")
cat("============================================================\n")

place_minute_support <- minute_links[
  ,
  .(
    place_events = uniqueN(event_id)
  ),
  by = .(minute_epoch, geoname_id)
]

place_minute_support <- merge(
  place_minute_support,
  total_support[
    ,
    .(
      minute_epoch,
      total_events = active_events
    )
  ],
  by = "minute_epoch"
)

place_minute_support[, remaining_events :=
                       total_events - place_events
]

place_removal_results <- place_minute_support[
  remaining_events == 0,
  .(
    minutes_becoming_uncovered = uniqueN(minute_epoch)
  ),
  by = geoname_id
]

place_information <- unique(
  events[
    ,
    .(
      geoname_id,
      name,
      country_code,
      latitude,
      longitude
    )
  ]
)

place_removal_results <- merge(
  place_information,
  place_removal_results,
  by = "geoname_id",
  all.x = TRUE
)

place_removal_results[
  is.na(minutes_becoming_uncovered),
  minutes_becoming_uncovered := 0L
]

setorder(
  place_removal_results,
  -minutes_becoming_uncovered,
  name
)

cat("\nPLACES WITH THE LARGEST REMOVAL IMPACT\n")

print(
  place_removal_results[
    1:min(20, .N)
  ]
)


# ============================================================
# 11. CREATE OUTPUT DIRECTORY
# ============================================================

output_folder <- "data/processed"

plot_folder <- "plots/hypothesis_visualizations"

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  plot_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 12. VISUALIZATION A: EVENT SUPPORT DISTRIBUTION
# ============================================================

plot_a <- ggplot(
  minute_grid,
  aes(x = active_events)
) +
  geom_histogram(
    bins = 40,
    fill = "#1677C8",
    color = "white"
  ) +
  labs(
    title = "Distribution of Global Event Support",
    subtitle = "Number of calculated events overlapping each UTC minute",
    x = "Overlapping calculated events",
    y = "Number of UTC minutes"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    plot_folder,
    "05_event_support_distribution.png"
  ),
  plot_a,
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# 13. VISUALIZATION B: WEAKEST MINUTES
# ============================================================

plot_b <- ggplot(
  weakest_minutes,
  aes(
    x = minute_time,
    y = active_events
  )
) +
  geom_point(
    color = "#D94B4B",
    size = 1.2
  ) +
  labs(
    title = "The Weakest UTC Minutes",
    subtitle = "100 minutes with the fewest overlapping calculated events",
    x = "UTC time",
    y = "Overlapping calculated events"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    plot_folder,
    "06_weakest_utc_minutes.png"
  ),
  plot_b,
  width = 14,
  height = 6,
  dpi = 300
)


# ============================================================
# 14. VISUALIZATION C: COUNTRY REMOVAL IMPACT
# ============================================================

top_country_impacts <- country_removal_results[
  order(-minutes_becoming_uncovered)
][1:min(20, .N)]

plot_c <- ggplot(
  top_country_impacts,
  aes(
    x = reorder(country_code, minutes_becoming_uncovered),
    y = minutes_becoming_uncovered
  )
) +
  geom_col(
    fill = "#8E5AC7"
  ) +
  coord_flip() +
  labs(
    title = "Country Removal Impact",
    subtitle = "UTC minutes that lose all modelled event support when one country is removed",
    x = "Country code",
    y = "Minutes becoming uncovered"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    plot_folder,
    "07_country_removal_impact.png"
  ),
  plot_c,
  width = 12,
  height = 7,
  dpi = 300
)


# ============================================================
# 15. VISUALIZATION D: PLACE REMOVAL IMPACT
# ============================================================

top_place_impacts <- place_removal_results[
  order(-minutes_becoming_uncovered)
][1:min(20, .N)]

plot_d <- ggplot(
  top_place_impacts,
  aes(
    x = reorder(name, minutes_becoming_uncovered),
    y = minutes_becoming_uncovered
  )
) +
  geom_col(
    fill = "#159D78"
  ) +
  coord_flip() +
  labs(
    title = "Populated Place Removal Impact",
    subtitle = "UTC minutes that lose all modelled support when one place is removed",
    x = "Populated place",
    y = "Minutes becoming uncovered"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(
    plot_folder,
    "08_place_removal_impact.png"
  ),
  plot_d,
  width = 12,
  height = 7,
  dpi = 300
)


# ============================================================
# 16. SAVE RESEARCH OUTPUTS
# ============================================================

fwrite(
  minute_grid,
  file.path(
    output_folder,
    "weakpoint_minute_support.csv"
  )
)

fwrite(
  weakest_minutes,
  file.path(
    output_folder,
    "weakest_utc_minutes.csv"
  )
)

fwrite(
  weakest_country_support,
  file.path(
    output_folder,
    "weakest_minute_country_support.csv"
  )
)

fwrite(
  country_removal_results,
  file.path(
    output_folder,
    "country_removal_impact.csv"
  )
)

fwrite(
  place_removal_results,
  file.path(
    output_folder,
    "place_removal_impact.csv"
  )
)


# ============================================================
# 17. FINAL RESEARCH REPORT
# ============================================================

cat("\n")
cat("============================================================\n")
cat("SCRIPT 32 COMPLETED\n")
cat("============================================================\n\n")

cat("UTC minutes examined:", nrow(minute_grid), "\n")

cat(
  "Covered minutes:",
  sum(minute_grid$covered),
  "\n"
)

cat(
  "Uncovered minutes:",
  sum(!minute_grid$covered),
  "\n"
)

cat(
  "Minimum events in a minute:",
  min(minute_grid$active_events),
  "\n"
)

cat(
  "Maximum events in a minute:",
  max(minute_grid$active_events),
  "\n"
)

cat(
  "Countries tested:",
  nrow(country_removal_results),
  "\n"
)

cat(
  "Populated places tested:",
  nrow(place_removal_results),
  "\n"
)

cat("\nOUTPUT FILES SAVED\n")

cat("data/processed/weakpoint_minute_support.csv\n")
cat("data/processed/weakest_utc_minutes.csv\n")
cat("data/processed/weakest_minute_country_support.csv\n")
cat("data/processed/country_removal_impact.csv\n")
cat("data/processed/place_removal_impact.csv\n")

cat("\nVISUALIZATIONS SAVED\n")

cat("plots/hypothesis_visualizations/05_event_support_distribution.png\n")
cat("plots/hypothesis_visualizations/06_weakest_utc_minutes.png\n")
cat("plots/hypothesis_visualizations/07_country_removal_impact.png\n")
cat("plots/hypothesis_visualizations/08_place_removal_impact.png\n")

cat("\nSCIENTIFIC NOTE:\n")

cat(
  "This experiment tests the robustness of calculated\n",
  "prayer-time event coverage. It does not establish\n",
  "actual Adhan broadcasts.\n"
)

cat("\n============================================================\n")
