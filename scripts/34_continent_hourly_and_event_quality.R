
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 34: CORRECTED CONTINENTAL AND EVENT QUALITY ANALYSIS
# ============================================================

library(data.table)
library(ggplot2)
library(countrycode)

# 1. PROJECT PATHS
# ============================================================

project_root <- "C:/Users/CDT WO AZEEZ SAMAD/Downloads/Global_Adhan_Clock"

event_file <- file.path(
  project_root,
  "data/processed/global_prayer_events_utc.csv"
)

date_aware_file <- file.path(
  project_root,
  "data/processed/global_prayer_times_date_aware.csv"
)

places_file <- file.path(
  project_root,
  "data/processed/application_places.csv"
)

output_dir <- file.path(project_root, "data/processed")

plot_dir <- file.path(
  project_root,
  "plots/hypothesis_visualizations"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)


# 2. RESEARCH WINDOW
# ============================================================

research_start <- as.POSIXct(
  "2026-11-10 00:00:00",
  tz = "UTC"
)

research_end <- as.POSIXct(
  "2026-11-17 00:00:00",
  tz = "UTC"
)

cat("\n============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 34: CORRECTED ANALYSIS\n")
cat("============================================\n")


# 3. LOAD DATA
# ============================================================

events <- fread(event_file)

date_aware <- fread(date_aware_file)

places <- fread(places_file)

cat("\nDATA LOADED\n")
cat("Event rows:", nrow(events), "\n")
cat("Date-aware rows:", nrow(date_aware), "\n")
cat("Geographical rows:", nrow(places), "\n")


# 4. FILTER EVENT TIMESTAMPS
# ============================================================

if (!"event_epoch" %in% names(events)) {
  stop("event_epoch column is missing from the event file.")
}

events <- events[
  !is.na(event_epoch) &
    event_epoch >= as.numeric(research_start) &
    event_epoch < as.numeric(research_end)
]

cat("\nEvents inside UTC window:", nrow(events), "\n")


# 5. RECONCILE EXPECTED AND RECORDED EVENTS
# ============================================================

required_columns <- c(
  "geoname_id",
  "local_date",
  "fajr_epoch",
  "dhuhr_epoch",
  "asr_epoch",
  "maghrib_epoch",
  "isha_epoch"
)

missing_columns <- setdiff(
  required_columns,
  names(date_aware)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing date-aware columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

prayer_columns <- c(
  "fajr_epoch",
  "dhuhr_epoch",
  "asr_epoch",
  "maghrib_epoch",
  "isha_epoch"
)

expected <- melt(
  date_aware,
  id.vars = c("geoname_id", "local_date"),
  measure.vars = prayer_columns,
  variable.name = "prayer_column",
  value.name = "event_epoch",
  variable.factor = FALSE
)

expected[, prayer := sub(
  "_epoch$",
  "",
  prayer_column
)]

expected[, event_epoch := as.numeric(event_epoch)]

expected <- expected[
  !is.na(event_epoch) &
    event_epoch >= as.numeric(research_start) &
    event_epoch < as.numeric(research_end)
]

expected[, event_id := paste(
  geoname_id,
  local_date,
  prayer,
  sep = "_"
)]

events[, event_id := paste(
  geoname_id,
  local_date,
  tolower(as.character(prayer)),
  sep = "_"
)]

expected_ids <- unique(expected$event_id)
recorded_ids <- unique(events$event_id)

missing_ids <- setdiff(expected_ids, recorded_ids)
unexpected_ids <- setdiff(recorded_ids, expected_ids)

missing_events <- expected[event_id %in% missing_ids]

unexpected_events <- events[event_id %in% unexpected_ids]

cat("\n============================================\n")
cat("EVENT QUALITY INVESTIGATION\n")
cat("============================================\n")

cat("Expected events:", length(expected_ids), "\n")
cat("Recorded events:", length(recorded_ids), "\n")
cat("Missing event IDs:", length(missing_ids), "\n")
cat("Unexpected event IDs:", length(unexpected_ids), "\n")

fwrite(
  missing_events,
  file.path(output_dir, "event_quality_missing_calculated_events.csv")
)

fwrite(
  unexpected_events,
  file.path(output_dir, "event_quality_unexpected_events.csv")
)

missing_by_prayer <- missing_events[
  ,
  .(missing_events = .N),
  by = prayer
]

fwrite(
  missing_by_prayer,
  file.path(output_dir, "event_quality_missing_by_prayer.csv")
)

missing_by_place <- missing_events[
  ,
  .(
    missing_events = .N,
    missing_prayers = paste(prayer, collapse = ", ")
  ),
  by = .(geoname_id, local_date)
]

fwrite(
  missing_by_place,
  file.path(output_dir, "event_quality_missing_by_place.csv")
)

cat("\nMISSING EVENTS BY PRAYER:\n")
print(missing_by_prayer)

cat("\nMISSING EVENTS BY PLACE:\n")
print(missing_by_place)


# 6. USE ONLY PLACES IN OUR RESEARCH EVENT SAMPLE
# ============================================================

if (!"country_code" %in% names(places)) {
  stop("country_code is missing from application_places.csv.")
}

if (!"geoname_id" %in% names(events)) {
  stop("geoname_id is missing from the event file.")
}

# These are the actual places represented in our events.

research_place_ids <- unique(events$geoname_id)

research_places <- unique(
  places[
    geoname_id %in% research_place_ids,
    .(geoname_id, country_code)
  ]
)

# Remove any existing country_code column from events.
# This prevents duplicate country_code.x / country_code.y columns.

if ("country_code" %in% names(events)) {
  events[, country_code := NULL]
}

events <- merge(
  events,
  research_places,
  by = "geoname_id",
  all.x = TRUE
)

# Classify countries into continents.

events[, continent := suppressWarnings(
  countrycode(
    country_code,
    origin = "iso2c",
    destination = "continent"
  )
)]

events[
  is.na(continent) | country_code == "",
  continent := "Unknown"
]

events[
  is.na(country_code),
  continent := "Unknown"
]

cat("\nCONTINENTS IN RESEARCH EVENTS:\n")
print(sort(unique(events$continent)))


# 7. COUNT RESEARCH PLACES PER CONTINENT
# ============================================================

place_continents <- unique(
  events[
    ,
    .(geoname_id, country_code, continent)
  ]
)

places_per_continent <- place_continents[
  ,
  .(selected_places = uniqueN(geoname_id)),
  by = continent
]

setorder(places_per_continent, continent)

cat("\nRESEARCH PLACES PER CONTINENT:\n")
print(places_per_continent)

cat(
  "\nTotal research places:",
  sum(places_per_continent$selected_places),
  "\n"
)


# 8. EXTRACT UTC HOUR
# ============================================================

events[, utc_datetime := as.POSIXct(
  event_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)]

events[, utc_hour := as.integer(
  format(utc_datetime, "%H", tz = "UTC")
)]

events[, utc_date := as.Date(
  utc_datetime,
  tz = "UTC"
)]


# 9. CALCULATE CONTINENTAL HOURLY ACTIVITY
# ============================================================

hourly_continent <- events[
  ,
  .(
    event_count = .N,
    active_places = uniqueN(geoname_id)
  ),
  by = .(continent, utc_hour)
]

hourly_continent <- merge(
  hourly_continent,
  places_per_continent,
  by = "continent",
  all.x = TRUE
)

hourly_continent[
  ,
  events_per_selected_place := event_count / selected_places
]


# 10. COMPLETE ALL 24 HOURS
# ============================================================

all_combinations <- CJ(
  continent = sort(unique(places_per_continent$continent)),
  utc_hour = 0:23
)

hourly_continent <- merge(
  all_combinations,
  hourly_continent,
  by = c("continent", "utc_hour"),
  all.x = TRUE
)

hourly_continent[
  is.na(event_count),
  event_count := 0L
]

hourly_continent[
  is.na(active_places),
  active_places := 0L
]

hourly_continent <- merge(
  hourly_continent,
  places_per_continent,
  by = "continent",
  suffixes = c("", "_lookup"),
  all.x = TRUE
)

hourly_continent[
  is.na(selected_places),
  selected_places := selected_places_lookup
]

hourly_continent[
  ,
  selected_places_lookup := NULL
]

hourly_continent[
  ,
  events_per_selected_place := fifelse(
    selected_places > 0,
    event_count / selected_places,
    NA_real_
  )
]

setorder(hourly_continent, continent, utc_hour)

fwrite(
  hourly_continent,
  file.path(
    output_dir,
    "continental_hourly_activity_normalized.csv"
  )
)


# 11. BUSIEST AND QUIETEST HOURS
# ============================================================

continent_extremes <- hourly_continent[
  ,
  {
    max_count <- max(event_count)
    min_count <- min(event_count)
    
    .(
      busiest_utc_hours = paste(
        utc_hour[event_count == max_count],
        collapse = ", "
      ),
      busiest_event_count = max_count,
      
      quietest_utc_hours = paste(
        utc_hour[event_count == min_count],
        collapse = ", "
      ),
      quietest_event_count = min_count
    )
  },
  by = continent
]

setorder(continent_extremes, continent)

fwrite(
  continent_extremes,
  file.path(
    output_dir,
    "continental_busiest_quietest_hours.csv"
  )
)

cat("\nCONTINENTAL BUSIEST AND QUIETEST HOURS:\n")
print(continent_extremes)


# 12. CONTINENTAL SUMMARY
# ============================================================

continent_summary <- events[
  ,
  .(
    total_events = .N,
    selected_places = uniqueN(geoname_id),
    active_countries = uniqueN(country_code),
    average_events_per_place = .N / uniqueN(geoname_id)
  ),
  by = continent
]

setorder(continent_summary, -total_events)

fwrite(
  continent_summary,
  file.path(
    output_dir,
    "continental_event_summary_script34.csv"
  )
)

cat("\nCONTINENTAL SUMMARY:\n")
print(continent_summary)


# 13. NORMALIZED CONTINENT-HOUR HEATMAP
# ============================================================

heatmap_plot <- ggplot(
  hourly_continent,
  aes(
    x = factor(utc_hour),
    y = continent,
    fill = events_per_selected_place
  )
) +
  geom_tile(color = "white") +
  scale_x_discrete(
    breaks = as.character(seq(0, 23, 2))
  ) +
  labs(
    title = "Global Calculated Prayer Activity by Continent",
    subtitle = "Events per selected research place across seven UTC days",
    x = "UTC Hour",
    y = "Continent",
    fill = "Events per\nselected place"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  file.path(plot_dir, "14_continent_hourly_heatmap.png"),
  heatmap_plot,
  width = 12,
  height = 6,
  dpi = 300
)


# 14. RAW EVENT COUNT HEATMAP
# ============================================================

raw_heatmap <- ggplot(
  hourly_continent,
  aes(
    x = factor(utc_hour),
    y = continent,
    fill = event_count
  )
) +
  geom_tile(color = "white") +
  scale_x_discrete(
    breaks = as.character(seq(0, 23, 2))
  ) +
  labs(
    title = "Raw Calculated Prayer Event Counts",
    subtitle = "Raw counts depend partly on research sample size",
    x = "UTC Hour",
    y = "Continent",
    fill = "Event count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  file.path(plot_dir, "15_continent_hourly_raw_counts.png"),
  raw_heatmap,
  width = 12,
  height = 6,
  dpi = 300
)


# 15. FINAL REPORT
# ============================================================

cat("\n============================================\n")
cat("SCRIPT 34 FINAL REPORT\n")
cat("============================================\n")

cat("\nExpected event IDs:", length(expected_ids))
cat("\nRecorded event IDs:", length(recorded_ids))
cat("\nMissing event IDs:", length(missing_ids))
cat("\nUnexpected event IDs:", length(unexpected_ids))

cat("\nResearch places:", uniqueN(events$geoname_id))
cat("\nCountries:", uniqueN(events$country_code))
cat("\nContinents:", uniqueN(events$continent))
cat("\nEvents:", nrow(events))

cat("\n\nOUTPUTS SAVED:")
cat("\n- event_quality_missing_calculated_events.csv")
cat("\n- event_quality_unexpected_events.csv")
cat("\n- event_quality_missing_by_prayer.csv")
cat("\n- event_quality_missing_by_place.csv")
cat("\n- continental_hourly_activity_normalized.csv")
cat("\n- continental_busiest_quietest_hours.csv")
cat("\n- continental_event_summary_script34.csv")

cat("\n\nGRAPHS SAVED:")
cat("\n- 14_continent_hourly_heatmap.png")
cat("\n- 15_continent_hourly_raw_counts.png")

cat("\n\n============================================\n")
cat("SCRIPT 34 COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat(
  "\nSCIENTIFIC REMINDER:",
  "\nThese are calculated prayer times.",
  "\nThey are not verified actual Adhan broadcasts.",
  "\nThe original hypothesis remains unproven.\n"
)
