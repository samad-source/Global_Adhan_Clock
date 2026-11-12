
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 33: GLOBAL GEOGRAPHICAL AND HOURLY ACTIVITY ANALYSIS
# ============================================================

library(data.table)
library(ggplot2)

if (!requireNamespace("countrycode", quietly = TRUE)) {
  install.packages("countrycode")
}

library(countrycode)

cat("\n")
cat("============================================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 33: GLOBAL ACTIVITY ANALYSIS\n")
cat("============================================================\n\n")


# ============================================================
# 1. DEFINE RESEARCH PERIOD
# ============================================================

research_start <- as.POSIXct(
  "2026-11-10 00:00:00",
  tz = "UTC"
)

research_end <- as.POSIXct(
  "2026-11-17 00:00:00",
  tz = "UTC"
)

research_days <- 7

cat("Research start:", format(research_start, tz = "UTC"), "UTC\n")
cat("Research end:", format(research_end, tz = "UTC"), "UTC\n")
cat("Research days:", research_days, "\n\n")


# ============================================================
# 2. LOAD EXISTING GLOBAL EVENT DATA
# ============================================================

input_file <- "data/processed/global_prayer_events_utc.csv"

if (!file.exists(input_file)) {
  stop("Global prayer event file was not found: ", input_file)
}

events <- fread(input_file)

required_columns <- c(
  "event_id",
  "geoname_id",
  "name",
  "country_code",
  "prayer",
  "event_epoch",
  "latitude",
  "longitude"
)

missing_columns <- setdiff(
  required_columns,
  names(events)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

cat("Events loaded:", nrow(events), "\n")
cat("Unique places:", uniqueN(events$geoname_id), "\n")
cat("Countries:", uniqueN(events$country_code), "\n\n")


# ============================================================
# 3. FILTER RESEARCH WINDOW
# ============================================================

events[, event_epoch := as.numeric(event_epoch)]

events <- events[
  !is.na(event_epoch) &
    event_epoch >= as.numeric(research_start) &
    event_epoch < as.numeric(research_end)
]

events <- events[
  !is.na(latitude) &
    !is.na(longitude) &
    latitude >= -90 &
    latitude <= 90 &
    longitude >= -180 &
    longitude <= 180
]

if (nrow(events) == 0) {
  stop("No valid events found inside the research window.")
}

cat("Valid events inside research window:", nrow(events), "\n\n")


# ============================================================
# 4. CREATE UTC DATE AND HOUR VARIABLES
# ============================================================

event_datetime <- as.POSIXct(
  events$event_epoch,
  origin = "1970-01-01",
  tz = "UTC"
)

events[, `:=`(
  utc_date = as.IDate(event_datetime, tz = "UTC"),
  utc_hour = as.integer(format(event_datetime, "%H", tz = "UTC"))
)]

cat("UTC date and hour variables created.\n\n")


# ============================================================
# 5. CONTINENT CLASSIFICATION
# ============================================================

# countrycode returns continent names:
# Africa, Americas, Asia, Europe, Oceania.
#
# North and South America are grouped as Americas because
# this is the classification returned by countrycode.

events[, continent := countrycode::countrycode(
  country_code,
  origin = "iso2c",
  destination = "continent",
  nomatch = "Unknown"
)]

events[
  is.na(continent) | continent == "",
  continent := "Unknown"
]

cat("Continental classification completed.\n\n")

print(
  events[, .N, by = continent][order(-N)]
)


# ============================================================
# 6. ANALYZE ACTIVITY BY UTC HOUR
# ============================================================

hourly_activity <- events[
  ,
  .(
    calculated_events = .N,
    active_countries = uniqueN(country_code),
    active_places = uniqueN(geoname_id)
  ),
  by = utc_hour
]

all_hours <- data.table(
  utc_hour = 0:23
)

hourly_activity <- merge(
  all_hours,
  hourly_activity,
  by = "utc_hour",
  all.x = TRUE
)

hourly_activity[
  is.na(calculated_events),
  `:=`(
    calculated_events = 0L,
    active_countries = 0L,
    active_places = 0L
  )
]

hourly_activity[
  ,
  average_events_per_day := calculated_events / research_days
]

setorder(hourly_activity, utc_hour)

fwrite(
  hourly_activity,
  "data/processed/hourly_global_activity.csv"
)

cat("\n============================================================\n")
cat("UTC HOURLY ACTIVITY\n")
cat("============================================================\n")

print(hourly_activity)

cat(
  "\nBusiest UTC hour:",
  sprintf(
    "%02d:00",
    hourly_activity[which.max(calculated_events), utc_hour]
  ),
  "\n"
)

cat(
  "Quietest UTC hour:",
  sprintf(
    "%02d:00",
    hourly_activity[which.min(calculated_events), utc_hour]
  ),
  "\n\n"
)


# ============================================================
# 7. ANALYZE ACTIVITY BY CONTINENT
# ============================================================

continent_activity <- events[
  ,
  .(
    calculated_events = .N,
    countries = uniqueN(country_code),
    populated_places = uniqueN(geoname_id),
    active_days = uniqueN(utc_date)
  ),
  by = continent
]

continent_activity[
  ,
  events_per_place := calculated_events / populated_places
]

continent_activity[
  ,
  percentage_of_global_events :=
    100 * calculated_events / sum(calculated_events)
]

setorder(continent_activity, -calculated_events)

fwrite(
  continent_activity,
  "data/processed/continental_global_activity.csv"
)

cat("\n============================================================\n")
cat("CONTINENTAL ACTIVITY\n")
cat("============================================================\n")

print(continent_activity)


# ============================================================
# 8. ANALYZE DAILY ACTIVITY BY CONTINENT
# ============================================================

daily_continent_activity <- events[
  ,
  .(
    calculated_events = .N,
    active_places = uniqueN(geoname_id),
    active_countries = uniqueN(country_code)
  ),
  by = .(utc_date, continent)
]

setorder(
  daily_continent_activity,
  utc_date,
  continent
)

fwrite(
  daily_continent_activity,
  "data/processed/daily_continental_activity.csv"
)

cat("\nDaily continental activity calculated.\n")


# ============================================================
# 9. ANALYZE COUNTRY CONTRIBUTIONS
# ============================================================

country_activity <- events[
  ,
  .(
    calculated_events = .N,
    populated_places = uniqueN(geoname_id),
    active_days = uniqueN(utc_date)
  ),
  by = country_code
]

country_activity[
  ,
  events_per_place := calculated_events / populated_places
]

country_activity[
  ,
  percentage_of_global_events :=
    100 * calculated_events / sum(calculated_events)
]

setorder(country_activity, -calculated_events)

fwrite(
  country_activity,
  "data/processed/country_global_activity.csv"
)

cat("\n============================================================\n")
cat("TOP 20 COUNTRIES BY CALCULATED EVENTS\n")
cat("============================================================\n")

print(country_activity[1:min(20, .N)])


# ============================================================
# 10. INVESTIGATE LOW-ACTIVITY COUNTRIES
# ============================================================

# Only include countries with at least 10 selected places.
# This reduces misleading comparisons from very small samples.

low_activity_countries <- country_activity[
  populated_places >= 10
]

setorder(
  low_activity_countries,
  events_per_place
)

fwrite(
  low_activity_countries,
  "data/processed/low_activity_countries.csv"
)

cat("\n============================================================\n")
cat("COUNTRIES WITH LOWEST EVENTS PER PLACE\n")
cat("(Minimum 10 selected populated places)\n")
cat("============================================================\n")

print(
  low_activity_countries[1:min(20, .N)]
)


# ============================================================
# 11. INVESTIGATE GEOGRAPHICAL ACTIVITY
# ============================================================

# Divide Earth into 10-degree latitude/longitude cells.
# These are analytical grid cells, not political boundaries.

events[
  ,
  latitude_cell := pmin(
    80,
    floor((latitude + 90) / 10) * 10 - 90
  )
]

events[
  ,
  longitude_cell := pmin(
    170,
    floor((longitude + 180) / 10) * 10 - 180
  )
]

geographical_activity <- events[
  ,
  .(
    calculated_events = .N,
    populated_places = uniqueN(geoname_id),
    countries = uniqueN(country_code)
  ),
  by = .(latitude_cell, longitude_cell)
]

geographical_activity[
  ,
  latitude_center := latitude_cell + 5
]

geographical_activity[
  ,
  longitude_center := longitude_cell + 5
]

geographical_activity[
  ,
  events_per_place := calculated_events / populated_places
]

setorder(
  geographical_activity,
  events_per_place
)

fwrite(
  geographical_activity,
  "data/processed/geographical_activity_grid.csv"
)

cat("\n============================================================\n")
cat("LOWEST ACTIVITY GEOGRAPHICAL CELLS\n")
cat("============================================================\n")

print(
  geographical_activity[1:min(20, .N)]
)


# ============================================================
# 12. CREATE VISUALIZATION DIRECTORY
# ============================================================

plot_directory <- "plots/hypothesis_visualizations"

if (!dir.exists(plot_directory)) {
  dir.create(
    plot_directory,
    recursive = TRUE
  )
}


# ============================================================
# 13. PLOT UTC HOURLY ACTIVITY
# ============================================================

plot_hourly <- ggplot(
  hourly_activity,
  aes(
    x = utc_hour,
    y = calculated_events
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 0:23
  ) +
  labs(
    title = "Global Calculated Prayer Events by UTC Hour",
    subtitle = "Seven-day research window",
    x = "UTC Hour",
    y = "Number of Calculated Events"
  ) +
  theme_minimal()

ggsave(
  file.path(
    plot_directory,
    "09_utc_hourly_activity.png"
  ),
  plot_hourly,
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# 14. PLOT CONTINENTAL ACTIVITY
# ============================================================

plot_continent <- ggplot(
  continent_activity,
  aes(
    x = reorder(continent, calculated_events),
    y = calculated_events
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Calculated Prayer Events by Continent",
    x = "Continent",
    y = "Number of Calculated Events"
  ) +
  theme_minimal()

ggsave(
  file.path(
    plot_directory,
    "10_continental_activity.png"
  ),
  plot_continent,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# 15. PLOT DAILY CONTINENTAL ACTIVITY
# ============================================================

plot_daily <- ggplot(
  daily_continent_activity,
  aes(
    x = utc_date,
    y = calculated_events,
    group = continent
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(
    ~ continent,
    scales = "free_y"
  ) +
  labs(
    title = "Daily Calculated Prayer Events by Continent",
    x = "UTC Date",
    y = "Calculated Events"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  file.path(
    plot_directory,
    "11_daily_continental_activity.png"
  ),
  plot_daily,
  width = 12,
  height = 8,
  dpi = 300
)


# ============================================================
# 16. PLOT TOP 20 COUNTRIES
# ============================================================

top_countries <- country_activity[
  1:min(20, .N)
]

plot_countries <- ggplot(
  top_countries,
  aes(
    x = reorder(country_code, calculated_events),
    y = calculated_events
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Countries by Calculated Prayer Events",
    x = "Country Code",
    y = "Calculated Events"
  ) +
  theme_minimal()

ggsave(
  file.path(
    plot_directory,
    "12_top_20_country_activity.png"
  ),
  plot_countries,
  width = 10,
  height = 8,
  dpi = 300
)


# ============================================================
# 17. PLOT GEOGRAPHICAL ACTIVITY GRID
# ============================================================

plot_geography <- ggplot(
  geographical_activity,
  aes(
    x = longitude_center,
    y = latitude_center,
    fill = events_per_place
  )
) +
  geom_tile() +
  coord_fixed() +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 60)
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30)
  ) +
  labs(
    title = "Global Calculated Prayer Activity by Geographic Grid",
    subtitle = "Average calculated events per selected populated place",
    x = "Longitude",
    y = "Latitude",
    fill = "Events per place"
  ) +
  theme_minimal()

ggsave(
  file.path(
    plot_directory,
    "13_geographical_activity_grid.png"
  ),
  plot_geography,
  width = 12,
  height = 7,
  dpi = 300
)


# ============================================================
# 18. FINAL RESEARCH SUMMARY
# ============================================================

cat("\n")
cat("============================================================\n")
cat("SCRIPT 33 COMPLETED\n")
cat("============================================================\n\n")

cat("Research window: 7 days\n")
cat("Calculated events:", nrow(events), "\n")
cat("Countries:", uniqueN(events$country_code), "\n")
cat("Populated places:", uniqueN(events$geoname_id), "\n")
cat("Continental groups:", uniqueN(events$continent), "\n")

cat(
  "\nBusiest UTC hour:",
  sprintf(
    "%02d:00",
    hourly_activity[which.max(calculated_events), utc_hour]
  ),
  "\n"
)

cat(
  "Quietest UTC hour:",
  sprintf(
    "%02d:00",
    hourly_activity[which.min(calculated_events), utc_hour]
  ),
  "\n"
)

cat("\nOUTPUT DATA FILES\n")

cat("data/processed/hourly_global_activity.csv\n")
cat("data/processed/continental_global_activity.csv\n")
cat("data/processed/daily_continental_activity.csv\n")
cat("data/processed/country_global_activity.csv\n")
cat("data/processed/low_activity_countries.csv\n")
cat("data/processed/geographical_activity_grid.csv\n")

cat("\nVISUALIZATIONS\n")

cat("09_utc_hourly_activity.png\n")
cat("10_continental_activity.png\n")
cat("11_daily_continental_activity.png\n")
cat("12_top_20_country_activity.png\n")
cat("13_geographical_activity_grid.png\n")

cat("\nSCIENTIFIC NOTE:\n")

cat(
  "These are calculated prayer-time events, not verified\n",
  "actual Adhan broadcasts. Results depend on the selected\n",
  "populated places and prayer-time conventions.\n"
)

cat("\n============================================================\n")
