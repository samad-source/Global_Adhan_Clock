
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 27: DATE-AWARE GLOBAL PRAYER CALCULATIONS
# ============================================================


# 1. Load required package

library(data.table)


# 2. Load our prayer-time calculation engine

source("R/prayer_times.R")


# 3. Define the research period

research_start_date <- "2026-11-10"

research_days <- 7


# 4. Define input and output files

input_file <- paste0(
  "data/processed/",
  "global_prayer_times.csv"
)

output_file <- paste0(
  "data/processed/",
  "global_prayer_times_date_aware.csv"
)

summary_file <- paste0(
  "data/processed/",
  "date_aware_prayer_summary.csv"
)


# 5. Check the input file

if (!file.exists(input_file)) {
  stop("Run Script 25 first.")
}


# 6. Load our global locations

places <- fread(input_file)


# Keep one record per geographical location.

location_columns <- c(
  "geoname_id",
  "name",
  "country_code",
  "latitude",
  "longitude",
  "timezone"
)


places <- unique(
  places[, ..location_columns],
  by = "geoname_id"
)


# 7. Define the UTC research window

utc_start <- as.POSIXct(
  paste(research_start_date, "00:00:00"),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)


if (is.na(utc_start)) {
  stop("Invalid research start date.")
}


utc_end <- utc_start + research_days * 86400


utc_start_epoch <- as.numeric(utc_start)

utc_end_epoch <- as.numeric(utc_end)


# 8. Display research settings

cat("\n")
cat("============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("DATE-AWARE GLOBAL PRAYER CALCULATIONS\n")
cat("============================================\n\n")


cat("UTC research start:", research_start_date, "\n")

cat("Number of research days:", research_days, "\n")

cat(
  "UTC research end:",
  format(utc_end, "%Y-%m-%d", tz = "UTC"),
  "\n"
)

cat(
  "Locations:",
  format(nrow(places), big.mark = ","),
  "\n\n"
)


# 9. Generate the local calendar dates for each location
#
# We include a one-day buffer before and after
# the UTC research window.
#
# This helps us capture events near UTC midnight.

date_tasks <- vector(
  "list",
  nrow(places)
)


for (i in seq_len(nrow(places))) {
  
  timezone <- places$timezone[i]
  
  
  if (
    is.na(timezone) ||
    !timezone %in% OlsonNames()
  ) {
    
    next
    
  }
  
  
  local_start_date <- as.Date(
    format(
      utc_start - 86400,
      "%Y-%m-%d",
      tz = timezone
    )
  )
  
  
  local_end_date <- as.Date(
    format(
      utc_end + 86400,
      "%Y-%m-%d",
      tz = timezone
    )
  )
  
  
  location_dates <- seq.Date(
    from = local_start_date,
    to = local_end_date,
    by = "day"
  )
  
  
  date_tasks[[i]] <- data.table(
    
    place_index = i,
    
    local_date = as.character(location_dates)
    
  )
  
}


# Remove locations that could not generate date tasks.

date_tasks <- rbindlist(
  date_tasks,
  fill = TRUE
)


if (nrow(date_tasks) == 0) {
  stop("No valid location-date tasks were generated.")
}


cat(
  "Location-date calculations required:",
  format(nrow(date_tasks), big.mark = ","),
  "\n\n"
)


# 10. Calculate all five prayers for every local date

results <- vector(
  "list",
  nrow(date_tasks)
)


for (i in seq_len(nrow(date_tasks))) {
  
  task <- date_tasks[i]
  
  place <- places[task$place_index]
  
  
  calculation <- tryCatch(
    
    {
      
      calculate_prayer_times_for_place(
        
        research_date = task$local_date,
        
        latitude = place$latitude,
        
        longitude = place$longitude,
        
        timezone = place$timezone
        
      )
      
    },
    
    
    error = function(e) {
      
      list(
        
        prayer_status = "error",
        
        calculation_error = conditionMessage(e)
        
      )
      
    }
    
  )
  
  
  # Combine location identity with its calculated times.
  
  results[[i]] <- cbind(
    
    data.table(
      
      geoname_id = place$geoname_id,
      
      name = place$name,
      
      country_code = place$country_code,
      
      latitude = place$latitude,
      
      longitude = place$longitude,
      
      timezone = place$timezone,
      
      local_date = task$local_date
      
    ),
    
    as.data.table(calculation)
    
  )
  
  
  # Display progress.
  
  if (
    i %% 5000 == 0 ||
    i == nrow(date_tasks)
  ) {
    
    cat(
      
      "Processed:",
      
      format(i, big.mark = ","),
      
      "of",
      
      format(nrow(date_tasks), big.mark = ","),
      
      "location-date calculations\n"
      
    )
    
  }
  
}


# 11. Combine all calculation results

global_date_aware <- rbindlist(
  results,
  fill = TRUE
)


# 12. Mark prayer events falling inside the UTC research window
#
# An event belongs to the research window if:
#
# event timestamp >= UTC start
# event timestamp < UTC end


global_date_aware[
  
  ,
  
  fajr_in_utc_window :=
    
    !is.na(fajr_epoch) &
    
    fajr_epoch >= utc_start_epoch &
    
    fajr_epoch < utc_end_epoch
  
]


global_date_aware[
  
  ,
  
  dhuhr_in_utc_window :=
    
    !is.na(dhuhr_epoch) &
    
    dhuhr_epoch >= utc_start_epoch &
    
    dhuhr_epoch < utc_end_epoch
  
]


global_date_aware[
  
  ,
  
  asr_in_utc_window :=
    
    !is.na(asr_epoch) &
    
    asr_epoch >= utc_start_epoch &
    
    asr_epoch < utc_end_epoch
  
]


global_date_aware[
  
  ,
  
  maghrib_in_utc_window :=
    
    !is.na(maghrib_epoch) &
    
    maghrib_epoch >= utc_start_epoch &
    
    maghrib_epoch < utc_end_epoch
  
]


global_date_aware[
  
  ,
  
  isha_in_utc_window :=
    
    !is.na(isha_epoch) &
    
    isha_epoch >= utc_start_epoch &
    
    isha_epoch < utc_end_epoch
  
]


# 13. Check prayer sequence

global_date_aware[
  
  ,
  
  prayer_sequence_valid :=
    
    !is.na(fajr_epoch) &
    
    !is.na(dhuhr_epoch) &
    
    !is.na(asr_epoch) &
    
    !is.na(maghrib_epoch) &
    
    !is.na(isha_epoch) &
    
    fajr_epoch < dhuhr_epoch &
    
    dhuhr_epoch < asr_epoch &
    
    asr_epoch < maghrib_epoch &
    
    maghrib_epoch < isha_epoch
  
]


# 14. Calculate summary statistics

total_calculations <- nrow(global_date_aware)


complete_calculations <- sum(
  global_date_aware$prayer_status == "complete",
  na.rm = TRUE
)


partial_calculations <- sum(
  global_date_aware$prayer_status == "partial",
  na.rm = TRUE
)


unavailable_calculations <- sum(
  global_date_aware$prayer_status == "unavailable",
  na.rm = TRUE
)


error_calculations <- sum(
  global_date_aware$prayer_status == "error",
  na.rm = TRUE
)


valid_sequences <- sum(
  global_date_aware$prayer_sequence_valid,
  na.rm = TRUE
)


# 15. Count events inside the UTC research window

event_counts <- data.table(
  
  prayer = c(
    "Fajr",
    "Dhuhr",
    "Asr",
    "Maghrib",
    "Isha"
  ),
  
  events_in_utc_window = c(
    
    sum(global_date_aware$fajr_in_utc_window),
    
    sum(global_date_aware$dhuhr_in_utc_window),
    
    sum(global_date_aware$asr_in_utc_window),
    
    sum(global_date_aware$maghrib_in_utc_window),
    
    sum(global_date_aware$isha_in_utc_window)
    
  )
  
)


# 16. Prepare final summary

summary_results <- data.table(
  
  research_start_date = research_start_date,
  
  research_days = research_days,
  
  locations = nrow(places),
  
  location_date_calculations = total_calculations,
  
  complete_calculations = complete_calculations,
  
  partial_calculations = partial_calculations,
  
  unavailable_calculations = unavailable_calculations,
  
  error_calculations = error_calculations,
  
  valid_sequences = valid_sequences
  
)


# 17. Display results

cat("\n")
cat("============================================\n")
cat("DATE-AWARE CALCULATION RESULTS\n")
cat("============================================\n\n")


print(summary_results)


cat("\n")
cat("EVENTS INSIDE THE UTC RESEARCH WINDOW\n\n")

print(event_counts)


# 18. Display a sample of results

cat("\n")
cat("SAMPLE DATE-AWARE RESULTS\n\n")


print(
  
  global_date_aware[
    
    1:min(10, .N),
    
    .(
      
      name,
      
      country_code,
      
      timezone,
      
      local_date,
      
      fajr_local,
      
      dhuhr_local,
      
      asr_local,
      
      maghrib_local,
      
      isha_local,
      
      prayer_status,
      
      prayer_sequence_valid
      
    )
    
  ]
  
)


# 19. Save the complete date-aware dataset

fwrite(
  global_date_aware,
  output_file
)


# 20. Save the summary

fwrite(
  summary_results,
  summary_file
)


fwrite(
  
  event_counts,
  
  "data/processed/date_aware_event_counts.csv"
  
)


# 21. Final confirmation

cat("\n")
cat("============================================\n")
cat("SCRIPT 27 COMPLETED\n")
cat("============================================\n\n")


cat("Date-aware dataset saved to:\n")

cat(output_file, "\n\n")


cat("Summary saved to:\n")

cat(summary_file, "\n\n")


cat("Event counts saved to:\n")

cat(
  "data/processed/date_aware_event_counts.csv\n"
)

cat("\n============================================\n")
