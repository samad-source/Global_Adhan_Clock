
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 25: GLOBAL FIVE-PRAYER CALCULATION
# ============================================================


# 1. Load required package

library(data.table)


# 2. Load our prayer-time engine

source("R/prayer_times.R")


# 3. Define research date

research_date <- "2026-11-10"


# 4. Define input and output files

input_file <- paste0(
  "data/processed/",
  "global_sunrise_sunset_local_dates.csv"
)


output_file <- paste0(
  "data/processed/",
  "global_prayer_times.csv"
)


# 5. Check that the input file exists

if (!file.exists(input_file)) {
  stop(
    "Input file not found. ",
    "Run Script 24 first."
  )
}


# 6. Load global locations

places <- fread(input_file)


cat("\n")
cat("============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("GLOBAL FIVE-PRAYER CALCULATION\n")
cat("============================================\n\n")


cat("Research date:", research_date, "\n")

cat(
  "Locations loaded:",
  format(nrow(places), big.mark = ","),
  "\n\n"
)


# 7. Check required columns

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
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
  
}


# 8. Prepare location information

identity_columns <- c(
  "geoname_id",
  "name",
  "country_code",
  "latitude",
  "longitude",
  "timezone"
)


location_information <- places[
  ,
  ..identity_columns
]


# 9. Calculate prayer times for every location

results <- vector(
  "list",
  nrow(places)
)


for (i in seq_len(nrow(places))) {
  
  
  current_place <- places[i]
  
  
  calculation <- tryCatch(
    
    {
      
      calculate_prayer_times_for_place(
        
        research_date = research_date,
        
        latitude = current_place$latitude,
        
        longitude = current_place$longitude,
        
        timezone = current_place$timezone
        
      )
      
    },
    
    
    error = function(e) {
      
      list(
        
        prayer_status = "error",
        
        calculation_error = conditionMessage(e)
        
      )
      
    }
    
  )
  
  
  # Store the result for this location.
  
  results[[i]] <- as.data.table(calculation)
  
  
  # Show progress every 500 locations.
  
  if (
    i %% 500 == 0 ||
    i == nrow(places)
  ) {
    
    cat(
      "Processed:",
      format(i, big.mark = ","),
      "of",
      format(nrow(places), big.mark = ","),
      "locations\n"
    )
    
  }
  
}


# 10. Combine all results

prayer_results <- rbindlist(
  results,
  fill = TRUE
)


global_prayer_times <- cbind(
  location_information,
  prayer_results
)


# 11. Check prayer-time sequence

global_prayer_times[
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


# 12. Calculate summary statistics

total_locations <- nrow(global_prayer_times)


complete_locations <- sum(
  global_prayer_times$prayer_status == "complete",
  na.rm = TRUE
)


partial_locations <- sum(
  global_prayer_times$prayer_status == "partial",
  na.rm = TRUE
)


unavailable_locations <- sum(
  global_prayer_times$prayer_status == "unavailable",
  na.rm = TRUE
)


error_locations <- sum(
  global_prayer_times$prayer_status == "error",
  na.rm = TRUE
)


valid_sequences <- sum(
  global_prayer_times$prayer_sequence_valid,
  na.rm = TRUE
)


# 13. Count missing prayer-time calculations

missing_prayer_counts <- data.table(
  
  prayer = c(
    "Fajr",
    "Dhuhr",
    "Asr",
    "Maghrib",
    "Isha"
  ),
  
  unavailable_count = c(
    
    sum(is.na(global_prayer_times$fajr_epoch)),
    
    sum(is.na(global_prayer_times$dhuhr_epoch)),
    
    sum(is.na(global_prayer_times$asr_epoch)),
    
    sum(is.na(global_prayer_times$maghrib_epoch)),
    
    sum(is.na(global_prayer_times$isha_epoch))
    
  )
  
)


# 14. Display global summary

cat("\n")
cat("============================================\n")
cat("GLOBAL PRAYER-TIME RESULTS\n")
cat("============================================\n\n")


cat(
  "Total locations:",
  format(total_locations, big.mark = ","),
  "\n"
)


cat(
  "Complete calculations:",
  format(complete_locations, big.mark = ","),
  "\n"
)


cat(
  "Partial calculations:",
  format(partial_locations, big.mark = ","),
  "\n"
)


cat(
  "Completely unavailable:",
  format(unavailable_locations, big.mark = ","),
  "\n"
)


cat(
  "Calculation errors:",
  format(error_locations, big.mark = ","),
  "\n"
)


cat(
  "Valid prayer sequences:",
  format(valid_sequences, big.mark = ","),
  "\n\n"
)


# 15. Display missing prayer-time counts

cat("UNAVAILABLE PRAYER-TIME COUNTS\n\n")

print(missing_prayer_counts)


# 16. Display sample global results

cat("\n")
cat("FIRST 10 GLOBAL LOCATIONS\n\n")


print(
  
  global_prayer_times[
    1:min(10, .N),
    
    .(
      name,
      country_code,
      timezone,
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


# 17. Display Nigerian locations

cat("\n")
cat("NIGERIAN LOCATION SAMPLE\n\n")


nigerian_sample <- global_prayer_times[
  country_code == "NG"
]


if (nrow(nigerian_sample) > 0) {
  
  print(
    
    nigerian_sample[
      1:min(10, .N),
      
      .(
        name,
        latitude,
        longitude,
        timezone,
        fajr_local,
        dhuhr_local,
        asr_local,
        maghrib_local,
        isha_local,
        prayer_status
      )
      
    ]
    
  )
  
} else {
  
  cat("No Nigerian locations found.\n")
  
}


# 18. Save global results

fwrite(
  global_prayer_times,
  output_file
)


# Save the missing-prayer summary separately.

fwrite(
  
  missing_prayer_counts,
  
  "data/processed/prayer_time_availability_summary.csv"
  
)


# 19. Final confirmation

cat("\n")
cat("============================================\n")
cat("SCRIPT 25 COMPLETED\n")
cat("============================================\n\n")


cat(
  "Global prayer-time dataset saved to:\n",
  output_file,
  "\n\n"
)


cat(
  "Missing-prayer summary saved to:\n",
  "data/processed/prayer_time_availability_summary.csv",
  "\n"
)

cat("\n============================================\n")
