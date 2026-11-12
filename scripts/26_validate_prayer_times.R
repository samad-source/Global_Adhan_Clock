
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 26: INDEPENDENT PRAYER-TIME VALIDATION
# ============================================================


# 1. Load required package

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

library(data.table)
library(jsonlite)


# 2. Define research date

research_date <- "2026-11-10"

api_date <- format(
  as.Date(research_date),
  "%d-%m-%Y"
)


# 3. Load our calculated results

input_file <- paste0(
  "data/processed/",
  "global_prayer_times.csv"
)

if (!file.exists(input_file)) {
  stop("Run Script 25 first.")
}

global_prayer_times <- fread(input_file)


# 4. Select our validation locations

candidate_names <- c(
  "Surulere",
  "Zaria",
  "Dubai",
  "London",
  "Tokyo"
)

validation_places <- global_prayer_times[
  tolower(name) %chin% tolower(candidate_names)
]


if (nrow(validation_places) == 0) {
  stop("None of the requested validation locations was found.")
}


cat("\n")
cat("============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("INDEPENDENT PRAYER-TIME VALIDATION\n")
cat("============================================\n\n")

cat("Research date:", research_date, "\n")

cat(
  "Validation locations:",
  nrow(validation_places),
  "\n\n"
)


# 5. Convert a time into minutes after midnight

clock_to_minutes <- function(clock) {
  
  if (is.na(clock) || !nzchar(clock)) {
    return(NA_real_)
  }
  
  parts <- strsplit(clock, ":", fixed = TRUE)[[1]]
  
  as.numeric(parts[1]) * 60 +
    as.numeric(parts[2])
}


# 6. Extract HH:MM from an API time

extract_api_clock <- function(value) {
  
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }
  
  value <- as.character(value[1])
  
  if (is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  
  if (grepl("T", value, fixed = TRUE)) {
    return(substr(value, 12, 16))
  }
  
  if (grepl("^[0-9]{2}:[0-9]{2}", value)) {
    return(substr(value, 1, 5))
  }
  
  return(NA_character_)
}


# 7. Compare two clock times

calculate_difference <- function(
    our_time,
    reference_time
) {
  
  our_minutes <- clock_to_minutes(our_time)
  
  reference_minutes <- clock_to_minutes(reference_time)
  
  if (
    is.na(our_minutes) ||
    is.na(reference_minutes)
  ) {
    return(NA_real_)
  }
  
  difference <- our_minutes - reference_minutes
  
  # Handle a possible midnight boundary.
  
  if (difference > 720) {
    difference <- difference - 1440
  }
  
  if (difference < -720) {
    difference <- difference + 1440
  }
  
  return(difference)
}


# 8. Define the five prayers

prayer_names <- c(
  "Fajr",
  "Dhuhr",
  "Asr",
  "Maghrib",
  "Isha"
)

our_columns <- c(
  "fajr_local",
  "dhuhr_local",
  "asr_local",
  "maghrib_local",
  "isha_local"
)


# 9. Compare each location with AlAdhan

validation_results <- list()


for (i in seq_len(nrow(validation_places))) {
  
  place <- validation_places[i]
  
  cat(
    "Checking:",
    place$name,
    place$country_code,
    "\n"
  )
  
  
  # Build API request.
  
  api_url <- paste0(
    "https://api.aladhan.com/v1/timings/",
    api_date,
    "?latitude=",
    place$latitude,
    "&longitude=",
    place$longitude,
    "&method=3",
    "&school=0",
    "&timezonestring=",
    URLencode(place$timezone, reserved = TRUE),
    "&iso8601=true"
  )
  
  
  # Request reference calculation.
  
  api_result <- tryCatch(
    
    {
      
      response_text <- paste(
        readLines(api_url, warn = FALSE),
        collapse = ""
      )
      
      jsonlite::fromJSON(
        response_text,
        simplifyVector = FALSE
      )
      
    },
    
    error = function(e) {
      
      warning(
        "API request failed for ",
        place$name,
        ": ",
        conditionMessage(e)
      )
      
      NULL
      
    }
    
  )
  
  
  if (
    is.null(api_result) ||
    is.null(api_result$data) ||
    is.null(api_result$data$timings)
  ) {
    
    next
    
  }
  
  
  reference_timings <- api_result$data$timings
  
  
  # Compare the five prayer times.
  
  for (j in seq_along(prayer_names)) {
    
    prayer <- prayer_names[j]
    
    our_column <- our_columns[j]
    
    
    our_clock <- substr(
      place[[our_column]],
      12,
      16
    )
    
    
    reference_clock <- extract_api_clock(
      reference_timings[[prayer]]
    )
    
    
    difference <- calculate_difference(
      our_clock,
      reference_clock
    )
    
    
    validation_results[[length(validation_results) + 1]] <-
      
      data.table(
        
        location = place$name,
        
        country = place$country_code,
        
        prayer = prayer,
        
        our_time = our_clock,
        
        reference_time = reference_clock,
        
        difference_minutes = difference
        
      )
    
  }
  
}


# 10. Combine results

if (length(validation_results) == 0) {
  stop("No successful API comparisons were returned.")
}


validation_table <- rbindlist(
  validation_results,
  fill = TRUE
)


# 11. Calculate absolute differences

validation_table[
  ,
  absolute_difference_minutes :=
    abs(difference_minutes)
]


# 12. Display comparison

cat("\n")
cat("============================================\n")
cat("PRAYER-TIME COMPARISON RESULTS\n")
cat("============================================\n\n")


print(validation_table)


# 13. Calculate summary by prayer

validation_summary <- validation_table[
  
  ,
  
  .(
    
    comparisons = sum(!is.na(difference_minutes)),
    
    mean_absolute_difference = mean(
      absolute_difference_minutes,
      na.rm = TRUE
    ),
    
    maximum_absolute_difference = max(
      absolute_difference_minutes,
      na.rm = TRUE
    )
    
  ),
  
  by = prayer
  
]


cat("\n")
cat("SUMMARY BY PRAYER\n\n")

print(validation_summary)


# 14. Save results

fwrite(
  
  validation_table,
  
  "data/processed/prayer_validation_comparison.csv"
  
)


fwrite(
  
  validation_summary,
  
  "data/processed/prayer_validation_summary.csv"
  
)


cat("\n")
cat("============================================\n")
cat("SCRIPT 26 COMPLETED\n")
cat("============================================\n\n")

cat(
  "Comparison results saved to:\n",
  "data/processed/prayer_validation_comparison.csv\n\n"
)

cat(
  "Summary saved to:\n",
  "data/processed/prayer_validation_summary.csv\n"
)
