# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 19: EQUATION OF TIME
# =====================================================


# 1. Create the calculation function

calculate_equation_of_time <- function(datetime_utc) {
  
  date_parts <- as.POSIXlt(
    datetime_utc,
    tz = "UTC"
  )
  
  day_of_year <- date_parts$yday + 1
  
  year <- date_parts$year + 1900
  
  decimal_hour <- date_parts$hour +
    date_parts$min / 60 +
    date_parts$sec / 3600
  
  
  # 2. Account for leap years
  
  days_in_year <- ifelse(
    ((year %% 4 == 0) & (year %% 100 != 0)) |
      (year %% 400 == 0),
    366,
    365
  )
  
  
  # 3. Calculate fractional year angle
  
  gamma <- (
    2 * pi / days_in_year
  ) * (
    day_of_year - 1 +
      (decimal_hour - 12) / 24
  )
  
  
  # 4. Calculate equation of time in minutes
  
  equation_of_time <- 229.18 * (
    
    0.000075 +
      
      0.001868 * cos(gamma) -
      
      0.032077 * sin(gamma) -
      
      0.014615 * cos(2 * gamma) -
      
      0.040849 * sin(2 * gamma)
    
  )
  
  
  return(equation_of_time)
  
}


# =====================================================
# TEST OUR FUNCTION
# =====================================================


june_date <- as.POSIXct(
  "2026-06-21 12:00:00",
  tz = "UTC"
)

december_date <- as.POSIXct(
  "2026-12-21 12:00:00",
  tz = "UTC"
)


june_eot <- calculate_equation_of_time(
  june_date
)

december_eot <- calculate_equation_of_time(
  december_date
)


# Display results

cat("\n====================================\n")

cat("GLOBAL ADHAN CLOCK\n")

cat("EQUATION OF TIME TEST\n")

cat("====================================\n")

cat(
  "June 21 equation of time:",
  round(june_eot, 3),
  "minutes\n"
)

cat(
  "December 21 equation of time:",
  round(december_eot, 3),
  "minutes\n"
)