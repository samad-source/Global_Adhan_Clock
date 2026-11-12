
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 08: ANALYZE TIME ZONES BY CATEGORY
# =====================================================

# 1. Load required package

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}


# 2. Define dataset location

input_file <- "data/processed/populated_places.csv"


# 3. Read only the required columns

places <- data.table::fread(
  input = input_file,
  select = c("feature_code", "timezone")
)


# 4. Identify missing time zones

places[, timezone_missing :=
         is.na(timezone) | trimws(timezone) == ""]


# 5. Count records by category and time-zone status

timezone_by_category <- places[
  ,
  .(
    total_records = .N,
    missing_timezones = sum(timezone_missing),
    available_timezones = sum(!timezone_missing)
  ),
  by = feature_code
]


# 6. Calculate time-zone completeness percentage

timezone_by_category[
  ,
  completeness_percentage :=
    round(
      available_timezones / total_records * 100,
      2
    )
]


# 7. Sort categories by number of records

data.table::setorder(
  timezone_by_category,
  -total_records
)


# 8. Display results

cat("\nTIME-ZONE AVAILABILITY BY CATEGORY\n")

print(timezone_by_category)


# 9. Save the results

output_file <- "data/processed/timezone_by_category.csv"

data.table::fwrite(
  timezone_by_category,
  output_file
)


cat("\nAnalysis completed!\n")

cat("Saved to:", output_file, "\n")
