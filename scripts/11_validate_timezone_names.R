
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 11: VALIDATE TIMEZONE NAMES
# =====================================================


# 1. Load required package

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

library(data.table)


# 2. Define the input file

input_file <- "data/processed/application_places.csv"


# 3. Check that the dataset exists

if (!file.exists(input_file)) {
  stop("Application dataset not found!")
}

cat("Application dataset found.\n")


# 4. Read ONLY the timezone column

cat("\nReading timezone information...\n")

places <- data.table::fread(
  input_file,
  select = "timezone",
  na.strings = c("", "NA")
)


# 5. Count geographical records

total_records <- nrow(places)

cat("Total geographical records:", total_records, "\n")


# 6. Obtain R's recognised timezone names

valid_timezones <- OlsonNames()

cat(
  "\nRecognised timezone names in R:",
  length(valid_timezones),
  "\n"
)


# 7. Count records belonging to each timezone

timezone_summary <- places[
  ,
  .(total_records = .N),
  by = timezone
]


# 8. Identify missing timezone names

timezone_summary[
  is.na(timezone) | trimws(timezone) == "",
  timezone_status := "Missing"
]


# 9. Validate available timezone names

timezone_summary[
  !is.na(timezone) & trimws(timezone) != "",
  timezone_status := fifelse(
    timezone %in% valid_timezones,
    "Valid",
    "Unrecognised"
  )
]


# 10. Calculate validation totals

valid_count <- timezone_summary[
  timezone_status == "Valid",
  sum(total_records)
]

invalid_count <- timezone_summary[
  timezone_status == "Unrecognised",
  sum(total_records)
]

missing_count <- timezone_summary[
  timezone_status == "Missing",
  sum(total_records)
]


# 11. Display the results

cat("\n====================================\n")
cat("TIMEZONE VALIDATION RESULTS\n")
cat("====================================\n")

cat("Total records:", total_records, "\n")

cat("Valid timezone records:", valid_count, "\n")

cat("Unrecognised timezone records:", invalid_count, "\n")

cat("Missing timezone records:", missing_count, "\n")


# 12. Save the complete timezone summary

output_file <- "data/processed/timezone_validation.csv"

data.table::fwrite(
  timezone_summary,
  output_file,
  na = ""
)


# 13. Save unrecognised timezone names separately

invalid_timezones <- timezone_summary[
  timezone_status == "Unrecognised"
]

invalid_file <- "data/processed/unrecognised_timezones.csv"

data.table::fwrite(
  invalid_timezones,
  invalid_file,
  na = ""
)


# 14. Final report

cat("\n====================================\n")
cat("VALIDATION COMPLETED\n")
cat("====================================\n")

cat("Timezone summary:", output_file, "\n")

cat("Unrecognised timezone report:", invalid_file, "\n")

cat("\nGLOBAL ADHAN CLOCK TIMEZONE CHECK COMPLETE!\n")
