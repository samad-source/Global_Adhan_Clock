
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 13: INVESTIGATE TIMEZONE MISMATCHES
# =====================================================


# 1. Load data.table

library(data.table)


# 2. Define the mismatch report

input_file <- "data/processed/timezone_coordinate_mismatches.csv"


# 3. Check that the report exists

if (!file.exists(input_file)) {
  stop("Timezone mismatch report not found!")
}


# 4. Read the mismatch report

mismatches <- fread(input_file)


# 5. Display the number of mismatches

cat("\n====================================\n")
cat("TIMEZONE MISMATCH INVESTIGATION\n")
cat("====================================\n")

cat("Total mismatches:", nrow(mismatches), "\n")


# 6. Display the important columns

cat("\nDETAILS OF MISMATCHED LOCATIONS\n")

print(
  mismatches[
    ,
    .(
      geoname_id,
      name,
      country_code,
      latitude,
      longitude,
      timezone,
      coordinate_timezone,
      timezone_match
    )
  ]
)


# 7. Count mismatches by country

cat("\nMISMATCHES BY COUNTRY\n")

country_summary <- mismatches[
  ,
  .(mismatch_count = .N),
  by = country_code
][order(-mismatch_count)]

print(country_summary)


# 8. Count mismatches by timezone pair

cat("\nTIMEZONE COMPARISON SUMMARY\n")

timezone_summary <- mismatches[
  ,
  .(mismatch_count = .N),
  by = .(timezone, coordinate_timezone)
][order(-mismatch_count)]

print(timezone_summary)


# 9. Save investigation results

output_file <- "data/processed/timezone_mismatch_investigation.csv"

fwrite(
  mismatches,
  output_file
)


# 10. Final message

cat("\n====================================\n")
cat("INVESTIGATION COMPLETED\n")
cat("====================================\n")

cat("Detailed report:", output_file, "\n")

cat("\nNo timezone values have been changed.\n")
