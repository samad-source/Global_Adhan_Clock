
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 09: INVESTIGATE BLANK FEATURE CODES
# =====================================================

# 1. Load required package

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}


# 2. Define dataset location

input_file <- "data/processed/populated_places.csv"


# 3. Read the processed dataset

places <- data.table::fread(
  input = input_file,
  na.strings = c("", "NA")
)


# 4. Identify missing feature codes

missing_feature_codes <- places[
  is.na(feature_code) |
    trimws(feature_code) == ""
]


# 5. Display the number of affected records

cat(
  "\nRecords with missing feature codes:",
  nrow(missing_feature_codes),
  "\n"
)


# 6. Display the affected record

cat("\nAFFECTED RECORD DETAILS\n")

print(missing_feature_codes)


# 7. Save the affected record

output_file <- "data/processed/missing_feature_codes.csv"

data.table::fwrite(
  missing_feature_codes,
  output_file
)


cat("\nInvestigation completed!\n")
cat("Saved to:", output_file, "\n")
