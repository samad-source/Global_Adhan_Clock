
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 04: CHECK DUPLICATE IDENTIFIERS
# =====================================================


# 1. LOAD REQUIRED PACKAGE

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}


# 2. DEFINE DATASET LOCATION

processed_file <- "data/processed/populated_places.csv"


# 3. CHECK THAT THE DATASET EXISTS

if (!file.exists(processed_file)) {
  stop("Processed populated places dataset not found!")
}


# 4. READ GEOGRAPHICAL IDENTIFIERS

ids <- data.table::fread(
  input = processed_file,
  select = "geoname_id",
  colClasses = c(geoname_id = "character")
)


# 5. COUNT DUPLICATE IDENTIFIERS

duplicate_count <- sum(
  duplicated(ids$geoname_id)
)


# 6. COUNT MISSING IDENTIFIERS

missing_id_count <- sum(
  is.na(ids$geoname_id) |
    trimws(ids$geoname_id) == ""
)


# 7. DISPLAY VALIDATION RESULTS

cat("\n====================================\n")
cat("GEOGRAPHICAL IDENTIFIER VALIDATION\n")
cat("====================================\n")

cat("Total identifiers:", nrow(ids), "\n")

cat("Duplicate identifiers:", duplicate_count, "\n")

cat("Missing identifiers:", missing_id_count, "\n")


# 8. DISPLAY FINAL STATUS

if (
  duplicate_count == 0 &&
  missing_id_count == 0
) {
  
  cat("\nRESULT: PASSED\n")
  cat("All geographical identifiers are unique and present.\n")
  
} else {
  
  cat("\nRESULT: CHECK REQUIRED\n")
  cat("Some identifiers are duplicated or missing.\n")
  
}

cat("====================================\n")
