
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 10: PREPARE APPLICATION-READY DATASET
# =====================================================


# 1. Load required package

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

library(data.table)


# 2. Define file locations

input_file <- "data/processed/populated_places.csv"

output_file <- "data/processed/application_places.csv"


# 3. Check whether the original dataset exists

if (!file.exists(input_file)) {
  stop("The master populated places dataset was not found!")
}

cat("Master dataset found.\n")


# 4. Read the master dataset

cat("\nReading geographical records...\n")

places <- data.table::fread(
  input_file,
  na.strings = c("", "NA")
)

cat("Dataset successfully loaded.\n")

cat("Total records:", nrow(places), "\n")


# 5. Identify abandoned places

places[, place_status := fifelse(
  feature_code == "PPLQ",
  "Abandoned",
  "Not identified as abandoned"
)]


# 6. Identify records with missing feature codes

places[
  is.na(feature_code) | trimws(feature_code) == "",
  place_status := "Feature code missing"
]


# 7. Check time zone availability

places[, timezone_status := fifelse(
  is.na(timezone) | trimws(timezone) == "",
  "Timezone missing",
  "Timezone available"
)]


# 8. Identify records suitable for clock calculations

places[, clock_eligible := (
  !is.na(timezone) &
    trimws(timezone) != ""
)]


# 9. Save the application-ready dataset

cat("\nSaving application-ready dataset...\n")

data.table::fwrite(
  places,
  output_file,
  na = ""
)


# 10. Display the results

cat("\n====================================\n")
cat("APPLICATION DATASET PREPARATION\n")
cat("====================================\n")

cat("Total geographical records:", nrow(places), "\n")

cat(
  "Abandoned places:",
  sum(places$place_status == "Abandoned"),
  "\n"
)

cat(
  "Records with missing feature codes:",
  sum(places$place_status == "Feature code missing"),
  "\n"
)

cat(
  "Records with available time zones:",
  sum(places$clock_eligible),
  "\n"
)

cat(
  "Records with missing time zones:",
  sum(!places$clock_eligible),
  "\n"
)

cat("\nOutput file:", output_file, "\n")

cat("====================================\n")

cat("\nAPPLICATION DATASET PREPARATION COMPLETE!\n")
