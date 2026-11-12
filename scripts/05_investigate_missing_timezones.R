
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 05: INVESTIGATE MISSING TIME ZONES
# =====================================================


# 1. LOAD REQUIRED PACKAGE

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}


# 2. DEFINE THE DATASET

input_file <- "data/processed/populated_places.csv"


# 3. CHECK THAT THE FILE EXISTS

if (!file.exists(input_file)) {
  stop("The processed dataset was not found!")
}


# 4. READ THE DATASET

places <- readr::read_csv(
  input_file,
  na = c("", "NA"),
  show_col_types = FALSE
)


# 5. IDENTIFY RECORDS WITH MISSING TIME ZONES

missing_timezone_places <- places[
  is.na(places$timezone) |
    trimws(places$timezone) == "",
]


# 6. DISPLAY THE NUMBER OF AFFECTED PLACES

cat(
  "\nPlaces with missing time zones:",
  nrow(missing_timezone_places),
  "\n"
)


# 7. DISPLAY THE FIRST 20 AFFECTED PLACES

cat("\nFIRST 20 PLACES WITH MISSING TIME ZONES\n")

print(
  head(missing_timezone_places, 20)
)


# 8. SAVE THE AFFECTED RECORDS

output_file <- "data/processed/missing_timezone_places.csv"

readr::write_csv(
  missing_timezone_places,
  output_file
)


# 9. DISPLAY COMPLETION MESSAGE

cat("\nInvestigation completed!\n")

cat("Saved to:", output_file, "\n")
