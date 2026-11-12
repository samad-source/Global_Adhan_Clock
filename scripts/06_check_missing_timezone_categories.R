
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 06: CHECK MISSING TIMEZONE CATEGORIES
# =====================================================

# 1. Load required package

library(readr)


# 2. Read the investigation dataset

missing_places <- read_csv(
  "data/processed/missing_timezone_places.csv",
  show_col_types = FALSE
)


# 3. Check feature codes

cat("\nFEATURE CODE DISTRIBUTION\n")

print(
  sort(
    table(missing_places$feature_code),
    decreasing = TRUE
  )
)


# 4. Check whether all records are PPLQ

cat("\nARE ALL RECORDS PPLQ?\n")

print(
  all(missing_places$feature_code == "PPLQ")
)


# 5. Check country distribution

cat("\nCOUNTRY DISTRIBUTION\n")

print(
  head(
    sort(
      table(missing_places$country_code),
      decreasing = TRUE
    ),
    20
  )
)


# 6. Check population values

cat("\nPOPULATION SUMMARY\n")

print(
  summary(missing_places$population)
)


cat("\nCATEGORY INVESTIGATION COMPLETE\n")
