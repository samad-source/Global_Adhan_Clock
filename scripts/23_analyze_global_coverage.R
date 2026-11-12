
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 23: ANALYZE GLOBAL DATASET COVERAGE
# =====================================================

library(data.table)

# 1. Load our calculated results

global_data <- fread(
  "data/processed/global_sunrise_sunset.csv"
)

# 2. Count unique countries

country_summary <- global_data[
  ,
  .(
    number_of_places = .N,
    total_population = sum(population, na.rm = TRUE)
  ),
  by = country_code
][order(-number_of_places)]

# 3. Display global statistics

cat("\n====================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("GLOBAL GEOGRAPHICAL COVERAGE\n")
cat("====================================\n")

cat(
  "Total locations:",
  nrow(global_data),
  "\n"
)

cat(
  "Countries represented:",
  uniqueN(global_data$country_code),
  "\n"
)

cat(
  "Locations with sunrise:",
  sum(!is.na(global_data$sunrise_utc)),
  "\n"
)

cat(
  "Locations with sunset:",
  sum(!is.na(global_data$sunset_utc)),
  "\n"
)

# 4. Display countries with the most locations

cat("\nTOP 20 COUNTRIES BY LOCATION COUNT\n")

print(
  country_summary[1:min(20, .N)]
)

# 5. Select a reproducible random sample

set.seed(123)

global_sample <- global_data[
  sample(.N, min(20, .N))
]

cat("\nRANDOM GLOBAL SAMPLE\n")

print(
  global_sample[
    ,
    .(
      name,
      country_code,
      latitude,
      longitude,
      sunrise_utc,
      sunset_utc
    )
  ]
)

# 6. Save country summary

fwrite(
  country_summary,
  "data/processed/global_country_coverage.csv"
)

cat("\n====================================\n")
cat("COVERAGE ANALYSIS COMPLETED\n")
cat("====================================\n")

cat(
  "Country summary saved to:",
  "data/processed/global_country_coverage.csv",
  "\n"
)
