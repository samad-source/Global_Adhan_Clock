
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 14: COMPARE TIMEZONE RULES
# =====================================================


# 1. Define timezone pairs

timezone_pairs <- data.frame(
  timezone_a = c(
    "Europe/Kyiv",
    "Europe/Kyiv",
    "Asia/Samarkand",
    "Australia/Lord_Howe"
  ),
  
  timezone_b = c(
    "Europe/Kiev",
    "Europe/Zaporozhye",
    "Asia/Tashkent",
    "Australia/Sydney"
  )
)


# 2. Create dates for comparison

comparison_dates <- seq(
  as.Date("1970-01-01"),
  as.Date("2050-12-31"),
  by = "15 days"
)

comparison_instants <- as.POSIXct(
  comparison_dates,
  tz = "UTC"
)


# 3. Create a function to compare timezone rules

compare_timezone_rules <- function(zone_a, zone_b) {
  
  if (
    !zone_a %in% OlsonNames() ||
    !zone_b %in% OlsonNames()
  ) {
    return("Timezone not recognised")
  }
  
  offsets_a <- format(
    comparison_instants,
    tz = zone_a,
    format = "%z"
  )
  
  offsets_b <- format(
    comparison_instants,
    tz = zone_b,
    format = "%z"
  )
  
  if (identical(offsets_a, offsets_b)) {
    return("Same sampled UTC offsets")
  }
  
  return("Different sampled UTC offsets")
}


# 4. Compare all timezone pairs

timezone_pairs$result <- mapply(
  compare_timezone_rules,
  timezone_pairs$timezone_a,
  timezone_pairs$timezone_b
)


# 5. Display results

cat("\n====================================\n")
cat("TIMEZONE RULE COMPARISON\n")
cat("====================================\n")

print(timezone_pairs)


# 6. Save results

output_file <- "data/processed/timezone_rule_comparison.csv"

write.csv(
  timezone_pairs,
  output_file,
  row.names = FALSE
)


# 7. Final message

cat("\n====================================\n")
cat("COMPARISON COMPLETED\n")
cat("====================================\n")

cat("Saved report:", output_file, "\n")
