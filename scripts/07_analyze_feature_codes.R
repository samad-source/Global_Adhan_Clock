
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 07: ANALYZE POPULATED PLACE CATEGORIES
# =====================================================

# 1. Load required package

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}


# 2. Define dataset location

input_file <- "data/processed/populated_places.csv"


# 3. Read only the feature code column

places <- data.table::fread(
  input = input_file,
  select = "feature_code"
)


# 4. Count each feature code

feature_counts <- sort(
  table(places$feature_code),
  decreasing = TRUE
)


# 5. Display the 30 most common feature codes

cat("\nTOP 30 POPULATED PLACE CATEGORIES\n")

print(
  head(feature_counts, 30)
)


# 6. Count the number of different categories

cat(
  "\nTotal different feature codes:",
  length(feature_counts),
  "\n"
)


# 7. Count abandoned places

abandoned_count <- sum(
  places$feature_code == "PPLQ",
  na.rm = TRUE
)

cat(
  "\nTotal abandoned populated places:",
  abandoned_count,
  "\n"
)


cat("\nFEATURE CODE ANALYSIS COMPLETE\n")
