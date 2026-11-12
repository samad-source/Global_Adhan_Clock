
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 12: CHECK TIMEZONE COORDINATE AGREEMENT
# =====================================================


# 1. Install required packages

required_packages <- c("data.table", "lutz", "sf")

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}


# 2. Load packages

library(data.table)
library(lutz)
library(sf)


# 3. Define input file

input_file <- "data/processed/application_places.csv"

if (!file.exists(input_file)) {
  stop("Application dataset not found!")
}


# 4. Read the necessary columns only

cat("Reading geographical information...\n")

places <- data.table::fread(
  input_file,
  select = c(
    "geoname_id",
    "name",
    "latitude",
    "longitude",
    "timezone",
    "country_code"
  ),
  na.strings = c("", "NA")
)


# 5. Keep records with complete coordinates and timezones

places <- places[
  !is.na(latitude) &
    !is.na(longitude) &
    !is.na(timezone) &
    trimws(timezone) != ""
]


# 6. Select a reproducible sample of 1,000 records

set.seed(123)

sample_size <- min(1000, nrow(places))

sample_places <- places[
  sample.int(nrow(places), sample_size)
]

cat("Sample size:", nrow(sample_places), "\n")


# 7. Look up timezones using geographical coordinates

cat("\nChecking coordinates against timezone boundaries...\n")

sample_places[
  ,
  coordinate_timezone := lutz::tz_lookup_coords(
    lat = latitude,
    lon = longitude,
    method = "accurate"
  )
]



# 8. Compare GeoNames and coordinate-based timezones

sample_places[
  ,
  timezone_match := mapply(
    function(original, coordinate) {
      
      if (is.na(coordinate) || coordinate == "") {
        return(NA)
      }
      
      detected_zones <- strsplit(
        coordinate,
        ";",
        fixed = TRUE
      )[[1]]
      
      # Remove unnecessary spaces
      detected_zones <- trimws(detected_zones)
      
      # Check whether the original timezone is present
      original %in% detected_zones
    },
    timezone,
    coordinate_timezone
  )
]



# 9. Calculate comparison results

matching_records <- sum(
  sample_places$timezone_match %in% TRUE
)

mismatching_records <- sum(
  sample_places$timezone_match %in% FALSE
)

unmatched_records <- sum(
  is.na(sample_places$timezone_match)
)


# 10. Calculate agreement percentage

agreement_percentage <- if (
  matching_records + mismatching_records > 0
) {
  matching_records /
    (matching_records + mismatching_records) * 100
} else {
  NA_real_
}


# 11. Display results

cat("\n====================================\n")
cat("TIMEZONE COORDINATE VALIDATION\n")
cat("====================================\n")

cat("Sample records:", sample_size, "\n")

cat("Matching records:", matching_records, "\n")

cat("Mismatching records:", mismatching_records, "\n")

cat("Records without a boundary result:", unmatched_records, "\n")

cat(
  "Agreement percentage:",
  round(agreement_percentage, 2),
  "%\n"
)


# 12. Save the complete comparison

output_file <- "data/processed/timezone_coordinate_sample.csv"

data.table::fwrite(
  sample_places,
  output_file,
  na = ""
)


# 13. Save mismatching records separately

mismatches <- sample_places[
  timezone_match == FALSE
]

mismatch_file <- "data/processed/timezone_coordinate_mismatches.csv"

data.table::fwrite(
  mismatches,
  mismatch_file,
  na = ""
)


# 14. Final report

cat("\n====================================\n")
cat("COORDINATE VALIDATION COMPLETED\n")
cat("====================================\n")

cat("Comparison report:", output_file, "\n")

cat("Mismatch report:", mismatch_file, "\n")

cat("\nGLOBAL ADHAN CLOCK VALIDATION COMPLETE!\n")
