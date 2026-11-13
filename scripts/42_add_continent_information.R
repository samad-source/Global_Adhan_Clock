
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 42: ADD CONTINENT INFORMATION
# CORRECTED VERSION
# ============================================================

cat("\n============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 42: CONTINENT CLASSIFICATION\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. FILE PATHS
# ------------------------------------------------------------

master_file <- "data/global_cities.csv"
browser_file <- "www/global_cities.csv"
country_file <- "data/countryInfo.txt"

if (!file.exists(master_file)) {
  stop("Run Script 41 first. Global city dataset not found.")
}

# ------------------------------------------------------------
# 2. DOWNLOAD COUNTRY INFORMATION IF NECESSARY
# ------------------------------------------------------------

country_url <- paste0(
  "https://download.geonames.org/",
  "export/dump/countryInfo.txt"
)

if (!file.exists(country_file)) {
  
  cat("Downloading GeoNames country information...\n")
  
  download.file(
    country_url,
    country_file,
    mode = "wb"
  )
  
} else {
  
  cat("Existing country information found.\n")
  
}

# ------------------------------------------------------------
# 3. READ GLOBAL CITY DATA
# ------------------------------------------------------------

cities <- read.csv(
  master_file,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 4. READ COUNTRY DATA
# ------------------------------------------------------------

countries <- read.delim(
  country_file,
  header = FALSE,
  sep = "\t",
  comment.char = "#",
  quote = "",
  fill = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  
  # IMPORTANT:
  # Preserve "NA" because it means North America.
  na.strings = ""
)

if (ncol(countries) < 9) {
  stop("Unexpected GeoNames country information format.")
}

# GeoNames countryInfo columns:
# 1 = ISO country code
# 5 = Country name
# 9 = Continent code

country_lookup <- data.frame(
  
  country_code = trimws(countries[[1]]),
  
  country_name = countries[[5]],
  
  continent_code = trimws(countries[[9]]),
  
  stringsAsFactors = FALSE
  
)

country_lookup <- country_lookup[
  !is.na(country_lookup$country_code) &
    nzchar(country_lookup$country_code),
]

country_lookup <- country_lookup[
  !duplicated(country_lookup$country_code),
]

# ------------------------------------------------------------
# 5. MATCH COUNTRIES TO CITIES
# ------------------------------------------------------------

match_index <- match(
  cities$country_code,
  country_lookup$country_code
)

cities$country_name <-
  country_lookup$country_name[match_index]

cities$continent_code <-
  country_lookup$continent_code[match_index]

# ------------------------------------------------------------
# 6. CONVERT CONTINENT CODES TO NAMES
# ------------------------------------------------------------

continent_names <- c(
  "AF" = "Africa",
  "AS" = "Asia",
  "EU" = "Europe",
  "NA" = "North America",
  "SA" = "South America",
  "OC" = "Oceania",
  "AN" = "Antarctica"
)

cities$continent <- unname(
  continent_names[cities$continent_code]
)

# ------------------------------------------------------------
# 7. CHECK UNMATCHED RECORDS
# ------------------------------------------------------------

unmatched <- sum(is.na(cities$continent))

cat("\nUNMATCHED CITY RECORDS:", unmatched, "\n")

if (unmatched > 0) {
  
  cat("\nUnmatched country codes:\n")
  
  print(
    unique(
      cities$country_code[
        is.na(cities$continent)
      ]
    )
  )
  
}

# ------------------------------------------------------------
# 8. SAVE MASTER DATASET
# ------------------------------------------------------------

write.csv(
  cities,
  master_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

# ------------------------------------------------------------
# 9. SAVE BROWSER DATASET
# ------------------------------------------------------------

write.csv(
  cities,
  browser_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

# ------------------------------------------------------------
# 10. CONTINENT DISTRIBUTION
# ------------------------------------------------------------

cat("\n============================================\n")
cat("CONTINENT DISTRIBUTION\n")
cat("============================================\n")

print(
  sort(
    table(cities$continent, useNA = "ifany"),
    decreasing = TRUE
  )
)

# ------------------------------------------------------------
# 11. COUNTRY COUNT
# ------------------------------------------------------------

cat("\nCOUNTRY COUNT:\n")

print(
  length(unique(cities$country_code))
)

# ------------------------------------------------------------
# 12. VERIFY NORTH AMERICA
# ------------------------------------------------------------

cat("\nNORTH AMERICA VERIFICATION:\n")

print(
  country_lookup[
    country_lookup$country_code %in% c("US", "CA", "MX"),
    ,
    drop = FALSE
  ]
)

# ------------------------------------------------------------
# 13. FINAL DATASET REPORT
# ------------------------------------------------------------

cat("\nTOTAL CITY RECORDS:\n")
print(nrow(cities))

cat("\nMISSING CONTINENT RECORDS:\n")
print(sum(is.na(cities$continent)))

cat("\nBROWSER FILE EXISTS:\n")
print(file.exists(browser_file))

cat("\n============================================\n")
cat("SCRIPT 42 COMPLETED\n")
cat("============================================\n")

cat(
  "\nSource: GeoNames.org",
  "\nhttps://www.geonames.org/",
  "\nLicense: Creative Commons Attribution\n"
)
