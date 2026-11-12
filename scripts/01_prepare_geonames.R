
# GLOBAL ADHAN CLOCK
# SCRIPT 01: PREPARE GEONAMES DATA


# 1. Define file locations

zip_file <- "data/raw/allCountries.zip"

extract_folder <- "data/raw/geonames"

geonames_file <- file.path(
  extract_folder,
  "allCountries.txt"
)


# 2. Check whether the downloaded ZIP exists

if (!file.exists(zip_file)) {
  stop("GeoNames ZIP file was not found!")
}

cat("GeoNames ZIP file found.\n")


# 3. Create extraction folder if necessary

dir.create(
  extract_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# 4. Extract only when the text file does not exist

if (!file.exists(geonames_file)) {
  
  cat("Extracting GeoNames dataset...\n")
  
  unzip(
    zip_file,
    exdir = extract_folder
  )
  
} else {
  
  cat("GeoNames dataset already extracted.\n")
  
}


# 5. Confirm extraction

if (!file.exists(geonames_file)) {
  stop("Extraction failed!")
}

cat("GeoNames dataset is ready.\n")


# 6. Inspect the first three records

cat("\nFirst three records:\n")

connection <- file(
  geonames_file,
  open = "r"
)

first_records <- readLines(
  connection,
  n = 3
)

close(connection)

print(first_records)
