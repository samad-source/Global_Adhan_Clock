
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 41: DOWNLOAD GLOBAL CITY DATA
# ============================================================

cat("\n============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("SCRIPT 41: GLOBAL CITY DATA\n")
cat("============================================\n\n")

# 1. DIRECTORIES

data_folder <- "data"
www_folder <- "www"

dir.create(data_folder, showWarnings = FALSE)
dir.create(www_folder, showWarnings = FALSE)

zip_path <- file.path(data_folder, "cities5000.zip")

csv_path <- file.path(data_folder, "global_cities.csv")

browser_csv_path <- file.path(www_folder, "global_cities.csv")

# 2. DOWNLOAD

download_url <- paste0(
  "https://download.geonames.org/",
  "export/dump/cities5000.zip"
)

if (!file.exists(zip_path)) {
  
  cat("Downloading GeoNames dataset...\n")
  
  download.file(
    download_url,
    zip_path,
    mode = "wb"
  )
  
} else {
  
  cat("Existing ZIP file found.\n")
  
}

# 3. CHECK ZIP

if (!file.exists(zip_path) ||
    file.info(zip_path)$size == 0) {
  
  stop("ZIP download failed.")
  
}

zip_contents <- unzip(zip_path, list = TRUE)

if (nrow(zip_contents) == 0) {
  stop("ZIP archive is empty.")
}

inner_file <- zip_contents$Name[1]

# 4. READ DATA

cat("Reading GeoNames records...\n")

geonames <- read.delim(
  unz(zip_path, inner_file),
  header = FALSE,
  sep = "\t",
  quote = "",
  fill = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

geonames_columns <- c(
  "geoname_id",
  "name",
  "asciiname",
  "alternatenames",
  "latitude",
  "longitude",
  "feature_class",
  "feature_code",
  "country_code",
  "cc2",
  "admin1_code",
  "admin2_code",
  "admin3_code",
  "admin4_code",
  "population",
  "elevation",
  "dem",
  "timezone",
  "modification_date"
)

if (ncol(geonames) != length(geonames_columns)) {
  stop("Unexpected GeoNames column count.")
}

names(geonames) <- geonames_columns

# 5. SELECT FIELDS

global_cities <- data.frame(
  
  location_id = as.character(geonames$geoname_id),
  
  city = geonames$name,
  
  country_code = geonames$country_code,
  
  latitude = as.numeric(geonames$latitude),
  
  longitude = as.numeric(geonames$longitude),
  
  feature_code = geonames$feature_code,
  
  population = as.numeric(geonames$population),
  
  timezone = geonames$timezone,
  
  stringsAsFactors = FALSE
  
)

# 6. CLEAN DATA

global_cities <- global_cities[
  !is.na(global_cities$latitude) &
    !is.na(global_cities$longitude) &
    global_cities$latitude >= -90 &
    global_cities$latitude <= 90 &
    global_cities$longitude >= -180 &
    global_cities$longitude <= 180 &
    !is.na(global_cities$city) &
    nzchar(global_cities$city),
]

global_cities <- global_cities[
  !duplicated(global_cities$location_id),
]

rownames(global_cities) <- NULL

# 7. SAVE MASTER DATASET

write.csv(
  global_cities,
  csv_path,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 8. EXPORT FOR THREE.JS

write.csv(
  global_cities,
  browser_csv_path,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 9. VALIDATION

cat("\n============================================\n")
cat("GLOBAL LOCATION DATA REPORT\n")
cat("============================================\n")

cat("Total locations:", nrow(global_cities), "\n")

cat(
  "Unique countries:",
  length(unique(global_cities$country_code)),
  "\n"
)

cat(
  "Unique IDs:",
  length(unique(global_cities$location_id)),
  "\n"
)

cat(
  "Missing coordinates:",
  sum(
    is.na(global_cities$latitude) |
      is.na(global_cities$longitude)
  ),
  "\n"
)

cat("\nMASTER DATASET:\n")
cat(csv_path, "\n")

cat("\nBROWSER DATASET:\n")
cat(browser_csv_path, "\n")

cat("\nBROWSER FILE EXISTS:\n")
print(file.exists(browser_csv_path))

cat("\n============================================\n")
cat("SCRIPT 41 COMPLETED\n")
cat("============================================\n")

cat(
  "\nData source: GeoNames.org",
  "\nhttps://www.geonames.org/",
  "\nDataset: cities5000.zip",
  "\nLicense: Creative Commons Attribution\n"
)
