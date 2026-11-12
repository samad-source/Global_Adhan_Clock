
# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 02: EXTRACT POPULATED PLACES
# =====================================================


# 1. CHECK AND LOAD REQUIRED PACKAGE

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}

library(readr)


# 2. DEFINE FILE LOCATIONS

geonames_file <- "data/raw/geonames/allCountries.txt"

output_folder <- "data/processed"

output_file <- file.path(
  output_folder,
  "populated_places.csv"
)


# 3. CHECK THAT THE ORIGINAL DATASET EXISTS

if (!file.exists(geonames_file)) {
  stop("GeoNames dataset was not found!")
}

cat("Original GeoNames dataset found.\n")


# 4. CREATE THE PROCESSED DATA FOLDER

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# 5. REMOVE OLD OUTPUT BEFORE REBUILDING

if (file.exists(output_file)) {
  
  file.remove(output_file)
  
  cat("Previous processed dataset removed.\n")
  
}


# 6. DEFINE THE 19 GEONAMES COLUMN NAMES

geonames_columns <- c(
  "geoname_id",
  "name",
  "ascii_name",
  "alternate_names",
  "latitude",
  "longitude",
  "feature_class",
  "feature_code",
  "country_code",
  "country_code_2",
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


# 7. CREATE A COUNTER

total_populated_places <- 0


# 8. CREATE A FUNCTION TO PROCESS EACH CHUNK

process_chunk <- function(chunk, position) {
  
  # Keep only populated places
  
  populated <- chunk[
    !is.na(chunk$feature_class) &
      chunk$feature_class == "P",
  ]
  
  
  # Select the nine columns needed by our application
  
  populated <- populated[, c(
    "geoname_id",
    "name",
    "latitude",
    "longitude",
    "feature_class",
    "feature_code",
    "country_code",
    "population",
    "timezone"
  )]
  
  
  # Convert geographical coordinates to numbers
  
  populated$latitude <- as.numeric(
    populated$latitude
  )
  
  populated$longitude <- as.numeric(
    populated$longitude
  )
  
  
  # Convert population to numbers
  
  populated$population <- as.numeric(
    populated$population
  )
  
  
  # Save the processed chunk using standard CSV formatting
  
  if (nrow(populated) > 0) {
    
    append_output <- file.exists(output_file)
    
    readr::write_csv(
      populated,
      file = output_file,
      append = append_output,
      col_names = !append_output,
      na = ""
    )
    
    
    # Update the total number of populated places
    
    total_populated_places <<-
      total_populated_places + nrow(populated)
    
  }
  
  
  # Display processing progress
  
  cat(
    "Processed chunk. Populated places found:",
    total_populated_places,
    "\n"
  )
  
}


# 9. READ AND PROCESS THE LARGE GEONAMES DATASET

cat("\nStarting GeoNames processing...\n")

readr::read_tsv_chunked(
  
  file = geonames_file,
  
  callback = readr::DataFrameCallback$new(
    process_chunk
  ),
  
  chunk_size = 100000,
  
  col_names = geonames_columns,
  
  col_types = readr::cols(
    .default = readr::col_character()
  ),
  
  progress = TRUE
  
)


# 10. DISPLAY THE FINAL RESULTS

cat("\n====================================\n")

cat("PROCESSING COMPLETED!\n")

cat(
  "Total populated places:",
  total_populated_places,
  "\n"
)

cat(
  "Output file:",
  output_file,
  "\n"
)

cat("====================================\n")

cat("\nGLOBAL ADHAN CLOCK DATA PREPARATION COMPLETE\n")
