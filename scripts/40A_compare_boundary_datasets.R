
# ============================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 40A: COMPARE BOUNDARY DATASETS
# ============================================================

library(data.table)

cat("\n============================================\n")
cat("SCRIPT 40A: BOUNDARY DATASET COMPARISON\n")
cat("============================================\n")

base_dir <- file.path(
  "data", "processed", "four_season_experiment"
)

files <- list(
  original = file.path(
    base_dir,
    "four_season_prayer_events.csv"
  ),
  recovered = file.path(
    base_dir,
    "four_season_recovered_boundary_events.csv"
  ),
  corrected = file.path(
    base_dir,
    "four_season_prayer_events_boundary_corrected.csv"
  ),
  script40 = file.path(
    base_dir,
    "final_boundary_audit",
    "script40_corrected_prayer_events.csv"
  )
)

# ------------------------------------------------------------
# 1. CHECK FILE EXISTENCE
# ------------------------------------------------------------

cat("\nFILE EXISTENCE\n")

for (name in names(files)) {
  cat(
    name, ":",
    file.exists(files[[name]]),
    "\n"
  )
}

if (!all(file.exists(unlist(files)))) {
  stop("One or more required files are missing.")
}

# ------------------------------------------------------------
# 2. LOAD DATASETS
# ------------------------------------------------------------

datasets <- lapply(files, fread)

cat("\nROW COUNTS\n")

for (name in names(datasets)) {
  cat(
    name, ":",
    nrow(datasets[[name]]),
    "rows\n"
  )
}

# ------------------------------------------------------------
# 3. CHECK REQUIRED COLUMNS
# ------------------------------------------------------------

required <- c(
  "geoname_id",
  "local_date",
  "prayer"
)

cat("\nCOLUMN CHECK\n")

for (name in names(datasets)) {
  
  missing_columns <- setdiff(
    required,
    names(datasets[[name]])
  )
  
  cat("\n", name, "\n")
  
  if (length(missing_columns) == 0) {
    cat("All required columns present\n")
  } else {
    cat(
      "Missing:",
      paste(missing_columns, collapse = ", "),
      "\n"
    )
    print(names(datasets[[name]]))
    stop("Dataset schema requires investigation.")
  }
}

# ------------------------------------------------------------
# 4. GENERATE STABLE EVENT IDS
# ------------------------------------------------------------

for (name in names(datasets)) {
  
  dt <- datasets[[name]]
  
  dt[, event_id := paste(
    geoname_id,
    as.character(local_date),
    prayer,
    sep = "_"
  )]
  
  datasets[[name]] <- dt
}

# ------------------------------------------------------------
# 5. DUPLICATE CHECK
# ------------------------------------------------------------

cat("\nDUPLICATE EVENT ID CHECK\n")

for (name in names(datasets)) {
  
  duplicate_count <- datasets[[name]][
    ,
    .N,
    by = event_id
  ][N > 1, .N]
  
  cat(
    name,
    "duplicate IDs:",
    duplicate_count,
    "\n"
  )
}

# ------------------------------------------------------------
# 6. COMPARE RECOVERED EVENTS WITH CORRECTED DATA
# ------------------------------------------------------------

original_ids <- unique(datasets$original$event_id)

recovered_ids <- unique(datasets$recovered$event_id)

corrected_ids <- unique(datasets$corrected$event_id)

script40_ids <- unique(datasets$script40$event_id)

cat("\nSCRIPT 38 RECOVERY CHECK\n")

cat(
  "Recovered events:",
  length(recovered_ids),
  "\n"
)

cat(
  "Recovered IDs present in corrected dataset:",
  sum(recovered_ids %in% corrected_ids),
  "\n"
)

cat(
  "Recovered IDs missing from corrected dataset:",
  sum(!recovered_ids %in% corrected_ids),
  "\n"
)

cat(
  "Corrected dataset IDs absent from original:",
  sum(!corrected_ids %in% original_ids),
  "\n"
)

cat("\nSCRIPT 40 COMPARISON\n")

cat(
  "Corrected Script 38 IDs:",
  length(corrected_ids),
  "\n"
)

cat(
  "Script 40 IDs:",
  length(script40_ids),
  "\n"
)

cat(
  "Script 38 corrected IDs missing from Script 40:",
  sum(!corrected_ids %in% script40_ids),
  "\n"
)

# ------------------------------------------------------------
# 7. DISPLAY RECOVERED EVENTS NOT IN ORIGINAL
# ------------------------------------------------------------

new_ids <- setdiff(
  recovered_ids,
  original_ids
)

new_events <- datasets$recovered[
  event_id %in% new_ids
]

cat("\nRECOVERED EVENTS NOT IN ORIGINAL DATASET\n")
cat("Count:", nrow(new_events), "\n")

if (nrow(new_events) > 0) {
  print(
    new_events[
      ,
      .(
        season,
        research_date,
        geoname_id,
        place_name,
        local_date,
        prayer,
        event_utc,
        event_id
      )
    ][1:min(.N, 20)]
  )
}

# ------------------------------------------------------------
# 8. SAVE COMPARISON REPORT
# ------------------------------------------------------------

comparison <- data.table(
  dataset = names(datasets),
  rows = sapply(datasets, nrow),
  unique_event_ids = sapply(
    datasets,
    function(x) uniqueN(x$event_id)
  )
)

fwrite(
  comparison,
  file.path(
    base_dir,
    "script40A_dataset_comparison.csv"
  )
)

if (nrow(new_events) > 0) {
  fwrite(
    new_events,
    file.path(
      base_dir,
      "script40A_recovered_events_not_in_original.csv"
    )
  )
}

cat("\n============================================\n")
cat("SCRIPT 40A COMPLETED\n")
cat("============================================\n")
print(comparison)
