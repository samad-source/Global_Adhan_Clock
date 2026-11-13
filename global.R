# ============================================================
# GLOBAL ADHAN CLOCK
# GLOBAL.R
# SHARED GLOBAL DATA AND CONFIGURATION
# ============================================================

# ------------------------------------------------------------
# 1. LOAD GEOGRAPHICAL FUNCTIONS
# ------------------------------------------------------------

source("geography.R")


# ------------------------------------------------------------
# 2. LOAD THE PRAYER-TIME ENGINE
# ------------------------------------------------------------

# Load the engine only if it has not already been loaded.
if (!exists("calculate_prayer_times_for_place",
            mode = "function")) {
  
  source("R/prayer_times.R")
}


# ------------------------------------------------------------
# 3. GLOBAL LOCATION DATASET
# ------------------------------------------------------------

# Every monitored location follows the same data structure.
# To add a city later, add one new row to this dataset.

cities <- data.frame(
  
  city = c(
    "Lagos",
    "Abuja",
    "London",
    "Tokyo"
  ),
  
  country = c(
    "Nigeria",
    "Nigeria",
    "United Kingdom",
    "Japan"
  ),
  
  latitude = c(
    6.5244,
    9.0765,
    51.5072,
    35.6762
  ),
  
  longitude = c(
    3.3792,
    7.3986,
    -0.1276,
    139.6503
  ),
  
  timezone = c(
    "Africa/Lagos",
    "Africa/Lagos",
    "Europe/London",
    "Asia/Tokyo"
  ),
  
  stringsAsFactors = FALSE
)


# Give every location a permanent identifier.
cities$location_id <- paste(
  tolower(gsub("[^A-Za-z0-9]+", "-", cities$city)),
  tolower(gsub("[^A-Za-z0-9]+", "-", cities$country)),
  sep = "-"
)


# ------------------------------------------------------------
# 4. FIND A LOCATION BY ITS ID
# ------------------------------------------------------------

get_monitored_city <- function(location_id) {
  
  result <- cities[
    cities$location_id == location_id,
    ,
    drop = FALSE
  ]
  
  if (nrow(result) == 0) {
    stop("Location not found in the global dataset.")
  }
  
  result
}


# ------------------------------------------------------------
# 5. CALCULATE DISTANCES FROM ANY SELECTED LOCATION
# ------------------------------------------------------------

get_distances_from_location <- function(selected_city) {
  
  distances <- vapply(
    
    seq_len(nrow(cities)),
    
    function(i) {
      
      haversine_km(
        
        lat1 = selected_city$latitude[1],
        lon1 = selected_city$longitude[1],
        
        lat2 = cities$latitude[i],
        lon2 = cities$longitude[i]
      )
    },
    
    numeric(1)
  )
  
  data.frame(
    
    city = cities$city,
    country = cities$country,
    
    distance_from_selected_km = round(
      distances,
      2
    ),
    
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# 6. CALCULATE PRAYER TIMES FOR ANY LOCATION
# ------------------------------------------------------------

get_location_prayer_times <- function(selected_city,
                                      research_date = NULL) {
  
  if (is.null(research_date)) {
    
    research_date <- format(
      Sys.time(),
      "%Y-%m-%d",
      tz = selected_city$timezone[1]
    )
  }
  
  calculate_prayer_times_for_place(
    
    research_date = research_date,
    
    latitude = selected_city$latitude[1],
    
    longitude = selected_city$longitude[1],
    
    timezone = selected_city$timezone[1]
  )
}


# ------------------------------------------------------------
# 7. DEFAULT SELECTED LOCATION
# ------------------------------------------------------------

# Lagos is only the initial display selection.
# It is NOT the permanent reference point of the application.

default_location_id <- "lagos-nigeria"


# ------------------------------------------------------------
# 8. STARTUP INFORMATION
# ------------------------------------------------------------

cat("\n")
cat("============================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("GLOBAL DATA LOADED\n")
cat("============================================\n")

cat("Monitored locations:", nrow(cities), "\n")
cat("Countries represented:", length(unique(cities$country)), "\n")
cat("Prayer calculation engine: Ready\n")
cat("Default display location: Lagos\n")

cat("============================================\n\n")