
# GLOBAL ADHAN CLOCK
# SHARED DATA AND CONFIGURATION

# Load geographical functions
source("geography.R")

# Create the global cities dataset
cities <- data.frame(
  
  city = c("Lagos", "London", "Tokyo", "Abuja"),
  
  country = c(
    "Nigeria",
    "United Kingdom",
    "Japan",
    "Nigeria"
  ),
  
  longitude = c(3.38, -0.13, 139.69, 7.40),
  
  latitude = c(6.52, 51.51, 35.68, 9.08),
  
  timezone = c(
    "Africa/Lagos",
    "Europe/London",
    "Asia/Tokyo",
    "Africa/Lagos"
  )
)

# Find Lagos in the dataset
lagos_row <- which(cities$city == "Lagos")

# Calculate the distance from Lagos to every city
cities$distance_from_lagos_km <- vapply(
  
  seq_len(nrow(cities)),
  
  function(i) {
    
    haversine_km(
      
      lat1 = cities$latitude[lagos_row],
      
      lon1 = cities$longitude[lagos_row],
      
      lat2 = cities$latitude[i],
      
      lon2 = cities$longitude[i]
    )
  },
  
  numeric(1)
)

# Round distances to two decimal places
cities$distance_from_lagos_km <-
  round(cities$distance_from_lagos_km, 2)
