
# GLOBAL ADHAN CLOCK
# GEOGRAPHICAL CALCULATIONS

# Calculate the distance between two geographical coordinates

haversine_km <- function(lat1, lon1, lat2, lon2) {
  
  # Earth's approximate radius in kilometres
  earth_radius <- 6371
  
  # Convert latitude to radians
  lat1_rad <- lat1 * pi / 180
  lat2_rad <- lat2 * pi / 180
  
  # Calculate coordinate differences
  delta_lat <- (lat2 - lat1) * pi / 180
  delta_lon <- (lon2 - lon1) * pi / 180
  
  # Apply the Haversine formula
  a <- sin(delta_lat / 2)^2 +
    cos(lat1_rad) * cos(lat2_rad) *
    sin(delta_lon / 2)^2
  
  # Calculate angular distance
  angle <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Return distance in kilometres
  earth_radius * angle
}
