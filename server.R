
# GLOBAL ADHAN CLOCK
# SERVER LOGIC

server <- function(input, output, session) {
  
  # Get the current moment and refresh every second
  current_moment <- reactive({
    
    invalidateLater(1000, session)
    
    Sys.time()
  })
  
  # Display information about all cities
  output$live_clocks <- renderTable({
    
    moment <- current_moment()
    
    data.frame(
      
      City = cities$city,
      
      Country = cities$country,
      
      Latitude = cities$latitude,
      
      Longitude = cities$longitude,
      
      Time_Zone = cities$timezone,
      
      Local_Date = vapply(
        cities$timezone,
        function(tz) {
          format(moment, tz = tz, format = "%Y-%m-%d")
        },
        character(1)
      ),
      
      Time_24_Hour = vapply(
        cities$timezone,
        function(tz) {
          format(moment, tz = tz, format = "%H:%M:%S")
        },
        character(1)
      ),
      
      Time_12_Hour = vapply(
        cities$timezone,
        function(tz) {
          format(moment, tz = tz, format = "%I:%M:%S %p")
        },
        character(1)
      ),
      
      Distance_from_Lagos_km = cities$distance_from_lagos_km
      
    )
  })
}
