
# ============================================================
# GLOBAL ADHAN CLOCK
# SERVER.R
# GLOBAL DYNAMIC DASHBOARD
# ============================================================

library(shiny)

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # 1. LIVE CLOCK
  # ----------------------------------------------------------
  
  current_moment <- reactive({
    invalidateLater(1000, session)
    Sys.time()
  })
  
  
  # ----------------------------------------------------------
  # 2. SELECTED LOCATION
  # ----------------------------------------------------------
  
  selected_location <- reactiveVal(
    get_monitored_city(default_location_id)
  )
  
  
  # Receive selected city from the 3D globe.
  observeEvent(input$selected_location, {
    
    incoming <- input$selected_location
    
    if (is.null(incoming)) {
      return()
    }
    
    location_id <- incoming$location_id
    
    if (is.null(location_id) || length(location_id) != 1) {
      return()
    }
    
    matching_city <- cities[
      tolower(cities$location_id) == tolower(location_id),
      ,
      drop = FALSE
    ]
    
    # Only accept a location present in our global dataset.
    if (nrow(matching_city) == 1) {
      selected_location(matching_city)
    }
    
  }, ignoreInit = TRUE)
  
  
  # ----------------------------------------------------------
  # 3. CURRENT UTC TIME
  # ----------------------------------------------------------
  
  output$utc_clock <- renderText({
    
    format(
      current_moment(),
      "%Y-%m-%d %H:%M:%S UTC",
      tz = "UTC"
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 4. DASHBOARD STATISTICS
  # ----------------------------------------------------------
  
  output$city_count <- renderText({
    nrow(cities)
  })
  
  output$country_count <- renderText({
    length(unique(cities$country))
  })
  
  output$app_status <- renderText({
    "Online"
  })
  
  
  # ----------------------------------------------------------
  # 5. SELECTED LOCATION NAME
  # ----------------------------------------------------------
  
  output$selected_location_name <- renderText({
    
    selected <- selected_location()
    
    paste(
      selected$city[1],
      selected$country[1],
      sep = ", "
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 6. SELECTED LOCATION DETAILS
  # ----------------------------------------------------------
  
  output$selected_location_details <- renderText({
    
    selected <- selected_location()
    
    paste0(
      "Latitude: ",
      selected$latitude[1],
      " | Longitude: ",
      selected$longitude[1],
      " | Timezone: ",
      selected$timezone[1]
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 7. SELECTED LOCATION LIVE CLOCK
  # ----------------------------------------------------------
  
  output$selected_local_time <- renderText({
    
    selected <- selected_location()
    
    format(
      current_moment(),
      "%A, %d %B %Y | %H:%M:%S",
      tz = selected$timezone[1]
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 8. SELECTED LOCATION PRAYER CALCULATIONS
  # ----------------------------------------------------------
  
  selected_prayers <- reactive({
    
    # Refresh periodically to handle local midnight.
    invalidateLater(60000, session)
    
    selected <- selected_location()
    
    local_date <- format(
      Sys.time(),
      "%Y-%m-%d",
      tz = selected$timezone[1]
    )
    
    get_location_prayer_times(
      selected_city = selected,
      research_date = local_date
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 9. FIVE DAILY PRAYER TIMES
  # ----------------------------------------------------------
  
  output$selected_prayers <- renderTable({
    
    prayers <- selected_prayers()
    
    prayer_names <- c(
      "Fajr",
      "Dhuhr",
      "Asr",
      "Maghrib",
      "Isha"
    )
    
    prayer_fields <- c(
      "fajr",
      "dhuhr",
      "asr",
      "maghrib",
      "isha"
    )
    
    local_times <- vapply(
      prayer_fields,
      function(field) {
        
        value <- prayers[[paste0(field, "_local")]]
        
        if (is.null(value) || length(value) == 0 ||
            is.na(value[1])) {
          return("Unavailable")
        }
        
        as.character(value[1])
        
      },
      character(1)
    )
    
    utc_times <- vapply(
      prayer_fields,
      function(field) {
        
        value <- prayers[[paste0(field, "_utc")]]
        
        if (is.null(value) || length(value) == 0 ||
            is.na(value[1])) {
          return("Unavailable")
        }
        
        as.character(value[1])
        
      },
      character(1)
    )
    
    data.frame(
      Prayer = prayer_names,
      `Local Time` = local_times,
      `UTC Time` = utc_times,
      check.names = FALSE
    )
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
  # ----------------------------------------------------------
  # 10. DISTANCES FROM SELECTED LOCATION
  # ----------------------------------------------------------
  
  output$distances_table <- renderTable({
    
    get_distances_from_location(
      selected_location()
    )
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
  # ----------------------------------------------------------
  # 11. LIVE CLOCKS FOR ALL MONITORED CITIES
  # ----------------------------------------------------------
  
  output$live_clocks <- renderTable({
    
    moment <- current_moment()
    
    selected <- selected_location()
    
    distances <- get_distances_from_location(selected)
    
    data.frame(
      
      City = cities$city,
      
      Country = cities$country,
      
      Local_Date = vapply(
        seq_len(nrow(cities)),
        function(i) {
          format(
            moment,
            "%Y-%m-%d",
            tz = cities$timezone[i]
          )
        },
        character(1)
      ),
      
      Local_Time = vapply(
        seq_len(nrow(cities)),
        function(i) {
          format(
            moment,
            "%H:%M:%S",
            tz = cities$timezone[i]
          )
        },
        character(1)
      ),
      
      Timezone = cities$timezone,
      
      Distance_from_Selected_km =
        distances$distance_from_selected_km,
      
      check.names = FALSE
      
    )
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
}
