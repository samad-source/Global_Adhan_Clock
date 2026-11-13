
# ============================================================
# GLOBAL ADHAN CLOCK
# FILE: server.R
# PURPOSE: Global location selection and application functions
# ============================================================

library(shiny)

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # 1. LOAD GLOBAL CITY DATA
  # ----------------------------------------------------------
  
  global_locations <- read.csv(
    "www/global_cities.csv",
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8",
    colClasses = c(
      location_id = "character",
      country_code = "character"
    )
  )
  
  required_columns <- c(
    "location_id",
    "city",
    "country_code",
    "latitude",
    "longitude",
    "timezone",
    "country_name",
    "continent"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(global_locations)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing columns in global_cities.csv: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  # Standardise data types.
  global_locations$location_id <- as.character(
    global_locations$location_id
  )
  
  global_locations$city <- as.character(
    global_locations$city
  )
  
  global_locations$country_code <- as.character(
    global_locations$country_code
  )
  
  global_locations$country_name <- as.character(
    global_locations$country_name
  )
  
  global_locations$continent <- as.character(
    global_locations$continent
  )
  
  global_locations$timezone <- as.character(
    global_locations$timezone
  )
  
  # Population is optional.
  if (!"population" %in% names(global_locations)) {
    global_locations$population <- 0
  }
  
  global_locations$population <- suppressWarnings(
    as.numeric(global_locations$population)
  )
  
  global_locations$population[
    is.na(global_locations$population)
  ] <- 0
  
  # Records without a continent remain in the master dataset,
  # but are excluded from continent-based dropdown choices.
  
  continent_locations <- global_locations[
    !is.na(global_locations$continent) &
      nzchar(trimws(global_locations$continent)) &
      !is.na(global_locations$country_code) &
      nzchar(trimws(global_locations$country_code)),
    ,
    drop = FALSE
  ]
  
  # ----------------------------------------------------------
  # 2. HELPER FUNCTIONS
  # ----------------------------------------------------------
  
  get_field <- function(data, field, default = "") {
    
    if (is.null(data) || !field %in% names(data)) {
      return(default)
    }
    
    value <- data[[field]]
    
    if (
      is.null(value) ||
      length(value) == 0 ||
      is.na(value[1])
    ) {
      return(default)
    }
    
    as.character(value[1])
  }
  
  
  make_location_record <- function(row) {
    
    data.frame(
      location_id = as.character(row$location_id[1]),
      city = as.character(row$city[1]),
      country = as.character(row$country_name[1]),
      country_code = as.character(row$country_code[1]),
      continent = as.character(row$continent[1]),
      latitude = as.numeric(row$latitude[1]),
      longitude = as.numeric(row$longitude[1]),
      timezone = as.character(row$timezone[1]),
      stringsAsFactors = FALSE
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 3. INITIAL LOCATION
  # ----------------------------------------------------------
  
  lagos_record <- global_locations[
    global_locations$city == "Lagos" &
      global_locations$country_code == "NG",
    ,
    drop = FALSE
  ]
  
  if (nrow(lagos_record) == 0) {
    stop("Lagos could not be found in global_cities.csv.")
  }
  
  selected_location <- reactiveVal(
    make_location_record(
      lagos_record[1, , drop = FALSE]
    )
  )
  
  timezone_method <- reactiveVal(
    "GeoNames city timezone"
  )
  
  
  # ----------------------------------------------------------
  # 4. CURRENT TIME
  # ----------------------------------------------------------
  
  current_moment <- reactive({
    
    invalidateLater(1000, session)
    
    Sys.time()
    
  })
  
  
  # ----------------------------------------------------------
  # 5. CONTINENT CHOICES
  # ----------------------------------------------------------
  
  available_continents <- sort(
    unique(continent_locations$continent)
  )
  
  continent_choices <- setNames(
    available_continents,
    available_continents
  )
  
  initial_continent <- "Africa"
  
  if (!initial_continent %in% available_continents) {
    initial_continent <- available_continents[1]
  }
  
  
  # ----------------------------------------------------------
  # 6. COUNTRY CHOICES
  # ----------------------------------------------------------
  
  get_country_choices <- function(continent) {
    
    records <- continent_locations[
      continent_locations$continent == continent,
      ,
      drop = FALSE
    ]
    
    countries <- unique(
      records[
        ,
        c("country_code", "country_name"),
        drop = FALSE
      ]
    )
    
    countries <- countries[
      !is.na(countries$country_code) &
        nzchar(countries$country_code) &
        !is.na(countries$country_name) &
        nzchar(countries$country_name),
      ,
      drop = FALSE
    ]
    
    countries <- countries[
      order(countries$country_name),
      ,
      drop = FALSE
    ]
    
    setNames(
      countries$country_code,
      countries$country_name
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 7. CITY CHOICES
  # ----------------------------------------------------------
  
  get_city_choices <- function(country_code, continent = NULL) {
    
    records <- continent_locations[
      continent_locations$country_code == country_code,
      ,
      drop = FALSE
    ]
    
    if (!is.null(continent)) {
      records <- records[
        records$continent == continent,
        ,
        drop = FALSE
      ]
    }
    
    if (nrow(records) == 0) {
      return(character(0))
    }
    
    records <- records[
      order(
        -records$population,
        records$city
      ),
      ,
      drop = FALSE
    ]
    
    # Include population and unique ID in the labels.
    city_labels <- paste0(
      records$city,
      " (",
      records$location_id,
      ")"
    )
    
    setNames(
      records$location_id,
      city_labels
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 8. INITIAL DROPDOWN VALUES
  # ----------------------------------------------------------
  
  initial_country <- "NG"
  
  initial_country_choices <- get_country_choices(
    initial_continent
  )
  
  if (!initial_country %in% initial_country_choices) {
    initial_country <- unname(
      initial_country_choices[1]
    )
  }
  
  initial_city_choices <- get_city_choices(
    initial_country,
    initial_continent
  )
  
  initial_city_id <- as.character(
    lagos_record$location_id[1]
  )
  
  # ----------------------------------------------------------
  # 9. INITIALISE DROPDOWNS
  # ----------------------------------------------------------
  
  updateSelectInput(
    session,
    "continent_selector",
    choices = continent_choices,
    selected = initial_continent
  )
  
  updateSelectInput(
    session,
    "country_selector",
    choices = initial_country_choices,
    selected = initial_country
  )
  
  updateSelectInput(
    session,
    "city_selector",
    choices = initial_city_choices,
    selected = initial_city_id
  )
  
  
  # ----------------------------------------------------------
  # 10. CONTINENT CHANGE
  # ----------------------------------------------------------
  
  observeEvent(
    input$continent_selector,
    {
      
      continent <- input$continent_selector
      
      req(continent)
      
      new_country_choices <- get_country_choices(
        continent
      )
      
      if (length(new_country_choices) == 0) {
        return()
      }
      
      new_country <- unname(
        new_country_choices[1]
      )
      
      new_city_choices <- get_city_choices(
        new_country,
        continent
      )
      
      if (length(new_city_choices) == 0) {
        return()
      }
      
      new_city_id <- unname(
        new_city_choices[1]
      )
      
      # Prevent intermediate input updates from triggering
      # observers with stale values.
      
      freezeReactiveValue(input, "country_selector")
      freezeReactiveValue(input, "city_selector")
      
      updateSelectInput(
        session,
        "country_selector",
        choices = new_country_choices,
        selected = new_country
      )
      
      updateSelectInput(
        session,
        "city_selector",
        choices = new_city_choices,
        selected = new_city_id
      )
      
      matching <- global_locations[
        global_locations$location_id == new_city_id,
        ,
        drop = FALSE
      ]
      
      if (nrow(matching) == 1) {
        selected_location(
          make_location_record(matching)
        )
        
        timezone_method("GeoNames city timezone")
      }
      
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 11. COUNTRY CHANGE
  # ----------------------------------------------------------
  
  observeEvent(
    input$country_selector,
    {
      
      country_code <- input$country_selector
      
      req(country_code)
      
      continent <- input$continent_selector
      
      new_city_choices <- get_city_choices(
        country_code,
        continent
      )
      
      if (length(new_city_choices) == 0) {
        return()
      }
      
      new_city_id <- unname(
        new_city_choices[1]
      )
      
      freezeReactiveValue(input, "city_selector")
      
      updateSelectInput(
        session,
        "city_selector",
        choices = new_city_choices,
        selected = new_city_id
      )
      
      matching <- global_locations[
        global_locations$location_id == new_city_id,
        ,
        drop = FALSE
      ]
      
      if (nrow(matching) == 1) {
        selected_location(
          make_location_record(matching)
        )
        
        timezone_method("GeoNames city timezone")
      }
      
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 12. CITY CHANGE
  # ----------------------------------------------------------
  
  observeEvent(
    input$city_selector,
    {
      
      city_id <- input$city_selector
      
      req(city_id)
      
      matching <- global_locations[
        global_locations$location_id == city_id,
        ,
        drop = FALSE
      ]
      
      req(nrow(matching) == 1)
      
      selected_location(
        make_location_record(matching)
      )
      
      timezone_method("GeoNames city timezone")
      
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 13. TIMEZONE LOOKUP
  # ----------------------------------------------------------
  
  find_timezone <- function(latitude, longitude) {
    
    if (!requireNamespace("lutz", quietly = TRUE)) {
      stop(
        "The lutz package is not installed. ",
        "Run install.packages('lutz')."
      )
    }
    
    accurate_timezone <- tryCatch({
      
      lutz::tz_lookup_coords(
        lat = latitude,
        lon = longitude,
        method = "accurate"
      )
      
    }, error = function(e) {
      NA_character_
    })
    
    if (
      length(accurate_timezone) > 0 &&
      !is.na(accurate_timezone[1]) &&
      nzchar(accurate_timezone[1])
    ) {
      
      timezone_method(
        "Accurate geographical timezone lookup"
      )
      
      return(
        trimws(
          strsplit(
            accurate_timezone[1],
            ";",
            fixed = TRUE
          )[[1]][1]
        )
      )
      
    }
    
    fast_timezone <- tryCatch({
      
      lutz::tz_lookup_coords(
        lat = latitude,
        lon = longitude,
        method = "fast",
        warn = FALSE
      )
      
    }, error = function(e) {
      NA_character_
    })
    
    if (
      length(fast_timezone) > 0 &&
      !is.na(fast_timezone[1]) &&
      nzchar(fast_timezone[1])
    ) {
      
      timezone_method(
        "Fast timezone lookup fallback"
      )
      
      return(fast_timezone[1])
      
    }
    
    timezone_method("Timezone unavailable")
    
    stop(
      "Could not identify the timezone for these coordinates."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 14. CREATE LOCATION FROM GLOBE
  # ----------------------------------------------------------
  
  create_location_record <- function(incoming) {
    
    latitude <- suppressWarnings(
      as.numeric(incoming$latitude)
    )
    
    longitude <- suppressWarnings(
      as.numeric(incoming$longitude)
    )
    
    if (
      length(latitude) != 1 ||
      length(longitude) != 1 ||
      !is.finite(latitude) ||
      !is.finite(longitude) ||
      latitude < -90 ||
      latitude > 90 ||
      longitude < -180 ||
      longitude > 180
    ) {
      
      stop("Invalid geographical coordinates received.")
      
    }
    
    location_id <- get_field(
      incoming,
      "location_id"
    )
    
    # Use the master dataset whenever a real city is selected.
    
    if (
      nzchar(location_id) &&
      location_id %in% global_locations$location_id
    ) {
      
      matching <- global_locations[
        global_locations$location_id == location_id,
        ,
        drop = FALSE
      ]
      
      timezone_method("GeoNames city timezone")
      
      return(
        make_location_record(matching)
      )
      
    }
    
    city_name <- get_field(
      incoming,
      "city",
      "Selected Point"
    )
    
    country_name <- get_field(
      incoming,
      "country"
    )
    
    continent_name <- get_field(
      incoming,
      "continent"
    )
    
    country_code <- get_field(
      incoming,
      "country_code"
    )
    
    incoming_timezone <- get_field(
      incoming,
      "timezone"
    )
    
    if (
      nzchar(incoming_timezone) &&
      incoming_timezone %in% OlsonNames()
    ) {
      
      timezone <- incoming_timezone
      
      timezone_method("Timezone supplied by globe")
      
    } else {
      
      timezone <- find_timezone(
        latitude,
        longitude
      )
      
    }
    
    data.frame(
      location_id = location_id,
      city = city_name,
      country = country_name,
      country_code = country_code,
      continent = continent_name,
      latitude = latitude,
      longitude = longitude,
      timezone = timezone,
      stringsAsFactors = FALSE
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 15. SYNCHRONISE DROPDOWNS WITH GLOBE
  # ----------------------------------------------------------
  
  sync_dropdowns <- function(location_id) {
    
    matching <- global_locations[
      global_locations$location_id == location_id,
      ,
      drop = FALSE
    ]
    
    if (nrow(matching) != 1) {
      
      freezeReactiveValue(input, "continent_selector")
      freezeReactiveValue(input, "country_selector")
      freezeReactiveValue(input, "city_selector")
      
      updateSelectInput(
        session,
        "city_selector",
        choices = character(0),
        selected = character(0)
      )
      
      return(invisible(NULL))
      
    }
    
    continent <- matching$continent[1]
    country_code <- matching$country_code[1]
    
    if (
      is.na(continent) ||
      is.na(country_code) ||
      !nzchar(continent) ||
      !nzchar(country_code) ||
      !continent %in% available_continents
    ) {
      return(invisible(NULL))
    }
    
    country_choices <- get_country_choices(continent)
    
    city_choices <- get_city_choices(
      country_code,
      continent
    )
    
    freezeReactiveValue(input, "continent_selector")
    freezeReactiveValue(input, "country_selector")
    freezeReactiveValue(input, "city_selector")
    
    updateSelectInput(
      session,
      "continent_selector",
      choices = continent_choices,
      selected = continent
    )
    
    updateSelectInput(
      session,
      "country_selector",
      choices = country_choices,
      selected = country_code
    )
    
    updateSelectInput(
      session,
      "city_selector",
      choices = city_choices,
      selected = location_id
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 16. SELECT LOCATION FROM GLOBE
  # ----------------------------------------------------------
  
  observeEvent(
    input$globe_location,
    {
      
      incoming <- input$globe_location
      
      tryCatch({
        
        new_location <- create_location_record(
          incoming
        )
        
        selected_location(new_location)
        
        incoming_id <- new_location$location_id[1]
        
        if (
          !is.na(incoming_id) &&
          nzchar(incoming_id)
        ) {
          
          sync_dropdowns(incoming_id)
          
        } else {
          
          freezeReactiveValue(input, "city_selector")
          
          updateSelectInput(
            session,
            "city_selector",
            choices = character(0),
            selected = character(0)
          )
          
        }
        
      }, error = function(e) {
        
        showNotification(
          paste(
            "Location selection error:",
            e$message
          ),
          type = "error",
          duration = 8
        )
        
      })
      
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 17. SELECTED LOCATION INFORMATION
  # ----------------------------------------------------------
  
  output$selected_location_name <- renderText({
    
    selected_location()$city[1]
    
  })
  
  
  output$selected_location_country <- renderText({
    
    country <- selected_location()$country[1]
    
    if (is.na(country) || !nzchar(country)) {
      return("Country not identified")
    }
    
    country
    
  })
  
  
  output$selected_location_details <- renderText({
    
    location <- selected_location()
    
    country <- location$country[1]
    
    if (is.na(country) || !nzchar(country)) {
      country <- "Not identified"
    }
    
    paste0(
      location$city[1],
      ", ",
      country,
      "\nCoordinates: ",
      round(location$latitude[1], 5),
      ", ",
      round(location$longitude[1], 5),
      "\nTimezone: ",
      location$timezone[1],
      "\nTimezone method: ",
      timezone_method()
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 18. GLOBAL UTC CLOCK
  # ----------------------------------------------------------
  
  output$utc_clock <- renderText({
    
    moment <- current_moment()
    
    format(
      moment,
      format = "%H:%M:%S UTC",
      tz = "UTC"
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 19. SELECTED LOCATION CLOCK
  # ----------------------------------------------------------
  
  output$selected_local_time <- renderText({
    
    moment <- current_moment()
    
    timezone <- selected_location()$timezone[1]
    
    format(
      moment,
      format = "%H:%M:%S",
      tz = timezone
    )
    
  })
  
  
  output$selected_local_date <- renderText({
    
    moment <- current_moment()
    
    timezone <- selected_location()$timezone[1]
    
    paste(
      "Local date:",
      format(
        moment,
        format = "%A, %d %B %Y",
        tz = timezone
      )
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 20. GLOBAL DATASET COUNTS
  # ----------------------------------------------------------
  
  output$city_count <- renderText({
    
    nrow(global_locations)
    
  })
  
  
  output$country_count <- renderText({
    
    length(
      unique(
        global_locations$country_code[
          !is.na(global_locations$country_code)
        ]
      )
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 21. PRAYER-TIME CALCULATION
  # ----------------------------------------------------------
  
  selected_prayers <- reactive({
    
    location <- selected_location()
    
    req(location)
    req(input$research_date)
    
    get_location_prayer_times(
      selected_city = location,
      research_date = as.character(input$research_date)
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 22. EXTRACT PRAYER VALUES
  # ----------------------------------------------------------
  
  extract_prayer_value <- function(
    result,
    prayer,
    time_type
  ) {
    
    field_name <- paste0(
      tolower(prayer),
      "_",
      time_type
    )
    
    value <- result[[field_name]]
    
    if (
      is.null(value) ||
      length(value) == 0 ||
      is.na(value[1])
    ) {
      return("Unavailable")
    }
    
    as.character(value[1])
    
  }
  
  
  # ----------------------------------------------------------
  # 23. PRAYER TABLE
  # ----------------------------------------------------------
  
  output$prayer_table <- renderTable({
    
    result <- tryCatch(
      selected_prayers(),
      error = function(e) NULL
    )
    
    prayers <- c(
      "Fajr",
      "Dhuhr",
      "Asr",
      "Maghrib",
      "Isha"
    )
    
    if (is.null(result)) {
      
      return(
        data.frame(
          Prayer = prayers,
          Local_Time = rep("Unavailable", 5),
          UTC_Time = rep("Unavailable", 5)
        )
      )
      
    }
    
    local_times <- vapply(
      prayers,
      function(prayer) {
        extract_prayer_value(
          result,
          prayer,
          "local"
        )
      },
      character(1)
    )
    
    utc_times <- vapply(
      prayers,
      function(prayer) {
        extract_prayer_value(
          result,
          prayer,
          "utc"
        )
      },
      character(1)
    )
    
    data.frame(
      Prayer = prayers,
      Local_Time = local_times,
      UTC_Time = utc_times,
      check.names = FALSE
    )
    
  }, striped = TRUE, hover = TRUE, bordered = FALSE)
  
  
  # ----------------------------------------------------------
  # 24. PRAYER LOCATION INFORMATION
  # ----------------------------------------------------------
  
  output$prayer_location_heading <- renderText({
    
    location <- selected_location()
    
    paste(
      location$city[1],
      location$country[1],
      sep = ", "
    )
    
  })
  
  
  output$prayer_local_clock <- renderText({
    
    moment <- current_moment()
    
    timezone <- selected_location()$timezone[1]
    
    format(
      moment,
      format = "%H:%M:%S",
      tz = timezone
    )
    
  })
  
  
  output$prayer_timezone <- renderText({
    
    paste(
      "Timezone:",
      selected_location()$timezone[1]
    )
    
  })
  
  
  output$prayer_location_coordinates <- renderText({
    
    location <- selected_location()
    
    paste0(
      "Coordinates: ",
      round(location$latitude[1], 5),
      ", ",
      round(location$longitude[1], 5)
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 25. PRAYER CALCULATION INFORMATION
  # ----------------------------------------------------------
  
  output$prayer_calculation_information <- renderUI({
    
    result <- tryCatch(
      selected_prayers(),
      error = function(e) e
    )
    
    if (inherits(result, "error")) {
      
      return(
        div(
          class = "status-message status-warning",
          paste(
            "Prayer-time calculation unavailable:",
            result$message
          )
        )
      )
      
    }
    
    status <- result$prayer_status
    
    if (is.null(status)) {
      status <- "Calculation completed."
    }
    
    tagList(
      
      p(
        paste(
          "Engine status:",
          as.character(status)[1]
        )
      ),
      
      p(
        class = "muted-text",
        "Timezone method: ",
        timezone_method()
      ),
      
      p(
        class = "muted-text",
        "These are calculated prayer-time estimates. ",
        "They do not independently establish the actual ",
        "local Adhan broadcast time."
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 26. CALCULATION STATUS
  # ----------------------------------------------------------
  
  output$calculation_status <- renderUI({
    
    div(
      class = "status-message status-success",
      icon("check-circle"),
      " Prayer-time calculation engine connected"
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 27. VERIFIED COVERAGE STATUS
  # ----------------------------------------------------------
  
  output$verified_coverage_status <- renderUI({
    
    div(
      class = "status-message status-warning",
      "Verified broadcast observations are not connected yet."
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 28. UTC COVERAGE STATUS
  # ----------------------------------------------------------
  
  output$coverage_data_status <- renderUI({
    
    div(
      
      class = "empty-state",
      
      icon(
        "database",
        class = "empty-state-icon"
      ),
      
      h4("Observation dataset not connected"),
      
      p(
        "The 1,440-minute coverage engine will be connected ",
        "after the interval and observation foundations are implemented."
      ),
      
      p(
        class = "muted-text",
        "No verified coverage percentage is being reported."
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 29. ADHAN OBSERVATION STATUS
  # ----------------------------------------------------------
  
  output$observation_data_status <- renderUI({
    
    div(
      
      class = "empty-state",
      
      icon(
        "radio-tower",
        class = "empty-state-icon"
      ),
      
      h4("Awaiting verified observation records"),
      
      p(
        "This area will display actual broadcast observations ",
        "with timestamps, locations and evidence-quality information."
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 30. ANALYTICS STATUS
  # ----------------------------------------------------------
  
  output$geographic_analytics_status <- renderUI({
    
    div(
      class = "status-message status-neutral",
      "Geographic research visualisations will be connected in a later stage."
    )
    
  })
  
  
  output$seasonal_analytics_status <- renderUI({
    
    div(
      class = "status-message status-neutral",
      "Seasonal research visualisations will be connected in a later stage."
    )
    
  })
  
}
