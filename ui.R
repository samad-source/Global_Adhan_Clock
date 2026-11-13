
# ============================================================
# GLOBAL ADHAN CLOCK
# FILE: ui.R
# PURPOSE: Main application interface
# ============================================================

library(shiny)

# Register the existing www folder for globe and image assets.
addResourcePath(
  "globe",
  normalizePath("www", winslash = "/", mustWork = TRUE)
)

# ============================================================
# MAIN APPLICATION INTERFACE
# ============================================================

ui <- navbarPage(
  
  title = tags$span(
    class = "brand-title",
    tags$span(class = "brand-mark", "G"),
    "GLOBAL ADHAN CLOCK"
  ),
  
  id = "main_navigation",
  selected = "overview",
  inverse = TRUE,
  collapsible = TRUE,
  
  # ==========================================================
  # HEADER, CSS AND GLOBE COMMUNICATION
  # ==========================================================
  
  header = tags$head(
    
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "css/app.css"
    ),
    
    # Receive location selections from the Three.js globe.
    tags$script(HTML("
      console.log('Global Adhan Clock: Message listener installed.');

      window.addEventListener('message', function(event) {

        console.log('Dashboard received message:', event.data);

        console.log('Message origin:', event.origin);
        console.log('Dashboard origin:', window.location.origin);

        // Check that the message comes from this application.
        if (event.origin !== window.location.origin) {
          console.warn('Message rejected: origin mismatch.');
          return;
        }

        // Check the message type and location information.
        if (
          event.data &&
          event.data.type === 'global-adhan-location-selected' &&
          event.data.location
        ) {

          console.log('Valid globe location received:', event.data.location);

          // Pass the selected location to Shiny.
          if (
            window.Shiny &&
            typeof Shiny.setInputValue === 'function'
          ) {

            Shiny.setInputValue(
              'globe_location',
              event.data.location,
              {priority: 'event'}
            );

            console.log('Location successfully passed to Shiny.');

          } else {

            console.error('Shiny.setInputValue is not available.');

          }

        }

      });
    "))
  ),
  
  # ==========================================================
  # PAGE 1: GLOBAL OVERVIEW
  # ==========================================================
  
  tabPanel(
    title = "Overview",
    value = "overview",
    
    div(
      class = "app-container",
      
      div(
        class = "page-heading",
        div(class = "eyebrow", "GLOBAL PRAYER-TIME INTELLIGENCE"),
        h1("One Earth. One Global Clock."),
        p(
          "Explore prayer times, global time zones and the research ",
          "behind worldwide Adhan coverage."
        )
      ),
      
      # Main status metrics.
      fluidRow(
        
        column(
          3,
          div(
            class = "metric-card",
            div(class = "metric-label", "GLOBAL UTC TIME"),
            div(
              class = "metric-value clock-value",
              textOutput("utc_clock", inline = TRUE)
            ),
            div(class = "metric-caption", "Universal reference time")
          )
        ),
        
        column(
          3,
          div(
            class = "metric-card",
            div(class = "metric-label", "SELECTED LOCATION"),
            div(
              class = "metric-value metric-text",
              textOutput("selected_location_name", inline = TRUE)
            ),
            div(
              class = "metric-caption",
              textOutput("selected_location_country", inline = TRUE)
            )
          )
        ),
        
        column(
          3,
          div(
            class = "metric-card",
            div(class = "metric-label", "MONITORED LOCATIONS"),
            div(
              class = "metric-value",
              textOutput("city_count", inline = TRUE)
            ),
            div(class = "metric-caption", "Locations in current dataset")
          )
        ),
        
        column(
          3,
          div(
            class = "metric-card",
            div(class = "metric-label", "COUNTRIES"),
            div(
              class = "metric-value",
              textOutput("country_count", inline = TRUE)
            ),
            div(class = "metric-caption", "Countries represented")
          )
        )
      ),
      
      fluidRow(
        
        column(
          8,
          div(
            class = "content-card globe-preview-card",
            
            div(
              class = "card-heading",
              div(
                h3("Earth at a glance"),
                p("Explore locations and select a city on the globe.")
              ),
              span(class = "live-indicator", "INTERACTIVE GLOBE")
            ),
            
            tags$iframe(
              src = "/globe/globe.html",
              class = "globe-frame",
              title = "Interactive Earth globe"
            )
          )
        ),
        
        column(
          4,
          
          div(
            class = "content-card location-summary",
            h3("Selected location"),
            
            div(
              class = "large-clock",
              textOutput("selected_local_time", inline = TRUE)
            ),
            
            div(
              class = "muted-text",
              textOutput("selected_local_date", inline = TRUE)
            ),
            
            tags$hr(),
            
            textOutput("selected_location_details"),
            
            tags$hr(),
            
            div(class = "metric-label", "CALCULATION STATUS"),
            uiOutput("calculation_status")
          ),
          
          div(
            class = "content-card research-summary",
            h3("Global research"),
            
            p(
              "Our central research question examines whether an actual ",
              "Adhan broadcast is taking place somewhere on Earth ",
              "during every minute of a UTC day."
            ),
            
            div(class = "research-number", "1,440"),
            
            div(class = "metric-caption", "UTC minute intervals per day"),
            
            tags$hr(),
            
            div(class = "metric-label", "VERIFIED BROADCAST COVERAGE"),
            uiOutput("verified_coverage_status")
          )
        )
      ),
      
      div(
        class = "content-card",
        h3("Explore the platform"),
        
        fluidRow(
          
          column(
            4,
            div(
              class = "feature-item",
              icon("globe", class = "feature-icon"),
              h4("Explore Earth"),
              p("Select locations and explore global geography.")
            )
          ),
          
          column(
            4,
            div(
              class = "feature-item",
              icon("clock", class = "feature-icon"),
              h4("Prayer times"),
              p("View calculated prayer times for different locations.")
            )
          ),
          
          column(
            4,
            div(
              class = "feature-item",
              icon("chart-line", class = "feature-icon"),
              h4("Research"),
              p("Investigate global coverage using evidence-backed data.")
            )
          )
        )
      )
    )
  ),
  
  # ==========================================================
  # PAGE 2: EXPLORE EARTH
  # ==========================================================
  
  tabPanel(
    title = "Explore Earth",
    value = "earth",
    
    div(
      class = "app-container",
      
      div(
        class = "page-heading",
        div(class = "eyebrow", "GEOGRAPHIC EXPLORATION"),
        h1("Explore Earth"),
        p(
          "Select a location on the globe to explore its local ",
          "time and prayer information."
        )
      ),
      
      div(
        class = "content-card",
        
        tags$iframe(
          src = "/globe/globe.html",
          class = "globe-frame globe-frame-large",
          title = "Interactive Earth globe"
        )
      ),
      
      div(
        class = "content-card",
        h3("Selected location"),
        textOutput("selected_location_details"),
        
        div(
          class = "large-clock",
          textOutput("selected_local_time", inline = TRUE)
        ),
        
        textOutput("selected_local_date")
      )
    )
  ),
  
  # ==========================================================
  # PAGE 3: PRAYER TIMES
  # ==========================================================
  
  tabPanel(
    title = "Prayer Times",
    value = "prayers",
    
    div(
      class = "app-container",
      
      div(
        class = "page-heading",
        div(class = "eyebrow", "LOCAL PRAYER-TIME EXPERIENCE"),
        h1("Daily Prayer Times"),
        p(
          "Choose a continent, country and city to calculate ",
          "local daily prayer times."
        )
      ),
      
      # LOCATION SELECTORS
      div(
        class = "content-card",
        
        h3("Choose your location"),
        
        fluidRow(
          
          column(
            4,
            selectInput(
              inputId = "continent_selector",
              label = "1. Select continent",
              choices = character(0),
              width = "100%"
            )
          ),
          
          column(
            4,
            selectInput(
              inputId = "country_selector",
              label = "2. Select country",
              choices = character(0),
              width = "100%"
            )
          ),
          
          column(
            4,
            selectInput(
              inputId = "city_selector",
              label = "3. Select city",
              choices = character(0),
              width = "100%"
            )
          )
        ),
        
        p(
          class = "muted-text",
          "The available cities come from the global GeoNames dataset."
        )
      ),
      
      # DATE SELECTOR
      div(
        class = "content-card",
        
        dateInput(
          inputId = "research_date",
          label = "Select prayer date",
          value = Sys.Date(),
          width = "100%"
        )
      ),
      
      # SELECTED LOCATION
      div(
        class = "content-card",
        
        h3("Selected location"),
        
        textOutput("prayer_location_heading"),
        
        div(
          class = "large-clock",
          textOutput("prayer_local_clock", inline = TRUE)
        ),
        
        textOutput("prayer_timezone"),
        textOutput("prayer_location_coordinates")
      ),
      
      # PRAYER TIMES
      div(
        class = "content-card",
        
        h3("Five daily prayers"),
        
        p(
          class = "muted-text",
          "Times below are astronomical calculations using the ",
          "currently configured prayer-time method."
        ),
        
        tableOutput("prayer_table")
      ),
      
      # CALCULATION INFORMATION
      div(
        class = "content-card",
        
        h3("Calculation information"),
        uiOutput("prayer_calculation_information")
      )
    )
  ),
  
  # ==========================================================
  # PAGE 4: UTC COVERAGE
  # ==========================================================
  
  tabPanel(
    title = "UTC Coverage",
    value = "coverage",
    
    div(
      class = "app-container",
      
      div(
        class = "page-heading",
        div(class = "eyebrow", "GLOBAL CONTINUOUS-COVERAGE RESEARCH"),
        h1("The 1,440-Minute Timeline"),
        p(
          "Investigate whether verified Adhan broadcasts overlap ",
          "every minute of a UTC day."
        )
      ),
      
      div(
        class = "content-card",
        
        h3("Research date"),
        
        dateInput(
          "coverage_date",
          "Choose a UTC research date",
          value = Sys.Date()
        ),
        
        div(class = "research-number", "1,440"),
        p("Individual UTC minute intervals in one complete day.")
      ),
      
      div(
        class = "content-card",
        
        h3("Observation coverage"),
        uiOutput("coverage_data_status"),
        
        p(
          class = "muted-text",
          "The coverage engine will be connected in a later stage. ",
          "No actual broadcast coverage is claimed at this point."
        )
      )
    )
  ),
  
  # ==========================================================
  # PAGE 5: ADHAN OBSERVATIONS
  # ==========================================================
  
  tabPanel(
    title = "Observations",
    value = "observations",
    
    div(
      class = "app-container",
      
      div(
        class = "page-heading",
        div(class = "eyebrow", "EVIDENCE AND OBSERVATION RECORDS"),
        h1("Actual Adhan Observations"),
        p(
          "A dedicated space for independently supported broadcast ",
          "records, timestamps and evidence quality."
        )
      ),
      
      div(
        class = "content-card",
        
        h3("Observation dataset"),
        uiOutput("observation_data_status"),
        
        p(
          class = "muted-text",
          "A calculated prayer time is not an actual broadcast observation. ",
          "This section will only report records supported by suitable evidence."
        )
      )
    )
  ),
  
  # ==========================================================
  # PAGE 6: ANALYTICS
  # ==========================================================
  
  tabPanel(
    title = "Analytics",
    value = "analytics",
    
    div(
      class = "app-container",
      
      div(
        class = "page-heading",
        div(class = "eyebrow", "RESEARCH AND STATISTICAL ANALYSIS"),
        h1("Research Analytics"),
        p(
          "Examine global, geographic and seasonal patterns ",
          "when the relevant datasets are available."
        )
      ),
      
      fluidRow(
        
        column(
          6,
          div(
            class = "content-card",
            h3("Geographic analysis"),
            p("Location-level and regional comparisons."),
            uiOutput("geographic_analytics_status")
          )
        ),
        
        column(
          6,
          div(
            class = "content-card",
            h3("Seasonal analysis"),
            p("Compare research results across different seasons."),
            uiOutput("seasonal_analytics_status")
          )
        )
      )
    )
  ),
  
  # ==========================================================
  # PAGE 7: METHODS AND SETTINGS
  # ==========================================================
  
  tabPanel(
    title = "Methods",
    value = "methods",
    
    div(
      class = "app-container",
      
      div(
        class = "page-heading",
        div(class = "eyebrow", "SCIENTIFIC TRANSPARENCY"),
        h1("Methods and Settings"),
        p(
          "Understand how the application calculates and classifies its data."
        )
      ),
      
      div(
        class = "content-card",
        
        h3("Prayer-time calculation"),
        
        p(
          "The current baseline configuration uses the Muslim World League ",
          "convention: Fajr angle 18 degrees, Isha angle 17 degrees, ",
          "and the Standard Asr shadow factor."
        )
      ),
      
      div(
        class = "content-card",
        
        h3("Three different data categories"),
        
        tags$ul(
          tags$li("Calculated prayer times: astronomical estimates."),
          tags$li("Estimated Adhan intervals: modelled durations."),
          tags$li("Actual Adhan observations: independently supported records.")
        )
      ),
      
      div(
        class = "content-card",
        
        h3("Research principle"),
        
        p(
          "Missing observation records are classified as unknown. ",
          "They are not automatically treated as proof that no Adhan occurred."
        )
      )
    )
  )
)
