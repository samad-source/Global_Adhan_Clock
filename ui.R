
# ============================================================
# GLOBAL ADHAN CLOCK
# UI.R
# GLOBAL DASHBOARD INTERFACE
# ============================================================

library(shiny)

# Register the globe and image directory.
addResourcePath(
  "globe",
  normalizePath(
    "www",
    winslash = "/",
    mustWork = TRUE
  )
)


ui <- fluidPage(
  
  # ----------------------------------------------------------
  # PAGE STYLING
  # ----------------------------------------------------------
  
  tags$head(
    
    tags$title("Global Adhan Clock"),
    
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    
    tags$style(HTML("

      body {
        background: #101827;
        color: #f1f5f9;
        font-family: Arial, sans-serif;
      }

      .container-fluid {
        padding: 25px;
      }

      .main-header {
        background: #172438;
        padding: 25px;
        border-radius: 15px;
        margin-bottom: 22px;
        border: 1px solid #26374e;
      }

      .main-header h1 {
        color: #ffffff;
        font-weight: bold;
        margin-top: 0;
      }

      .main-header p {
        color: #a8b6c9;
      }

      .section-heading {
        color: #e5edf8;
        font-weight: bold;
        margin-top: 28px;
        margin-bottom: 15px;
      }

      .metric-card {
        background: #172438;
        padding: 20px;
        border-radius: 12px;
        border: 1px solid #26374e;
        margin-bottom: 15px;
        min-height: 115px;
      }

      .metric-card h4 {
        color: #9fb0c7;
        margin-top: 0;
        font-size: 14px;
      }

      .metric-card h2 {
        color: #ffffff;
        font-weight: bold;
        margin-bottom: 0;
      }

      .clock-panel {
        background: #172438;
        padding: 22px;
        border-radius: 13px;
        border: 1px solid #26374e;
        margin-bottom: 20px;
      }

      .clock-panel h3 {
        color: #e8f0fb;
        margin-top: 0;
      }

      .selected-city {
        color: #70d6b0;
        font-size: 26px;
        font-weight: bold;
      }

      .local-clock {
        color: #70d6b0;
        font-size: 24px;
        font-weight: bold;
      }

      .globe-panel {
        background: #172438;
        padding: 10px;
        border-radius: 13px;
        border: 1px solid #26374e;
        margin-bottom: 20px;
      }

      .globe-frame {
        width: 100%;
        height: 650px;
        border: none;
        border-radius: 10px;
        display: block;
      }

      table {
        color: #e5edf8 !important;
        background: #172438 !important;
      }

      table th {
        color: #70d6b0 !important;
        background: #1d3048 !important;
      }

      table td {
        color: #e5edf8 !important;
      }

      .table {
        margin-bottom: 0;
      }

      .status-text {
        color: #70d6b0;
      }

      .footer-text {
        text-align: center;
        color: #8091a9;
        padding: 20px;
        margin-top: 20px;
      }

    ")),
    
    # --------------------------------------------------------
    # GLOBE-TO-SHINY MESSAGE BRIDGE
    # --------------------------------------------------------
    
    tags$script(HTML("

      window.addEventListener('message', function(event) {

        // Accept messages only from this same application.
        if (event.origin !== window.location.origin) {
          return;
        }

        if (
          event.data &&
          event.data.type === 'global-adhan-location-selected'
        ) {

          if (
            window.Shiny &&
            event.data.location
          ) {

            Shiny.setInputValue(
              'selected_location',
              event.data.location,
              { priority: 'event' }
            );

          }

        }

      });

    "))
    
  ),
  
  
  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------
  
  div(
    class = "main-header",
    
    h1("GLOBAL ADHAN CLOCK"),
    
    p(
      "A global astronomical prayer-time and Adhan coverage platform."
    ),
    
    p(
      "Explore locations across Earth and examine their local prayer times."
    )
  ),
  
  
  # ----------------------------------------------------------
  # DASHBOARD METRICS
  # ----------------------------------------------------------
  
  fluidRow(
    
    column(
      3,
      div(
        class = "metric-card",
        h4("CURRENT UTC TIME"),
        h2(textOutput("utc_clock"))
      )
    ),
    
    column(
      3,
      div(
        class = "metric-card",
        h4("MONITORED LOCATIONS"),
        h2(textOutput("city_count"))
      )
    ),
    
    column(
      3,
      div(
        class = "metric-card",
        h4("COUNTRIES REPRESENTED"),
        h2(textOutput("country_count"))
      )
    ),
    
    column(
      3,
      div(
        class = "metric-card",
        h4("APPLICATION STATUS"),
        h2(
          textOutput("app_status"),
          class = "status-text"
        )
      )
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # INTERACTIVE EARTH
  # ----------------------------------------------------------
  
  h3(
    "Interactive 3D Earth",
    class = "section-heading"
  ),
  
  div(
    class = "globe-panel",
    
    tags$iframe(
      src = "/globe/globe.html",
      class = "globe-frame"
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # SELECTED LOCATION
  # ----------------------------------------------------------
  
  h3(
    "Selected Location",
    class = "section-heading"
  ),
  
  div(
    class = "clock-panel",
    
    h3(
      textOutput("selected_location_name"),
      class = "selected-city"
    ),
    
    p(
      textOutput("selected_location_details")
    ),
    
    h3("Current Local Time"),
    
    h2(
      textOutput("selected_local_time"),
      class = "local-clock"
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # DAILY PRAYER TIMES
  # ----------------------------------------------------------
  
  h3(
    "Daily Prayer Times",
    class = "section-heading"
  ),
  
  div(
    class = "clock-panel",
    
    tableOutput("selected_prayers")
    
  ),
  
  
  # ----------------------------------------------------------
  # DISTANCES
  # ----------------------------------------------------------
  
  h3(
    "Distances from Selected Location",
    class = "section-heading"
  ),
  
  div(
    class = "clock-panel",
    
    tableOutput("distances_table")
    
  ),
  
  
  # ----------------------------------------------------------
  # LIVE CITY CLOCKS
  # ----------------------------------------------------------
  
  h3(
    "Global Monitored Location Clocks",
    class = "section-heading"
  ),
  
  div(
    class = "clock-panel",
    
    tableOutput("live_clocks")
    
  ),
  
  
  # ----------------------------------------------------------
  # FOOTER
  # ----------------------------------------------------------
  
  div(
    class = "footer-text",
    
    "GLOBAL ADHAN CLOCK | Astronomy • Geography • Data Science"
  )
  
)
