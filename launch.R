
# GLOBAL ADHAN CLOCK
# APPLICATION LAUNCHER

library(shiny)

# Load the saved application from app.R
app_environment <- new.env(parent = globalenv())

app <- source(
  "app.R",
  local = app_environment
)$value

# Launch the complete application
shiny::runApp(
  app,
  launch.browser = TRUE
)
