
# GLOBAL ADHAN CLOCK
# USER INTERFACE

library(shiny)

# Register the globe and image directory
addResourcePath(
  "globe",
  normalizePath("www", winslash = "/", mustWork = TRUE)
)

# Define the application interface
ui <- fluidPage(
  
  titlePanel("Global Adhan Clock"),
  
  h3("Interactive 3D Earth"),
  
  tags$iframe(
    src = "/globe/globe.html",
    width = "100%",
    height = "650px",
    style = "border: none; border-radius: 12px;"
  ),
  
  hr(),
  
  h3("Live Global City Dashboard"),
  
  p(
    "Explore local times and geographical information ",
    "for cities around the world."
  ),
  
  tableOutput("live_clocks")
)
