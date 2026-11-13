
# ==========================================
# GLOBAL ADHAN CLOCK
# APPLICATION LAUNCHER
# ==========================================

# Confirm that we are in the project directory
project_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

cat("\n====================================\n")
cat("GLOBAL ADHAN CLOCK\n")
cat("STARTING APPLICATION\n")
cat("====================================\n")

cat("\nProject directory:\n")
cat(project_dir, "\n")

# Check that the essential application files exist
required_files <- c(
  "ui.R",
  "server.R"
)

missing_files <- required_files[
  !file.exists(file.path(project_dir, required_files))
]

if (length(missing_files) > 0) {
  stop(
    paste(
      "Missing application files:",
      paste(missing_files, collapse = ", ")
    )
  )
}

cat("\nApplication files verified.\n")
cat("Launching Global Adhan Clock...\n\n")

# Start the Shiny application
shiny::runApp(
  appDir = project_dir,
  launch.browser = TRUE
)
