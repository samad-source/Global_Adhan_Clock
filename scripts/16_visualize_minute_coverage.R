# =====================================================
# GLOBAL ADHAN CLOCK
# SCRIPT 16: VISUALISE MINUTE COVERAGE
# =====================================================


# 1. Load our coverage results

coverage <- read.csv(
  "data/processed/coverage_test_results.csv"
)


# 2. Convert minute_utc into date-time format

coverage$minute_utc <- as.POSIXct(
  coverage$minute_utc,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)


# 3. Create the plots folder if it doesn't exist

if (!dir.exists("plots")) {
  dir.create("plots")
}


# 4. Save our timeline as an image

png(
  "plots/minute_coverage_timeline.png",
  width = 1400,
  height = 700
)


# 5. Draw the coverage timeline

plot(
  coverage$minute_utc,
  coverage$active_adhan_count,
  
  type = "h",
  
  xlab = "Time (UTC)",
  
  ylab = "Number of active test events",
  
  main = "Global Adhan Clock - Test Coverage Timeline",
  
  xaxt = "n",
  
  ylim = c(0, 3),
  
  lwd = 3
)


# 6. Add time labels every two hours

time_labels <- seq(
  from = as.POSIXct(
    "2026-01-01 00:00:00",
    tz = "UTC"
  ),
  to = as.POSIXct(
    "2026-01-01 22:00:00",
    tz = "UTC"
  ),
  by = "2 hours"
)

axis(
  side = 1,
  at = time_labels,
  labels = format(time_labels, "%H:%M", tz = "UTC")
)


# 7. Add a horizontal reference line

abline(h = 0, lty = 2)


# 8. Close the image file

dev.off()


# 9. Display completion message

cat("\n====================================\n")
cat("COVERAGE TIMELINE CREATED\n")
cat("====================================\n")

cat(
  "Saved image:",
  "plots/minute_coverage_timeline.png",
  "\n"
)