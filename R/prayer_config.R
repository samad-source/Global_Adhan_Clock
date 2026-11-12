# =====================================================
# GLOBAL ADHAN CLOCK
# PRAYER CALCULATION CONFIGURATION
# =====================================================


# 1. Select our baseline calculation convention

prayer_config <- list(
  
  method = "Muslim World League",
  
  fajr_angle = 18,
  
  isha_angle = 17,
  
  asr_method = "Standard",
  
  asr_shadow_factor = 1,
  
  sunrise_sunset_zenith = 90.833
  
)


# 2. Check that our settings are valid

stopifnot(
  prayer_config$fajr_angle > 0,
  prayer_config$isha_angle > 0,
  prayer_config$asr_shadow_factor %in% c(1, 2)
)


# 3. Display our configuration

cat("\n====================================\n")

cat("GLOBAL ADHAN CLOCK\n")

cat("PRAYER CALCULATION SETTINGS\n")

cat("====================================\n")

cat("Method:", prayer_config$method, "\n")

cat("Fajr angle:", prayer_config$fajr_angle, "degrees\n")

cat("Isha angle:", prayer_config$isha_angle, "degrees\n")

cat("Asr method:", prayer_config$asr_method, "\n")

cat("Asr shadow factor:",
    prayer_config$asr_shadow_factor, "\n")

cat("Sunrise/sunset zenith:",
    prayer_config$sunrise_sunset_zenith, "degrees\n")