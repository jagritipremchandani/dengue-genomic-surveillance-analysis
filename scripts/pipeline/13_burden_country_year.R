# ==========================================================
# 13_burden_country_year.R
#
# Create canonical burden country-year table.
#
# ==========================================================

library(tidyverse)
burden_master_dir <- "data/intermediate/burden"
burden_dir <- "data/processed"

burden_master <- read_csv(
  file.path(
    burden_master_dir,
    "burden_master.csv"
  ),
  show_col_types = FALSE
)

dir.create(
  burden_dir,
  recursive = TRUE,
  showWarnings = FALSE
)
# ==========================================================
# OPENDENGUE
#
# Keep annual observations only.
# ==========================================================

surveillance_year <- burden_master %>%
  
  filter(
    burden_source == "OpenDengue",
    temporal_resolution == "Year"
  )

# ==========================================================
# GBD
#
# Already annual.
# ==========================================================

gbd_year <- burden_master %>%
  
  filter(
    burden_source == "GBD"
  )

# ==========================================================
# COMBINE
# ==========================================================

burden_country_year <- bind_rows(
  
  surveillance_year,
  gbd_year
  
)

write_csv(
  burden_country_year,
  file.path(
    burden_dir,
    "burden_country_year.csv"
  )
)

# ==========================================================
# QC
# ==========================================================

cat("\nRows:\n")
print(nrow(burden_country_year))

cat("\nSources:\n")
print(table(burden_country_year$burden_source))

cat("\nMeasures:\n")
print(table(burden_country_year$measure))
