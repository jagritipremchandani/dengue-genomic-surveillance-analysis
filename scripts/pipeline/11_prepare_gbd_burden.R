# ==========================================================
# 11_prepare_gbd_burden.R
#
# PURPOSE
#
# Standardize GBD dengue burden estimates prior to burden
# harmonization and surveillance linkage.
#
# GBD records represent MODELLED burden estimates and are
# intentionally kept separate from reported surveillance
# data.
#
# OUTPUT
#
# dengue_gbd_clean.csv
#
# ==========================================================

library(tidyverse)

# ==========================================================
# PATHS
# ==========================================================

input_file <- "data/raw/gbd_modelled_burden.csv"

output_dir <- "data/intermediate/burden"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==========================================================
# LOAD
# ==========================================================

gbd <- read_csv(
  input_file,
  show_col_types = FALSE
)

# ==========================================================
# STANDARDIZE COLUMN NAMES
# ==========================================================

gbd_clean <- gbd %>%
  
  rename(
    
    country = location_name,
    
    burden_year = year,
    
    estimate = val,
    
    estimate_upper = upper,
    
    estimate_lower = lower
    
  )

# ==========================================================
# PROVENANCE
# ==========================================================

gbd_clean <- gbd_clean %>%
  
  mutate(
    
    burden_source =
      "GBD",
    
    reported_or_modelled =
      "Modelled"
    
  )

# ==========================================================
# REORDER CORE FIELDS
# ==========================================================

gbd_clean <- gbd_clean %>%
  
  select(
    
    country,
    
    burden_year,
    
    measure_name,
    
    estimate,
    
    estimate_lower,
    
    estimate_upper,
    
    burden_source,
    
    reported_or_modelled,
    
    everything()
    
  )

# ==========================================================
# SAVE
# ==========================================================

write_csv(
  
  gbd_clean,
  
  file.path(
    output_dir,
    "dengue_gbd_clean.csv"
  )
  
)

# ==========================================================
# QC
# ==========================================================

cat(
  "\n====================================\n"
)

cat(
  "GBD BURDEN CLEANING COMPLETE\n"
)

cat(
  "====================================\n"
)

cat(
  "\nUnique countries:\n"
)

print(
  n_distinct(
    gbd_clean$country
  )
)

cat(
  "\nYear range:\n"
)

print(
  
  gbd_clean %>%
    
    summarise(
      
      min_year =
        min(
          burden_year,
          na.rm = TRUE
        ),
      
      max_year =
        max(
          burden_year,
          na.rm = TRUE
        )
      
    )
  
)

cat(
  "\nMeasures:\n"
)

print(
  table(
    gbd_clean$measure_name
  )
)

cat(
  "\nMetric:\n"
)

print(
  table(
    gbd_clean$metric_name
  )
)

cat(
  "\nSex:\n"
)

print(
  table(
    gbd_clean$sex_name
  )
)

cat(
  "\nAge:\n"
)

print(
  table(
    gbd_clean$age_name
  )
)

cat(
  "\nRows:\n"
)

print(
  nrow(
    gbd_clean
  )
)

cat(
  "\nSaved:\n"
)

cat(
  file.path(
    output_dir,
    "dengue_gbd_clean.csv"
  ),
  "\n"
)