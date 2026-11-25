# ==========================================================
# 12_burden_harmonization.R
#
# PURPOSE
#
# Harmonize surveillance and modelled dengue burden data
# into a common burden schema while preserving provenance.
#
# THIS SCRIPT DOES NOT:
#
#   - aggregate burden estimates
#   - merge surveillance and modelled estimates
#   - perform genomic linkage
#
# OUTPUTS
#
#   burden_master.csv
#   country_iso_lookup_fi.csv
#
# DESIGN PRINCIPLE
#
# burden_master is a harmonized SOURCE dataset.
#
# Provenance is retained wherever possible.
#
# Analytical reductions and linkage occur later.
#
# ==========================================================

library(tidyverse)

# ==========================================================
# PATHS
# ==========================================================

burden_dir <- "data/intermediate/burden"

lookup_dir <- "data/intermediate/lookup_tables"

dir.create(
  lookup_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  burden_dir,
  "burden_master.csv"
)

# ==========================================================
# LOAD
# ==========================================================

surveillance <- read_csv(
  file.path(
    burden_dir,
    "dengue_surveillance_clean.csv"
  ),
  show_col_types = FALSE
)

gbd <- read_csv(
  file.path(
    burden_dir,
    "dengue_gbd_clean.csv"
  ),
  show_col_types = FALSE
)

country_iso_lookup <- read_csv(
  file.path(
    lookup_dir,
    "country_iso_lookup_final.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# Surveillance ISO validation
surveillance_missing_iso <- surveillance %>%
  filter(
    is.na(iso3)
  )

print(nrow(surveillance_missing_iso))

#==========================================================
# GBD ISO3 ENRICHMENT
#
# GBD contains:
#
#   country
#
# ==========================================================

gbd <- gbd %>%
  
  rename(
    country_name = country
  ) %>%
  
  left_join(
    country_iso_lookup %>%
      select(
        country_name,
        iso3
      ),
    by = "country_name"
  )

# ==========================================================
# DROP INVARIANT GBD DIMENSIONS
#
# The current GBD extract contains only:
#
#   population_group_name = All Population
#   sex_name              = Both
#   age_name              = All ages
#   metric_name           = Number
#
# These dimensions are invariant across the dataset and
# therefore contribute no analytical information.
#
# They are removed to reduce redundancy.
#
# Provenance-bearing variables are retained:
#
#   location_id
#   measure_id
#   measure_name
#   cause_id
#   cause_name
#
# ==========================================================

gbd <- gbd %>%
  
  select(
    
    -population_group_id,
    -population_group_name,
    
    -sex_id,
    -sex_name,
    
    -age_id,
    -age_name,
    
    -metric_id,
    -metric_name
    
  )

# ==========================================================
# SURVEILLANCE BURDEN
#
# Reported surveillance observations.
#
# Surveillance-specific metadata are intentionally
# retained because they inform later aggregation and
# interpretation decisions.
# ==========================================================

surveillance_master <- surveillance %>%
  
  mutate(
    
    pathogen = "dengue",
    
    measure = "Cases",
    
    estimate = dengue_total,
    
    estimate_lower = NA_real_,
    
    estimate_upper = NA_real_
    
  )

# ==========================================================
# GBD BURDEN
#
# Modelled burden estimates.
#
# estimate
# estimate_lower
# estimate_upper
#
# were standardized during GBD preparation and therefore
# require no additional renaming.
# ==========================================================

gbd_master <- gbd %>%
  
  mutate(
    
    pathogen = "dengue",
    
    measure = measure_name,
    
    temporal_resolution = NA_character_,
    
    case_definition_standardised =
      NA_character_,
    
    case_definition_group =
      NA_character_,
    
    calendar_start_date =
      as.Date(NA),
    
    calendar_end_date =
      as.Date(NA),
    
    UUID =
      NA_character_
    
  )

# ==========================================================
# COMBINE
# ==========================================================

burden_master <- bind_rows(
  
  surveillance_master,
  
  gbd_master
  
)

# ==========================================================
# SAVE
# ==========================================================

write_csv(
  burden_master,
  output_file
)

# ==========================================================
# QC
# ==========================================================

cat(
  "\n====================================\n"
)

cat(
  "BURDEN HARMONIZATION COMPLETE\n"
)

cat(
  "====================================\n"
)

# ----------------------------------------------------------
# ISO COVERAGE
# ----------------------------------------------------------

cat(
  "\nMissing ISO3 values:\n"
)

print(
  sum(
    is.na(
      burden_master$iso3
    )
  )
)

cat(
  "\nGBD Countries Without ISO3 Match:\n"
)

print(
  
  gbd %>%
    
    filter(
      is.na(iso3)
    ) %>%
    
    distinct(
      country_name
    ) %>%
    
    arrange(
      country_name
    )
  
)

# ----------------------------------------------------------
# SOURCES
# ----------------------------------------------------------

cat(
  "\nBurden Sources:\n"
)

print(
  table(
    burden_master$burden_source,
    useNA = "ifany"
  )
)

cat(
  "\nReported vs Modelled:\n"
)

print(
  table(
    burden_master$reported_or_modelled,
    useNA = "ifany"
  )
)

# ----------------------------------------------------------
# MEASURES
# ----------------------------------------------------------

cat(
  "\nMeasures by Source:\n"
)

print(
  table(
    burden_master$measure,
    burden_master$burden_source
  )
)
cat(
  "\nMeasure × Reported/Modelled:\n"
)

print(
  table(
    burden_master$measure,
    burden_master$reported_or_modelled
  )
)

# ----------------------------------------------------------
# TEMPORAL RESOLUTION
# ----------------------------------------------------------

cat(
  "\nTemporal Resolution:\n"
)

print(
  table(
    burden_master$temporal_resolution,
    useNA = "ifany"
  )
)

# ----------------------------------------------------------
# CASE DEFINITIONS
# ----------------------------------------------------------

cat(
  "\nCase Definition Groups:\n"
)

print(
  table(
    burden_master$case_definition_group,
    useNA = "ifany"
  )
)

# ----------------------------------------------------------
# YEAR RANGE
# ----------------------------------------------------------

cat(
  "\nYear Range:\n"
)

print(
  
  burden_master %>%
    
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

# ----------------------------------------------------------
# RECORD COUNTS
# ----------------------------------------------------------

cat(
  "\nTotal Rows:\n"
)

print(
  nrow(
    burden_master
  )
)

cat(
  "\nUnique Countries:\n"
)

print(
  n_distinct(
    burden_master$country_name
  )
)

cat(
  "\nUnique ISO3:\n"
)

print(
  n_distinct(
    burden_master$iso3
  )
)

# ----------------------------------------------------------
# OUTPUTS
# ----------------------------------------------------------

cat(
  "\nSaved:\n"
)

cat(
  output_file,
  "\n"
)

cat(
  file.path(
    lookup_dir,
    "country_iso_lookup_final.csv"
  ),
  "\n"
)