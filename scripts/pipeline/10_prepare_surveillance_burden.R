# ==========================================================
# #10_prepare_surveillance_burden.R
#
# PURPOSE
#
# Standardize dengue surveillance burden records prior to
# burden aggregation and pathogen-country-year linkage.
#
# This script DOES NOT aggregate records.
#
# Weekly, monthly and yearly observations remain separate.
#
# OUTPUT
#
# surveillance_burden_clean.csv
#
# ==========================================================

library(tidyverse)
library(lubridate)

# ==========================================================
# PATHS
# ==========================================================

input_file <- "data/raw/opendengue_surveillance.csv"

output_dir <- "data/intermediate/burden"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==========================================================
# LOAD
# ==========================================================

burden <- read_csv(
  input_file,
  show_col_types = FALSE
)

# ==========================================================
# CASE DEFINITIONS
# ==========================================================

burden <- burden %>%
  
  mutate(
    
    case_definition_standardised =
      str_trim(
        case_definition_standardised
      ),
    
    case_definition_standardised =
      case_when(
        
        tolower(
          case_definition_standardised
        ) == "confirmed"
        ~ "Confirmed",
        
        TRUE
        ~ case_definition_standardised
        
      )
    
  )

# ==========================================================
# CASE-DEFINITION GROUPS
#
# Preserve original definitions while providing a simplified
# analytical grouping.
# ==========================================================

burden <- burden %>%
  
  mutate(
    
    case_definition_group = case_when(
      
      case_definition_standardised ==
        "Confirmed"
      ~ "Confirmed",
      
      case_definition_standardised ==
        "Probable"
      ~ "Probable",
      
      case_definition_standardised ==
        "Suspected"
      ~ "Suspected",
      
      case_definition_standardised %in%
        c(
          "Probable and confirmed",
          "Suspected and confirmed"
        )
      ~ "Mixed",
      
      case_definition_standardised ==
        "Total"
      ~ "Total",
      
      TRUE
      ~ "Other"
      
    )
    
  )

# ==========================================================
# GEOGRAPHY
# ==========================================================

burden <- burden %>%
  
  rename(
    
    country_name = adm_0_name,
    
    iso3 = ISO_A0,
    
    burden_year = Year
    
  )

# ==========================================================
# TEMPORAL VARIABLES
# ==========================================================

burden <- burden %>%
  
  mutate(
    
    temporal_resolution = T_res,
    
    period_start = calendar_start_date,
    
    period_end = calendar_end_date,
    
    burden_month = month(period_start),
    
    burden_week = isoweek(period_start)
    
  )

# ==========================================================
# PERIOD IDENTIFIER
#
# Useful for diagnostics and later aggregation.
# ==========================================================

burden <- burden %>%
  
  mutate(
    
    period_id = case_when(
      
      temporal_resolution == "Year"
      ~ paste0(
        iso3,
        "_",
        burden_year
      ),
      
      temporal_resolution == "Month"
      ~ paste0(
        iso3,
        "_",
        burden_year,
        "_M",
        sprintf(
          "%02d",
          burden_month
        )
      ),
      
      temporal_resolution == "Week"
      ~ paste0(
        iso3,
        "_",
        burden_year,
        "_W",
        sprintf(
          "%02d",
          burden_week
        )
      ),
      
      TRUE
      ~ NA_character_
      
    )
    
  )

# ==========================================================
# PROVENANCE
# ==========================================================

burden <- burden %>%
  
  mutate(
    
    burden_source =
      "OpenDengue",
    
    reported_or_modelled =
      "Reported"
    
  )

# ==========================================================
# OUTPUT
# ==========================================================

burden_clean <- burden %>%
  
  select(
    
    country_name,
    iso3,
    
    burden_year,
    
    period_start,
    period_end,
    
    temporal_resolution,
    burden_month,
    burden_week,
    period_id,
    
    dengue_total,
    
    case_definition_standardised,
    case_definition_group,
    
    burden_source,
    reported_or_modelled,
    
    everything()
    
  )

# ==========================================================
# SAVE
# ==========================================================

write_csv(
  burden_clean,
  file.path(
    output_dir,
    "dengue_surveillance_clean.csv"
  )
)

# ==========================================================
# QC
# ==========================================================

cat(
  "\n====================================\n"
)

cat(
  "SURVEILLANCE BURDEN CLEANING COMPLETE\n"
)

cat(
  "====================================\n"
)

cat(
  "\nYear range:\n"
)

print(
  
  burden_clean %>%
    
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
  "\nTemporal resolution:\n"
)

print(
  table(
    burden_clean$temporal_resolution
  )
)

cat(
  "\nCase definitions:\n"
)

print(
  table(
    burden_clean$case_definition_standardised
  )
)

cat(
  "\nCountries:\n"
)

print(
  n_distinct(
    burden_clean$iso3
  )
)

cat(
  "\nRows:\n"
)

print(
  nrow(
    burden_clean
  )
)

cat(
  "\nSaved:\n"
)

cat(
  file.path(
    output_dir,
    "surveillance_burden_clean.csv"
  ),
  "\n"
)