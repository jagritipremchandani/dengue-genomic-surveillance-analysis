# ==========================================================
# Figure A5 Dataset
# Burden vs Genomic Surveillance Representation
#
# QUESTION
#
# Do higher-burden countries have greater genomic
# surveillance representation?
#
# INPUT
#
# pathogen_country_year.csv
#
# OUTPUT
#
# burden_surveillance_correlation.csv
#
# UNIT
#
# One row =
#
#   ISO3
#   + Year
#   + Burden Source
#   + Measure
#
# Genomic surveillance is represented using total
# source-level observations.
#
# ==========================================================

library(tidyverse)

# ==========================================================
# PATHS
# ==========================================================

burden_dir <- "data/intermediate/burden"

output_dir <- "outputs/figure_csvs"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==========================================================
# LOAD
# ==========================================================

pathogen_country_year <- read_csv(
  file.path(
    burden_dir,
    "pathogen_country_year.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# COUNTRY-YEAR SURVEILLANCE AGGREGATION
#
# Collapse across:
#
#   serotype
#   record_class
#
# because this analysis asks:
#
# "Do higher burden country-years receive more
# surveillance representation?"
#
# ==========================================================

burden_surveillance_correlation <- pathogen_country_year %>%
  
  group_by(
    
    iso3,
    
    country_name,
    
    collection_year,
    
    burden_source,
    
    reported_or_modelled,
    
    measure,
    
    estimate,
    
    estimate_lower,
    
    estimate_upper
    
  ) %>%
  
  summarise(
    
    n_sources =
      sum(
        n_sources,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    
    burden_source,
    
    measure,
    
    iso3,
    
    collection_year
    
  )

# ==========================================================
# SAVE
# ==========================================================

write_csv(
  
  burden_surveillance_correlation,
  
  file.path(
    output_dir,
    "burden_surveillance_correlation.csv"
  )
  
)

# ==========================================================
# QC
# ==========================================================

cat(
  "\n=====================================\n"
)

cat(
  "BURDEN-SURVEILLANCE CORRELATION DATASET CREATED\n"
)

cat(
  "=====================================\n"
)

cat(
  "\nRows:\n"
)

print(
  nrow(
    burden_surveillance_correlation
  )
)

cat(
  "\nMeasures:\n"
)

print(
  table(
    burden_surveillance_correlation$measure
  )
)

cat(
  "\nBurden Sources:\n"
)

print(
  table(
    burden_surveillance_correlation$burden_source
  )
)

cat(
  "\nSummary of Source Counts:\n"
)

print(
  summary(
    burden_surveillance_correlation$n_sources
  )
)

cat(
  "\nRows with surveillance observations:\n"
)

print(
  sum(
    burden_surveillance_correlation$n_sources > 0,
    na.rm = TRUE
  )
)

cat(
  "\nSaved:\n"
)

cat(
  file.path(
    output_dir,
    "burden_surveillance_correlation.csv"
  ),
  "\n"
)
