# ==========================================================
# 13_burden_qc.R
#
# PURPOSE
#
# Quality-control assessment of harmonized burden data
# prior to:
#
#   - burden aggregation
#   - country-year table construction
#   - genomic linkage
#
# INPUT
#
#   burden_master.csv
#
# OUTPUT
#
#   QC summaries only
#
# No data are modified by this script.
#
# ==========================================================

library(tidyverse)

# ==========================================================
# PATHS
# ==========================================================

burden_dir <- "data/intermediate/burden"

# ==========================================================
# LOAD
# ==========================================================

burden_master <- read_csv(
  file.path(
    burden_dir,
    "burden_master.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# SECTION 1
# GENERAL OVERVIEW
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 1: OVERVIEW\n"
)

cat(
  "==================================================\n"
)

cat(
  "\nTotal rows:\n"
)

print(
  nrow(
    burden_master
  )
)

cat(
  "\nUnique countries:\n"
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

# ==========================================================
# SECTION 2
# ISO COVERAGE
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 2: ISO COVERAGE\n"
)

cat(
  "==================================================\n"
)

cat(
  "\nMissing ISO3 rows:\n"
)

print(
  sum(
    is.na(
      burden_master$iso3
    )
  )
)

cat(
  "\nCountries with missing ISO3:\n"
)

print(
  
  burden_master %>%
    
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

# ==========================================================
# SECTION 3
# SOURCE STRUCTURE
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 3: BURDEN SOURCES\n"
)

cat(
  "==================================================\n"
)

print(
  table(
    burden_master$burden_source,
    useNA = "ifany"
  )
)

print(
  table(
    burden_master$reported_or_modelled,
    useNA = "ifany"
  )
)

# ==========================================================
# SECTION 4
# MEASURES
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 4: MEASURE STRUCTURE\n"
)

cat(
  "==================================================\n"
)

print(
  table(
    burden_master$measure,
    useNA = "ifany"
  )
)

cat(
  "\nMeasure × Source\n"
)

print(
  table(
    burden_master$measure,
    burden_master$burden_source
  )
)

cat(
  "\nMeasure × Reported/Modelled\n"
)

print(
  table(
    burden_master$measure,
    burden_master$reported_or_modelled
  )
)

# ==========================================================
# SECTION 5
# TEMPORAL STRUCTURE
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 5: TEMPORAL STRUCTURE\n"
)

cat(
  "==================================================\n"
)

print(
  table(
    burden_master$temporal_resolution,
    useNA = "ifany"
  )
)

cat(
  "\nYear range by source\n"
)

print(
  
  burden_master %>%
    
    group_by(
      burden_source
    ) %>%
    
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
        ),
      
      n_years =
        n_distinct(
          burden_year
        ),
      
      .groups = "drop"
      
    )
  
)

# ==========================================================
# SECTION 6
# CASE DEFINITIONS
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 6: CASE DEFINITIONS\n"
)

cat(
  "==================================================\n"
)

print(
  table(
    burden_master$case_definition_standardised,
    useNA = "ifany"
  )
)

print(
  table(
    burden_master$case_definition_group,
    useNA = "ifany"
  )
)

# ==========================================================
# SECTION 7
# DUPLICATE COUNTRY-YEARS
#
# Important before constructing pathogen-country-year
# tables.
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 7: COUNTRY-YEAR DUPLICATES\n"
)

cat(
  "==================================================\n"
)

country_year_counts <- burden_master %>%
  
  count(
    burden_source,
    iso3,
    burden_year
  )

cat(
  "\nCountry-year frequency distribution:\n"
)

print(
  country_year_counts %>%
    count(n)
)

cat(
  "\nMaximum rows in a country-year:\n"
)

print(
  max(
    country_year_counts$n
  )
)

# ==========================================================
# SECTION 8
# ESTIMATE QUALITY
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 8: ESTIMATE QUALITY\n"
)

cat(
  "==================================================\n"
)

cat(
  "\nMissing estimates:\n"
)

print(
  sum(
    is.na(
      burden_master$estimate
    )
  )
)

cat(
  "\nNegative estimates:\n"
)

print(
  sum(
    burden_master$estimate < 0,
    na.rm = TRUE
  )
)

cat(
  "\nEstimate summary:\n"
)

print(
  summary(
    burden_master$estimate
  )
)

# ==========================================================
# SECTION 9
# GBD UNCERTAINTY
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 9: GBD UNCERTAINTY\n"
)

cat(
  "==================================================\n"
)

gbd_only <- burden_master %>%
  
  filter(
    reported_or_modelled ==
      "Modelled"
  )

cat(
  "\nMissing lower bounds:\n"
)

print(
  sum(
    is.na(
      gbd_only$estimate_lower
    )
  )
)

cat(
  "\nMissing upper bounds:\n"
)

print(
  sum(
    is.na(
      gbd_only$estimate_upper
    )
  )
)

cat(
  "\nEstimate outside uncertainty interval:\n"
)

print(
  
  sum(
    
    gbd_only$estimate <
      gbd_only$estimate_lower |
      
      gbd_only$estimate >
      gbd_only$estimate_upper,
    
    na.rm = TRUE
    
  )
  
)

# ==========================================================
# SECTION 10
# LINKAGE READINESS
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "SECTION 10: LINKAGE READINESS\n"
)

cat(
  "==================================================\n"
)

linkage_summary <- burden_master %>%
  
  filter(
    !is.na(iso3)
  ) %>%
  
  summarise(
    
    countries =
      n_distinct(
        iso3
      ),
    
    years =
      n_distinct(
        burden_year
      ),
    
    rows =
      n()
    
  )

print(
  linkage_summary
)

# ==========================================================
# FINAL SUMMARY
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "BURDEN QC COMPLETE\n"
)


