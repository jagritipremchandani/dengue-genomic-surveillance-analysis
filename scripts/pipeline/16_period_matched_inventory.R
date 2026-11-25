# ==========================================================
# 16_period_matched_inventory.R
#
# PURPOSE
#
# Create a burden-compatible genomic inventory for
# pathogen-country-year analyses.
#
# This script preserves the original Source Inventory 
# while generating a period-matched subset suitable for
# burden linkage.
#
# INPUT
#
#   source_inventory.csv
#
# OUTPUTS
#
#   source_inventory_period_matched.csv
#   source_inventory_unmatched.csv
#
# PERIOD-MATCHING RULES
#
# Keep records only when:
#
#   1. ISO3 is available
#   2. collection_year is available
#   3. collection_year falls within burden coverage
#
# This script does NOT alter:
#
#   source_inventory.csv
#
# ==========================================================

library(tidyverse)

# ==========================================================
# PATHS
# ==========================================================

inventory_dir <- "data/intermediate/inventories"

output_dir <- "data/processed"

# ==========================================================
# LOAD
# ==========================================================

source_inventory <- read_csv(
  file.path(
    inventory_dir,
    "source_inventory.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# BURDEN COVERAGE WINDOW
#
# Current burden data coverage:
#
# OpenDengue : 1924–2025
# GBD        : 1980–2023
#
# We use the union of available burden years because
# pathogen-country-year linkage can use either source.
#
# ==========================================================

burden_min_year <- 1924

burden_max_year <- 2025

# ==========================================================
# CLASSIFY LINKAGE ELIGIBILITY
# ==========================================================

source_inventory <- source_inventory %>%
  
  mutate(
    
    linkage_status = case_when(
      
      is.na(country)
      ~ "missing_country",
      
      is.na(collection_year)
      ~ "missing_year",
      
      collection_year < burden_min_year
      ~ "pre_burden_coverage",
      
      collection_year > burden_max_year
      ~ "post_burden_coverage",
      
      TRUE
      ~ "eligible"
      
    )
    
  )

# ==========================================================
# PERIOD-MATCHED INVENTORY
# ==========================================================

source_inventory_period_matched <- source_inventory %>%
  
  filter(
    linkage_status == "eligible"
  )

# ==========================================================
# UNMATCHED INVENTORY
#
# Retained for transparency and QC.
# ==========================================================

source_inventory_unmatched <- source_inventory %>%
  
  filter(
    linkage_status != "eligible"
  )

# ==========================================================
# SAVE
# ==========================================================

write_csv(
  source_inventory_period_matched,
  file.path(
    output_dir,
    "source_inventory_period_matched.csv"
  )
)

write_csv(
  source_inventory_unmatched,
  file.path(
    output_dir,
    "source_inventory_unmatched.csv"
  )
)

# ==========================================================
# QC
# ==========================================================

cat(
  "\n=====================================\n"
)

cat(
  "PERIOD-MATCHED INVENTORY COMPLETE\n"
)

cat(
  "=====================================\n"
)

cat(
  "\nOriginal Source Inventory Rows:\n"
)

print(
  nrow(
    source_inventory
  )
)

cat(
  "\nPeriod-Matched Rows:\n"
)

print(
  nrow(
    source_inventory_period_matched
  )
)

cat(
  "\nExcluded Rows:\n"
)

print(
  nrow(
    source_inventory_unmatched
  )
)

# ----------------------------------------------------------
# EXCLUSION REASONS
# ----------------------------------------------------------

cat(
  "\nExclusion Reasons:\n"
)

print(
  table(
    source_inventory$linkage_status,
    useNA = "ifany"
  )
)

# ----------------------------------------------------------
# ARCHIVE STATUS
# ----------------------------------------------------------

cat(
  "\nArchive Status (Matched Inventory):\n"
)

print(
  table(
    source_inventory_period_matched$archive_status,
    useNA = "ifany"
  )
)

# ----------------------------------------------------------
# RECORD CLASSES
# ----------------------------------------------------------

cat(
  "\nRecord Classes (Matched Inventory):\n"
)

print(
  table(
    source_inventory_period_matched$record_class,
    useNA = "ifany"
  )
)

# ----------------------------------------------------------
# TEMPORAL COVERAGE
# ----------------------------------------------------------

cat(
  "\nMatched Year Range:\n"
)

print(
  
  source_inventory_period_matched %>%
    
    summarise(
      
      min_year =
        min(
          collection_year,
          na.rm = TRUE
        ),
      
      max_year =
        max(
          collection_year,
          na.rm = TRUE
        )
      
    )
  
)

# ----------------------------------------------------------
# SOURCE COUNTS
# ----------------------------------------------------------

cat(
  "\nMatched Source Records:\n"
)

print(
  nrow(
    source_inventory_period_matched
  )
)

cat(
  "\nUnique Source Keys:\n"
)

print(
  n_distinct(
    source_inventory_period_matched$source_key
  )
)

# ----------------------------------------------------------
# OUTPUTS
# ----------------------------------------------------------

cat(
  "\nSaved:\n"
)

cat(
  file.path(
    output_dir,
    "source_inventory_period_matched.csv"
  ),
  "\n"
)

cat(
  file.path(
    output_dir,
    "source_inventory_unmatched.csv"
  ),
  "\n"
)