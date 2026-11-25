# ==========================================================
# 14_genomic_country_year.R
#
# Create canonical genomic country-year table.
#
# ==========================================================

library(tidyverse)

inventory_dir <- "data/processed"

lookup_dir <- "data/intermediate/lookup_tables"

source_inventory <- read_csv(
  file.path(
    inventory_dir,
    "source_inventory.csv"
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
# ISO ENRICHMENT
# ==========================================================

source_inventory <- source_inventory %>%
  
  left_join(
    
    country_iso_lookup %>%
      select(
        country_name,
        iso3
      ),
    
    by = c(
      "country" = "country_name"
    )
    
  )

# ==========================================================
# COUNTRY-YEAR SUMMARY
# ==========================================================

genomic_country_year <- source_inventory %>%
  
  group_by(
    
    iso3,
    
    country,
    
    collection_year,
    
    serotype,
    
    record_class,
    
    archive_status,
    
    location_type
    
  ) %>%
  
  summarise(
    
    n_sources = n(),
    
    .groups = "drop"
    
  )

write_csv(
  
  genomic_country_year,
  
  file.path(
    inventory_dir,
    "genomic_country_year.csv"
  )
  
)

# ==========================================================
# QC
# ==========================================================

cat("\nRows:\n")
print(nrow(genomic_country_year))

cat("\nMissing ISO3:\n")
print(sum(is.na(genomic_country_year$iso3)))

