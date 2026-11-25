# ==========================================================
# Figure A2 Dataset
# Surveillance Persistence
#
# Question:
# Which countries maintain dengue genomic surveillance
# through time?
#
# Metric:
# Number of calendar years represented in the
# Source Inventory.
#
# OUTPUT
#
# surveillance_persistence.csv
#
# ==========================================================

library(tidyverse)

inventory_dir <- "data/intermediate/inventories"

lookup_dir <- "data/intermediate/lookup_tables"

output_dir <- "outputs/figure_csvs"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ----------------------------------------------------------
# LOAD
# ----------------------------------------------------------

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

# ----------------------------------------------------------
# ISO ENRICHMENT
# ----------------------------------------------------------

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

# ----------------------------------------------------------
# KEEP GEOLOCATABLE RECORDS
# ----------------------------------------------------------

source_inventory <- source_inventory %>%
  
  filter(
    !is.na(iso3)
  ) %>%
  
  filter(
    !is.na(collection_year)
  )

# ----------------------------------------------------------
# SURVEILLANCE PERSISTENCE
# ----------------------------------------------------------

surveillance_persistence <- source_inventory %>%
  
  group_by(
    iso3,
    country
  ) %>%
  
  summarise(
    
    years_represented =
      n_distinct(
        collection_year
      ),
    
    first_year =
      min(
        collection_year,
        na.rm = TRUE
      ),
    
    last_year =
      max(
        collection_year,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    desc(years_represented)
  )

# ----------------------------------------------------------
# SAVE
# ----------------------------------------------------------

write_csv(
  surveillance_persistence,
  file.path(
    output_dir,
    "surveillance_persistence.csv"
  )
)

# ----------------------------------------------------------
# QC
# ----------------------------------------------------------

cat(
  "\nCountries represented:\n"
)

print(
  nrow(
    surveillance_persistence
  )
)

cat(
  "\nPersistence summary:\n"
)

print(
  summary(
    surveillance_persistence$years_represented
  )
)

cat(
  "\nTop 20 countries:\n"
)

print(
  surveillance_persistence %>%
    slice_max(
      years_represented,
      n = 20
    )
)
