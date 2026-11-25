# ==========================================================
# Figure 01 Dataset
# Global Sequencing Representation
#
# Purpose:
# Count source-level dengue observations by country
# across all available years.
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
# Add ISO3
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
# Exclude non-country locations
# ----------------------------------------------------------

global_sequencing_representation <- source_inventory %>%
  
  filter(
    !is.na(iso3)
  ) %>%
  
  filter(
    location_type %in%
      c(
        "COUNTRY",
        "TERRITORY",
        "SUBNATIONAL"
      )
  ) %>%
  
  count(
    iso3,
    country,
    name = "n_sources"
  ) %>%
  
  arrange(
    desc(n_sources)
  )

# ----------------------------------------------------------
# Save
# ----------------------------------------------------------

write_csv(
  global_sequencing_representation,
  file.path(
    output_dir,
    "global_sequencing_representation.csv"
  )
)

# ----------------------------------------------------------
# QC
# ----------------------------------------------------------

cat(
  "\nCountries represented:\n"
)

print(
  n_distinct(
    global_sequencing_representation$iso3
  )
)

cat(
  "\nTotal source observations:\n"
)

print(
  sum(
    global_sequencing_representation$n_sources
  )
)
