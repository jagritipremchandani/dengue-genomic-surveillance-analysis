# ==========================================================
# Table 03 – Geographic Classification Framework
#
# PURPOSE
#
# Summarize classification of geographic metadata values
# into standardized location categories.
#
# Categories:
#
#   COUNTRY
#   TERRITORY
#   SUBNATIONAL
#   REGION
#   OCEAN
#   MISSING
#
# INPUT
#
#   country_lookup_final.csv
#
# OUTPUT
#
#   table_03_geographic_classification.csv
#
# RESULTS SECTION
#
# Geographic metadata harmonization
#
# ==========================================================
# ==========================================================

library(tidyverse)
# ==========================================================
# PATHS
# ==========================================================

data_dir<-"data/intermediate/lookup_tables"
output_dir<-"outputs/tables"

# ==========================================================
# LOAD CLASSIFIED DATASETS
# ==========================================================

country_lookup_final <- read_csv(
  file.path(
    data_dir,
    "country_lookup_final.csv"
  ),
  show_col_types = FALSE
)


# ==========================================================
# GENERATE GEOGRAPHIC CLASSIFICATION TABLE
# ==========================================================
geographic_classification <- country_lookup_final %>%
  
  count(
    location_type,
    name = "n_values"
  ) %>%
  
  arrange(
    desc(n_values)
  )

geographic_classification

geographic_classification <- country_lookup_final %>%
  
  count(
    location_type,
    name = "n_values"
  ) %>%
  
  mutate(
    pct =
      round(
        100 *
          n_values /
          sum(n_values),
        1
      )
  )

geographic_classification


write_csv(
  geographic_classification,
  file.path(
    "outputs",
    "tables",
    "table_03_geographic_classification.csv"
  )
)

