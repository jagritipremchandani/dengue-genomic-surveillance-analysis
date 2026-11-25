# ==========================================================
# Figure A3.1 Dataset
# Temporal Surveillance Growth
# ==========================================================

library(tidyverse)

inventory_dir <- "data/intermediate/inventories"

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

temporal_surveillance_growth <- source_inventory %>%
  
  filter(
    !is.na(collection_year)
  ) %>%
  
  count(
    collection_year,
    name = "n_sources"
  ) %>%
  
  arrange(
    collection_year
  )

write_csv(
  temporal_surveillance_growth,
  file.path(
    output_dir,
    "temporal_surveillance_growth.csv"
  )
)

temporal_surveillance_growth
