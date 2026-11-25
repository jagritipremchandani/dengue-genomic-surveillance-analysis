# ==========================================================
# Figure A3.2 Dataset
# Surveillance Growth by Serotype
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

serotype_temporal_growth <- source_inventory %>%
  
  filter(
    !is.na(collection_year)
  ) %>%
  
  filter(
    !is.na(serotype)
  ) %>%
  
  count(
    
    collection_year,
    
    serotype,
    
    name = "n_sources"
    
  ) %>%
  
  arrange(
    collection_year
  )

write_csv(
  serotype_temporal_growth,
  file.path(
    output_dir,
    "serotype_temporal_growth.csv"
  )
)

serotype_temporal_growth
