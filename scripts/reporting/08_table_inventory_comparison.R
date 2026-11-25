# ==========================================================
# Table 8 – Inventory Comparison
# ==========================================================

library(tidyverse)

inventory_dir <- "data/intermediate/inventories"

output_dir <- "outputs/tables"

archive_inventory <- read_csv(
  file.path(
    inventory_dir,
    "archive_inventory.csv"
  ),
  show_col_types = FALSE
)

sequence_inventory <- read_csv(
  file.path(
    inventory_dir,
    "sequence_inventory.csv"
  ),
  show_col_types = FALSE
)

source_inventory <- read_csv(
  file.path(
    inventory_dir,
    "source_inventory.csv"
  ),
  show_col_types = FALSE
)

table_8_inventory_comparison <- tibble(
  
  inventory = c(
    "Archive Inventory",
    "Sequence Inventory",
    "Source Inventory"
  ),
  
  counting_unit = c(
    "accession/run_accession",
    "sequence_md5",
    "source_key + serotype + record_class"
  ),
  
  count = c(
    nrow(archive_inventory),
    nrow(sequence_inventory),
    nrow(source_inventory)
  )
  
)

write_csv(
  table_8_inventory_comparison,
  file.path(
    output_dir,
    "table_08_inventory_comparison.csv"
  )
)
