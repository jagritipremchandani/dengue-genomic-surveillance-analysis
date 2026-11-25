# ==========================================================
# Supplementary Table S1
# Source-Key Archive Representation
#
# PURPOSE
#
# Summarize how final source-level observations are
# represented across ENA and NCBI.
#
# Archive status:
#
#   BOTH
#   ENA_ONLY
#   NCBI_ONLY
#
# ==========================================================

library(tidyverse)

inventory_dir <- "data/intermediate/inventories"

output_dir <- "outputs/tables/supplementary"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
source_archive_overlap <- read_csv(
  file.path(
    inventory_dir,
    "source_key_overlap_inventory.csv"
  ),
  show_col_types = FALSE
)

table_S1_source_key_archive_representation <-
  source_archive_overlap %>%
  
  distinct(
    source_key,
    archive_status
  ) %>%
  
  count(
    archive_status,
    name = "source_units"
  ) %>%
  
  mutate(
    pct =
      round(
        100 *
          source_units /
          sum(source_units),
        1
      )
  ) %>%
  
  arrange(
    desc(source_units)
  )

write_csv(
  table_S1_source_key_archive_representation,
  file.path(
    output_dir,
    "table_S1_source_key_archive_representation.csv"
  )
)

table_S1_source_key_archive_representation
