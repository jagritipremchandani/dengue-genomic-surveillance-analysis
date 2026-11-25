# ==========================================================
# Supplementary Table S5
# Record-Class Representation Among Shared Source Keys
#
# PURPOSE
#
# Compare record-class representation for source keys
# present in BOTH archives.
#
# This table illustrates how the same biological source
# may be represented differently across archives.
#
# ==========================================================

library(tidyverse)

inventory_dir <- "data/intermediate/inventories"

output_dir <- "outputs/tables/supplementary"

source_archive_overlap <- read_csv(
  file.path(
    inventory_dir,
    "source_key_overlap_inventory.csv"
  ),
  show_col_types = FALSE
)

table_S5_record_class_representation <-
  source_archive_overlap %>%
  
  filter(
    archive_status == "BOTH"
  ) %>%
  
  count(
    
    record_class_ena,
    record_class_ncbi,
    
    name = "source_units"
    
  ) %>%
  
  mutate(
    pct =
      round(
        100 *
          source_units /
          sum(source_units),
        2
      )
  ) %>%
  
  arrange(
    desc(source_units)
  )

write_csv(
  table_S5_record_class_representation,
  file.path(
    output_dir,
    "table_S5_record_class_representation.csv"
  )
)

table_S5_record_class_representation
