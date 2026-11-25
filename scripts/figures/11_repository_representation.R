# ==========================================================
# Figure 5 Dataset
# Repository Representation
# ==========================================================

library(tidyverse)

inventory_dir <- "data/intermediate/inventories"

output_dir <- "outputs/figure_csvs"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

source_archive_overlap <- read_csv(
  file.path(
    inventory_dir,
    "source_key_overlap_inventory.csv"
  ),
  show_col_types = FALSE
)

repository_representation <- source_archive_overlap %>%
  
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
  )

write_csv(
  repository_representation,
  file.path(
    output_dir,
    "repository_representation.csv"
  )
)

repository_representation