# ==========================================================
# Supplementary Table S3
# Burden Source Coverage
# ==========================================================

library(tidyverse)

burden_dir <- "data/intermediate/burden"

output_dir <- "outputs/tables/supplementary"

burden_master <- read_csv(
  file.path(
    burden_dir,
    "burden_master.csv"
  ),
  show_col_types = FALSE
)

table_S3_burden_source_coverage <- burden_master %>%
  
  group_by(
    burden_source
  ) %>%
  
  summarise(
    
    countries =
      n_distinct(
        iso3
      ),
    
    years =
      n_distinct(
        burden_year
      ),
    
    rows =
      n(),

    .groups = "drop"

  )

write_csv(
  table_S3_burden_source_coverage,
  file.path(
    output_dir,
    "table_S3_burden_source_coverage.csv"
  )
)
