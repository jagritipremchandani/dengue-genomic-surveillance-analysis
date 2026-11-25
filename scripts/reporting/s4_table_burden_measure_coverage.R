# ==========================================================
# Supplementary Table S4
# Burden Measure Coverage
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

table_S4_burden_measure_coverage <- burden_master %>%
  
  count(
    burden_source,
    measure,
    name = "records"
  )

write_csv(
  table_S4_burden_measure_coverage,
  file.path(
    output_dir,
    "table_S4_burden_measure_coverage.csv"
  )
)
