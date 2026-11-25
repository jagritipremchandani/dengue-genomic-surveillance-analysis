# ==========================================================
# Table 07 – Sequence Redundancy Diagnostics
# ==========================================================

library(tidyverse)

classified_dir <- "data/intermediate/classified_ready"

output_dir <- "outputs/tables"

ena_seq <- read_csv(
  file.path(classified_dir,
            "ena_seq_classified.csv"),
  show_col_types = FALSE
)

table_07_sequence_redundancy <- tibble(
  
  metric = c(
    "ENA Accessions",
    "Unique Sequence MD5",
    "Unique Isolates"
  ),
  
  count = c(
    
    nrow(ena_seq),
    
    n_distinct(
      ena_seq$sequence_md5,
      na.rm = TRUE
    ),
    
    n_distinct(
      ena_seq$isolate,
      na.rm = TRUE
    )
    
  )
  
)

write_csv(
  table_07_sequence_redundancy,
  file.path(
    output_dir,
    "table_07_sequence_redundancy.csv"
  )
)
