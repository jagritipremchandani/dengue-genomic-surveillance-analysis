# ==========================================================
# Table 05 – Taxonomy Distribution
# ==========================================================

library(tidyverse)

classified_dir <- "data/intermediate/classified_ready"

output_dir <- "outputs/tables"

dir.create(output_dir,
           recursive = TRUE,
           showWarnings = FALSE)

ena_seq <- read_csv(
  file.path(classified_dir,
            "ena_seq_classified.csv"),
  show_col_types = FALSE
)

ena_sra <- read_csv(
  file.path(classified_dir,
            "ena_sra_classified.csv"),
  show_col_types = FALSE
)

ncbi <- read_csv(
  file.path(classified_dir,
            "ncbi_classified.csv"),
  show_col_types = FALSE
)

table_05_taxonomy_distribution <- bind_rows(
  
  ena_seq %>%
    mutate(source = "ENA Sequence"),
  
  ena_sra %>%
    mutate(source = "ENA SRA"),
  
  ncbi %>%
    mutate(source = "NCBI Virus")
  
) %>%
  
  count(
    source,
    serotype,
    name = "records"
  ) %>%
  
  arrange(
    source,
    desc(records)
  )

write_csv(
  table_05_taxonomy_distribution,
  file.path(
    output_dir,
    "table_05_taxonomy_distribution.csv"
  )
)
