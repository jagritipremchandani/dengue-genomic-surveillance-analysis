# ==========================================================
# Supplementary Table S2
# Source Key Hierarchy Utilization
# ==========================================================

library(tidyverse)

classified_dir <- "data/intermediate/classified_ready"

output_dir <- "outputs/tables/supplementary"

ena <- read_csv(
  file.path(
    classified_dir,
    "ena_seq_classified.csv"
  ),
  show_col_types = FALSE
)

ncbi <- read_csv(
  file.path(
    classified_dir,
    "ncbi_classified.csv"
  ),
  show_col_types = FALSE
)

ena_table <- ena %>%
  
  mutate(
    
    identifier_type = case_when(
      
      !is.na(sample_accession) &
        sample_accession != ""
      ~ "biosample",
      
      !is.na(isolate) &
        isolate != ""
      ~ "isolate",
      
      !is.na(strain) &
        strain != ""
      ~ "strain",
      
      TRUE
      ~ "accession"
      
    )
    
  ) %>%
  
  count(
    identifier_type,
    name = "records"
  ) %>%
  
  mutate(
    archive = "ENA"
  )

ncbi_table <- ncbi %>%
  
  mutate(
    
    identifier_type = case_when(
      
      !is.na(biosample) &
        biosample != ""
      ~ "biosample",
      
      !is.na(isolate_name) &
        isolate_name != ""
      ~ "isolate",
      
      TRUE
      ~ "accession"
      
    )
    
  ) %>%
  
  count(
    identifier_type,
    name = "records"
  ) %>%
  
  mutate(
    archive = "NCBI"
  )

table_S2_source_key_hierarchy <- bind_rows(
  ena_table,
  ncbi_table
)

write_csv(
  table_S2_source_key_hierarchy,
  file.path(
    output_dir,
    "table_S2_source_key_hierarchy.csv"
  )
)
