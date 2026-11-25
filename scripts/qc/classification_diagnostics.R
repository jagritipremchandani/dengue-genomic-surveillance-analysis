# ==========================================================
# classification_diagnostics.R
#
# PURPOSE
#
# Validation report supporting:
#
# - taxonomic assignment decisions
# - genome classification thresholds
# - metadata completeness assessment
# - inventory design decisions
#
# This script is NOT part of the production pipeline.
#
# It exists solely to document methodological decisions.
# ==========================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

#=========================================================
# PATHS
#=========================================================
input_folder <- "data/intermediate/dates_parsed"

output_folder <- file.path(
  input_folder,
  "audit_outputs"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

#=========================================================
# LOAD FILES
#=========================================================

ena_seq <- read_csv(
  file.path(
    input_folder,
    "ena_seq_date_parsed.csv"
  ),
  show_col_types = FALSE
)

ena_sra <- read_csv(
  file.path(
    input_folder,
    "ena_readruns_date_parsed.csv"
  ),
  show_col_types = FALSE
)

ncbi <- read_csv(
  file.path(
    input_folder,
    "ncbi_virus_date_parsed.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# PHASE 1
# TAXONOMY AUDIT
#=========================================================

tax_audit <- bind_rows(
  
  ena_seq %>%
    transmute(
      archive = "ENA_SEQ",
      taxid = tax_id,
      scientific_name
    ),
  
  ena_sra %>%
    transmute(
      archive = "ENA_SRA",
      taxid = tax_id,
      scientific_name
    ),
  
  ncbi %>%
    transmute(
      archive = "NCBI_VIRUS",
      taxid = taxid,
      scientific_name = organism_name
    )
  
) %>%
  
  filter(
    !is.na(taxid)
  ) %>%
  
  group_by(
    taxid,
    scientific_name
  ) %>%
  
  summarise(
    records = n(),
    archives = paste(
      sort(unique(archive)),
      collapse = ";"
    ),
    .groups = "drop"
  ) %>%
  
  arrange(
    taxid
  )

write_csv(
  tax_audit,
  file.path(
    output_folder,
    "taxid_taxonomy_audit.csv"
  )
)

#=========================================================
# PHASE 2
# SEQUENCE CLASSIFICATION AUDIT
#=========================================================

ena_length_audit <- ena_seq %>%
  
  mutate(
    
    length_bin = case_when(
      
      base_count < 1000 ~ "<1000",
      
      base_count < 5000 ~ "1000-4999",
      
      base_count < 9000 ~ "5000-8999",
      
      TRUE ~ "9000+"
      
    )
    
  ) %>%
  
  count(
    length_bin,
    sort = TRUE
  )

ncbi_completeness <- ncbi %>%
  count(
    completeness,
    sort = TRUE
  )

ncbi_segment <- ncbi %>%
  count(
    segment,
    sort = TRUE
  )

ncbi_length_bins <- ncbi %>%
  
  mutate(
    
    length_bin = case_when(
      
      length < 1000 ~ "<1000",
      
      length < 5000 ~ "1000-4999",
      
      length < 9000 ~ "5000-8999",
      
      TRUE ~ "9000+"
      
    )
    
  ) %>%
  
  count(
    length_bin,
    sort = TRUE
  )

write_csv(
  ena_length_audit,
  file.path(
    output_folder,
    "ena_sequence_length_audit.csv"
  )
)

write_csv(
  ncbi_completeness,
  file.path(
    output_folder,
    "ncbi_completeness_audit.csv"
  )
)

write_csv(
  ncbi_segment,
  file.path(
    output_folder,
    "ncbi_segment_audit.csv"
  )
)

write_csv(
  ncbi_length_bins,
  file.path(
    output_folder,
    "ncbi_length_bins_audit.csv"
  )
)

#=========================================================
# PHASE 3
# READ RUN AUDIT
#=========================================================

readrun_strategy <- ena_sra %>%
  count(
    library_strategy,
    sort = TRUE
  )

readrun_source <- ena_sra %>%
  count(
    library_source,
    sort = TRUE
  )

readrun_platform <- ena_sra %>%
  count(
    instrument_platform,
    sort = TRUE
  )

write_csv(
  readrun_strategy,
  file.path(
    output_folder,
    "readrun_strategy_audit.csv"
  )
)

write_csv(
  readrun_source,
  file.path(
    output_folder,
    "readrun_source_audit.csv"
  )
)

write_csv(
  readrun_platform,
  file.path(
    output_folder,
    "readrun_platform_audit.csv"
  )
)

#=========================================================
# PHASE 4
# DEDUP READINESS AUDIT
#=========================================================

dedup_audit <- tibble(
  
  dataset = c(
    "ENA_SEQ",
    "ENA_SRA",
    "NCBI_VIRUS"
  ),
  
  records = c(
    nrow(ena_seq),
    nrow(ena_sra),
    nrow(ncbi)
  ),
  
  biosample_available = c(
    sum(!is.na(ena_seq$sample_accession)),
    sum(!is.na(ena_sra$sample_accession)),
    sum(!is.na(ncbi$biosample))
  ),
  
  bioproject_available = c(
    sum(!is.na(ena_seq$study_accession)),
    sum(!is.na(ena_sra$study_accession)),
    sum(!is.na(ncbi$bioprojects))
  ),
  
  isolate_available = c(
    sum(!is.na(ena_seq$isolate)),
    sum(!is.na(ena_sra$isolate)),
    sum(!is.na(ncbi$isolate_name))
  ),
  
  strain_available = c(
    sum(!is.na(ena_seq$strain)),
    sum(!is.na(ena_sra$strain)),
    NA
  )
  
)

write_csv(
  dedup_audit,
  file.path(
    output_folder,
    "dedup_readiness_audit.csv"
  )
)

#=========================================================
# CONSOLE SUMMARY
#=========================================================

cat(
  "\n========================================\n"
)

cat(
  "DENGUE AUDIT COMPLETE\n"
)

cat(
  "========================================\n"
)

cat(
  "\nAudit files written to:\n",
  output_folder,
  "\n"
)