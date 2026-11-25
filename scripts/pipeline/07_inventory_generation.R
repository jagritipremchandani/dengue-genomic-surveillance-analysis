# ==========================================================
# 07_inventory_generation.R
#
# PURPOSE
#
# Construct archive- and sequence-level inventories and
# create source identifiers for downstream source inventory
# generation.
#
# INVENTORIES GENERATED
#
# 1. Archive Inventory
#    Unit:
#       accession
#       run_accession
#
# 2. Sequence Inventory
#    Unit:
#       sequence_md5
#
# This script DOES NOT generate the source inventory.
# Source inventory creation occurs separately in:
#
#     08_source_inventory_generation.R
#
# ==========================================================

library(tidyverse)

# ==========================================================
# PATHS
# ==========================================================

data_dir <- "data/intermediate/classified_ready"

inventory_dir <- "data/intermediate/inventories"

dir.create(
  inventory_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==========================================================
# LOAD CLASSIFIED DATASETS
# ==========================================================

ena_seq <- read_csv(
  file.path(
    data_dir,
    "ena_seq_classified.csv"
  ),
  show_col_types = FALSE
)

ena_readruns <- read_csv(
  file.path(
    data_dir,
    "ena_sra_classified.csv"
  ),
  show_col_types = FALSE
)

ncbi <- read_csv(
  file.path(
    data_dir,
    "ncbi_classified.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# CREATE SOURCE KEYS
# ==========================================================

# ----------------------------------------------------------
# ENA SEQUENCES
#
# sample_accession
#     ↓
# isolate
#     ↓
# strain
#     ↓
# accession
# ----------------------------------------------------------

ena_seq <- ena_seq %>%
  mutate(
    
    source_key = case_when(
      
      !is.na(sample_accession) &
        sample_accession != ""
      ~ paste0(
        "biosample:",
        sample_accession
      ),
      
      !is.na(isolate) &
        isolate != ""
      ~ paste0(
        "isolate:",
        isolate
      ),
      
      !is.na(strain) &
        strain != ""
      ~ paste0(
        "strain:",
        strain
      ),
      
      TRUE
      ~ paste0(
        "accession:",
        accession
      )
    ),
    
    source_archive = "ENA_SEQUENCE"
  )

# ----------------------------------------------------------
# ENA READ RUNS
#
# sample_accession
#     ↓
# isolate
#     ↓
# run_accession
# ----------------------------------------------------------

ena_readruns <- ena_readruns %>%
  mutate(
    
    source_key = case_when(
      
      !is.na(sample_accession) &
        sample_accession != ""
      ~ paste0(
        "biosample:",
        sample_accession
      ),
      
      !is.na(isolate) &
        isolate != ""
      ~ paste0(
        "isolate:",
        isolate
      ),
      
      TRUE
      ~ paste0(
        "run:",
        run_accession
      )
    ),
    
    source_archive = "ENA_SRA"
  )

# ----------------------------------------------------------
# NCBI VIRUS
#
# biosample
#     ↓
# isolate_name
#     ↓
# accession
# ----------------------------------------------------------

ncbi <- ncbi %>%
  mutate(
    
    source_key = case_when(
      
      !is.na(biosample) &
        biosample != ""
      ~ paste0(
        "biosample:",
        biosample
      ),
      
      !is.na(isolate_name) &
        isolate_name != ""
      ~ paste0(
        "isolate:",
        isolate_name
      ),
      
      TRUE
      ~ paste0(
        "accession:",
        accession
      )
    ),
    
    source_archive = "NCBI_VIRUS"
  )

# ==========================================================
# SAVE SOURCE-KEY READY DATASETS
# ==========================================================

write_csv(
  ena_seq,
  file.path(
    inventory_dir,
    "ena_seq_sourcekey.csv"
  )
)

write_csv(
  ena_readruns,
  file.path(
    inventory_dir,
    "ena_readruns_sourcekey.csv"
  )
)

write_csv(
  ncbi,
  file.path(
    inventory_dir,
    "ncbi_virus_sourcekey.csv"
  )
)

# ==========================================================
# ARCHIVE INVENTORY
#
# Question:
#   What archive records exist?
#
# Unit:
#   accession / run_accession
# ==========================================================

archive_inventory <- bind_rows(
  
  ena_seq %>%
    transmute(
      
      source_archive,
      
      record_id = accession,
      
      serotype,
      
      record_class,
      
      country = country_standardized,
      
      collection_year
    ),
  
  ena_readruns %>%
    transmute(
      
      source_archive,
      
      record_id = run_accession,
      
      serotype,
      
      record_class,
      
      country = country_standardized,
      
      collection_year
    ),
  
  ncbi %>%
    transmute(
      
      source_archive,
      
      record_id = accession,
      
      serotype,
      
      record_class,
      
      country = country_standardized,
      
      collection_year
    )
)

write_csv(
  archive_inventory,
  file.path(
    inventory_dir,
    "archive_inventory.csv"
  )
)

# ==========================================================
# SEQUENCE INVENTORY
#
# Question:
#   What unique nucleotide sequences exist?
#
# Unit:
#   sequence_md5
#
# ENA only.
# ==========================================================

sequence_inventory <- ena_seq %>%
  
  filter(
    !is.na(sequence_md5),
    sequence_md5 != ""
  ) %>%
  
  group_by(
    sequence_md5,
    serotype,
    record_class
  ) %>%
  
  slice(1) %>%
  
  ungroup()

write_csv(
  sequence_inventory,
  file.path(
    inventory_dir,
    "sequence_inventory.csv"
  )
)

# ==========================================================
# QC
# ==========================================================

cat(
  "\n==================================================\n"
)

cat(
  "INVENTORY GENERATION COMPLETE\n"
)

cat(
  "==================================================\n"
)

cat(
  "\nARCHIVE INVENTORY\n"
)

cat(
  "Records:",
  nrow(archive_inventory),
  "\n"
)

cat(
  "\nSEQUENCE INVENTORY\n"
)

cat(
  "Unique sequences:",
  nrow(sequence_inventory),
  "\n"
)

cat(
  "\nSOURCE KEY COVERAGE\n"
)

cat(
  "\nENA Sequence source keys:",
  n_distinct(ena_seq$source_key),
  "\n"
)

cat(
  "ENA Read-run source keys:",
  n_distinct(ena_readruns$source_key),
  "\n"
)

cat(
  "NCBI source keys:",
  n_distinct(ncbi$source_key),
  "\n"
)

cat(
  "\nArchive inventory written.\n"
)

cat(
  "Sequence inventory written.\n"
)

cat(
  "Source-key datasets written for source inventory generation.\n"
)