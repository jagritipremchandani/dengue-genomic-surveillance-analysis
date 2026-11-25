# ==========================================================
# 09_source_inventory_generation.R
#
# PURPOSE
#
# Construct source-level surveillance inventory from
# classified ENA assembled-sequence records and
# NCBI Virus records.
#
# SOURCE INVENTORY UNIT
#
#     source_key
#   + serotype
#   + record_class
#
# This inventory approximates biologically distinguishable
# source observations while avoiding unsupported infection-
# level inference.
#
# This inventory DOES NOT represent:
#
#   unique infections
#   unique patients
#   incidence
#   transmission events
#
# Raw-read-only records are retained separately within the
# Archive Inventory and are intentionally excluded from the
# Source Inventory.
#
# INPUTS
#
#   ena_seq_classified.csv
#   ncbi_virus_classified.csv
#   cross_archive_overlap_inventory.csv
#
# OUTPUT
#
#   source_inventory.csv
#
# ==========================================================

library(dplyr)
library(readr)

# ==========================================================
# PATHS
# ==========================================================

classified_dir <- "data/intermediate/classified_ready"

inventory_dir <- "data/intermediate/inventories"

overlap_file <- file.path(
  inventory_dir,
  "cross_archive_overlap_inventory.csv"
)

output_file <- file.path(
  inventory_dir,
  "source_inventory.csv"
)

# ==========================================================
# LOAD
# ==========================================================

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

overlap <- read_csv(
  overlap_file,
  show_col_types = FALSE
)

# ==========================================================
# SOURCE KEY
# ==========================================================

# ----------------------------------------------------------
# ENA SOURCE IDENTIFIER
#
# Priority hierarchy:
#
# sample_accession
#       ↓
# isolate
#       ↓
# strain
#       ↓
# accession
#
# The highest-available source-level metadata is used as
# the source identifier.
#
# source_key should be interpreted as a source proxy rather
# than a patient identifier or infection identifier.
# ----------------------------------------------------------

ena <- ena %>%
  mutate(
    
    source_archive = "ENA_SEQ",
    
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
      
    )
    
  )

# ----------------------------------------------------------
# NCBI SOURCE IDENTIFIER
#
# Priority hierarchy:
#
# biosample
#      ↓
# isolate_name
#      ↓
# accession
#
# Biosample identifiers take precedence because they most
# closely represent source-level metadata.
# ----------------------------------------------------------

ncbi <- ncbi %>%
  mutate(
    
    source_archive = "NCBI_VIRUS",
    
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
      
    )
    
  )

# ==========================================================
# OVERLAP STATUS
# ==========================================================

# ----------------------------------------------------------
# archive_status originates from the cross-archive overlap
# inventory and is retained as descriptive metadata.
#
# archive_status:
#
#   BOTH
#   ENA_ONLY
#   NCBI_ONLY
#
# It is NEVER used to automatically merge or remove records.
# ----------------------------------------------------------

archive_status_lookup <- overlap %>%
  
  select(
    source_key,
    archive_status
  ) %>%
  
  distinct()

# ==========================================================
# ENA SOURCE INVENTORY
# ==========================================================

ena_source <- ena %>%
  
  transmute(
    
    source_archive,
    
    source_key,
    
    serotype,
    
    taxon_level,
    
    record_class,
    
    collection_year,
    
    collection_month,
    
    country =
      country_standardized,
    
    location_type,
    
    host =
      host_standardized,
    
    host_group
    
  ) %>%
  
  left_join(
    archive_status_lookup,
    by = "source_key"
  )

# ==========================================================
# NCBI SOURCE INVENTORY
# ==========================================================

ncbi_source <- ncbi %>%
  
  transmute(
    
    source_archive,
    
    source_key,
    
    serotype,
    
    taxon_level,
    
    record_class,
    
    collection_year,
    
    collection_month,
    
    country =
      country_standardized,
    
    location_type,
    
    host =
      host_standardized,
    
    host_group
    
  ) %>%
  
  left_join(
    archive_status_lookup,
    by = "source_key"
  )
    

# ==========================================================
# COMBINE
# ==========================================================

source_inventory <- bind_rows(
  
  ena_source,
  ncbi_source
  
)

# ==========================================================
# SOURCE INVENTORY DEDUPLICATION
#
# One source inventory record is defined as:
#
#     source_key
#   + serotype
#   + record_class
#
# source_key alone is insufficient because a source may
# legitimately appear under multiple serotypes or record
# classes.
#
# Exact duplicate combinations are collapsed while
# preserving biologically meaningful distinctions.
# ==========================================================

source_inventory <- source_inventory %>%
  
  group_by(
    
    source_key,
    serotype,
    record_class
    
  ) %>%
  
  slice(1) %>%
  
  ungroup()

# ==========================================================
# SAVE
# ==========================================================

write_csv(
  source_inventory,
  output_file
)

# ==========================================================
# QC
# ==========================================================

cat(
  "\n=====================================\n"
)

cat(
  "SOURCE INVENTORY CREATED\n"
)

cat(
  "=====================================\n"
)

cat(
  "\nRows:",
  nrow(source_inventory),
  "\n"
)

cat(
  "\nUnique source keys:",
  n_distinct(
    source_inventory$source_key
  ),
  "\n"
)

cat(
  "\nArchive status:\n"
)

print(
  table(
    source_inventory$archive_status,
    useNA = "ifany"
  )
)

cat(
  "\nRecord classes:\n"
)

print(
  table(
    source_inventory$record_class,
    useNA = "ifany"
  )
)

cat(
  "\nSerotypes:\n"
)

print(
  table(
    source_inventory$serotype,
    useNA = "ifany"
  )
)

# ==========================================================
# DEDUPLICATION EFFECT
# ==========================================================

before_counts <- bind_rows(
  ena_source,
  ncbi_source
) %>%
  count(
    record_class,
    name = "before"
  )

after_counts <- source_inventory %>%
  count(
    record_class,
    name = "after"
  )

dedup_effect <- before_counts %>%
  left_join(
    after_counts,
    by = "record_class"
  ) %>%
  mutate(
    removed = before - after,
    pct_removed = round(
      100 * removed / before,
      1
    )
  )

cat(
  "\nDeduplication effect:\n"
)

print(
  dedup_effect
)

cat(
  "\nSaved:\n",
  output_file,
  "\n"
)