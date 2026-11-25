# ==========================================================
# 08_source_key_archive_overlap_inventory.R
#
# PURPOSE
#
# Evaluate overlap between ENA assembled-sequence records
# and NCBI Virus records at the source level.
#
# This script does NOT merge archives.
#
# It identifies potential source-level overlaps and
# evaluates agreement between archives for:
#
#   serotype
#   record_class
#   country
#   host
#
# OUTPUT
#
# source_key_archive_overlap_inventory.csv
#
# archive_status:
#
#   BOTH
#   ENA_ONLY
#   NCBI_ONLY
#
# safe_merge is retained as a diagnostic indicator only and
# is never used to automatically remove or collapse records.
#
# ==========================================================

library(dplyr)
library(readr)

# ==========================================================
# PATHS
# ==========================================================

classified_dir <- "data/intermediate/classified_ready"

inventory_dir <- "data/intermediate/inventories"


# ==========================================================
# LOAD CLASSIFIED DATA
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

# ==========================================================
# SOURCE KEY
# ==========================================================

# ----------------------------------------------------------
# ENA SOURCE IDENTIFIER
#
# sample_accession
#      ↓
# isolate
#      ↓
# strain
#      ↓
# accession
# ----------------------------------------------------------

ena <- ena %>%
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
      
    )
    
  )

# ----------------------------------------------------------
# NCBI SOURCE IDENTIFIER
#
# biosample
#      ↓
# isolate_name
#      ↓
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
      
    )
    
  )

# ==========================================================
# CREATE SOURCE SUMMARIES
# ==========================================================
ena_summary <- ena %>%
  
  group_by(
    source_key
  ) %>%
  
  summarise(
    
    serotype_ena =
      first(serotype),
    
    record_class_ena =
      first(record_class),
    
    country_ena =
      first(country_standardized),
    
    host_ena =
      first(host_standardized),
    
    .groups = "drop"
    
  )

ncbi_summary <- ncbi %>%
  
  group_by(
    source_key
  ) %>%
  
  summarise(
    
    serotype_ncbi =
      first(serotype),
    
    record_class_ncbi =
      first(record_class),
    
    country_ncbi =
      first(country_standardized),
    
    host_ncbi =
      first(host_standardized),
    
    .groups = "drop"
    
  )

cat(
  "\nUnique ENA source keys:\n"
)

print(
  nrow(ena_summary)
)

cat(
  "\nUnique NCBI source keys:\n"
)

print(
  nrow(ncbi_summary)
)

# ==========================================================
# SOURCE-KEY-ARCHIVE OVERLAP
# ==========================================================

overlap_inventory <- full_join(
  
  ena_summary,
  
  ncbi_summary,
  
  by = "source_key"
  
)

# ==========================================================
# ARCHIVE STATUS
# ==========================================================

overlap_inventory <- overlap_inventory %>%
  
  mutate(
    
    archive_status = case_when(
      
      !is.na(serotype_ena) &
        !is.na(serotype_ncbi)
      ~ "BOTH",
      
      !is.na(serotype_ena)
      ~ "ENA_ONLY",
      
      TRUE
      ~ "NCBI_ONLY"
      
    )
    
  )

# ==========================================================
# METADATA AGREEMENT
#
# For source keys present in both archives, metadata
# agreement is evaluated independently.
# ==========================================================

overlap_inventory <- overlap_inventory %>%
  
  mutate(
    
    same_serotype = case_when(
      
      archive_status != "BOTH"
      ~ NA,
      
      TRUE
      ~ serotype_ena ==
        serotype_ncbi
      
    ),
    
    same_record_class = case_when(
      
      archive_status != "BOTH"
      ~ NA,
      
      TRUE
      ~ record_class_ena ==
        record_class_ncbi
      
    ),
    
    same_country = case_when(
      
      archive_status != "BOTH"
      ~ NA,
      
      TRUE
      ~ country_ena ==
        country_ncbi
      
    ),
    
    same_host = case_when(
      
      archive_status != "BOTH"
      ~ NA,
      
      TRUE
      ~ host_ena ==
        host_ncbi
      
    )
    
  )

# ==========================================================
# SAFE MERGE FLAG
#
# Diagnostic only.
#
# Not used to merge records.
# ==========================================================

overlap_inventory <- overlap_inventory %>%
  
  mutate(
    
    safe_merge = case_when(
      
      archive_status != "BOTH"
      ~ FALSE,
      
      same_serotype &
        same_record_class &
        same_country &
        same_host
      ~ TRUE,
      
      TRUE
      ~ FALSE
      
    )
    
  )

# ==========================================================
# SAVE
# ==========================================================

write_csv(
  overlap_inventory,
  file.path(
    inventory_dir,
    "source_key_overlap_inventory.csv"
  )
)

# ==========================================================
# QC
# ==========================================================

cat(
  "\n=====================================\n"
)

cat(
  "SOURCE-KEY-ARCHIVE OVERLAP CREATED\n"
)

cat(
  "=====================================\n"
)

cat(
  "\nArchive status:\n"
)

print(
  table(
    overlap_inventory$archive_status,
    useNA = "ifany"
  )
)

cat(
  "\nSafe merge:\n"
)

print(
  table(
    overlap_inventory$safe_merge,
    useNA = "ifany"
  )
)

cat(
  "\nOverlap records:",
  nrow(overlap_inventory),
  "\n"
)

cat(
  "\nSaved:\n"
)

cat(
  file.path(
    inventory_dir,
    "source_key_archive_overlap_inventory.csv"
  ),
  "\n"
)