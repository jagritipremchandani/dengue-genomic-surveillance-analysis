# ==========================================================
# 06_record_classification.R
#
# PURPOSE
#
# Apply curated dengue taxonomy annotations and classify
# records into biologically meaningful surveillance units.
#
# Output classes:
#
#   complete_genome
#   partial_sequence
#   genome_segment
#   raw_read_run
#
# This script prepares datasets for downstream
# deduplication and inventory construction.
# ==========================================================

# install.packages("here")
library(here)

input_folder  <- here("data", "intermediate", "dates_parsed")
output_folder <- here("data", "intermediate", "classified_ready")

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

#=========================================================
# TAXONOMY LOOKUP
#=========================================================

tax_lookup <- tibble(
  
  taxid = c(
    11053,
    11060,
    11069,
    11070,
    12637,
    31634,
    31635
  ),
  
  serotype = c(
    "DENV-1",
    "DENV-2",
    "DENV-3",
    "DENV-4",
    "DENV",
    "SPECIAL_DENV2",
    "SPECIAL_DENV2"
  ),
  
  taxon_level = c(
    "SEROTYPE",
    "SEROTYPE",
    "SEROTYPE",
    "SEROTYPE",
    "PARENT",
    "SPECIAL_STRAIN",
    "SPECIAL_STRAIN"
  )
)

#=========================================================
# DEDUP KEY CLEANER
#=========================================================

clean_key <- function(x){
  
  x <- tolower(as.character(x))
  
  x <- trimws(x)
  
  x <- str_replace_all(
    x,
    "[^a-z0-9]",
    ""
  )
  
  x[x %in% c(
    "",
    "na",
    "false",
    "unknown",
    "missing"
  )] <- NA
  
  x
}

#=========================================================
# ENA SEQUENCES
#=========================================================
# ----------------------------------------------------------
# ENA RECORD CLASSIFICATION
#
# Diagnostic analyses identified:
#
#   partial sequence cluster ~1.5 kb
#   complete genome cluster ~10.5 kb
#
# Thresholds:
#
#   >= 10000 bp  -> complete_genome
#   >= 1000 bp   -> partial_sequence
#   < 1000 bp    -> genome_segment
#
# These thresholds were selected after inspection of
# archive sequence-length distributions.
# ----------------------------------------------------------
ena <- read_csv(
  file.path(
    input_folder,
    "ena_seq_date_parsed.csv"
  ),
  show_col_types = FALSE
)

ena <- ena %>%
  
  left_join(
    tax_lookup,
    by = c(
      "tax_id" = "taxid"
    )
  ) %>%
  
  mutate(
    
    record_class = case_when(
      
      base_count >= 10000
      ~ "complete_genome",
      
      base_count >= 1000
      ~ "partial_sequence",
      
      TRUE
      ~ "genome_segment"
    ),
    
    dedup_isolate =
      clean_key(isolate),
    
    dedup_strain =
      clean_key(strain),
    
    dedup_biosample =
      clean_key(sample_accession),
    
    dedup_bioproject =
      clean_key(study_accession),
    
    dedup_primary_key = case_when(
      
      !is.na(dedup_biosample)
      ~ paste0(
        "biosample:",
        dedup_biosample
      ),
      
      !is.na(dedup_isolate)
      ~ paste0(
        "isolate:",
        dedup_isolate
      ),
      
      !is.na(dedup_strain)
      ~ paste0(
        "strain:",
        dedup_strain
      ),
      
      TRUE ~ NA_character_
    )
    
  )

write_csv(
  ena,
  file.path(
    output_folder,
    "ena_seq_classified.csv"
  )
)

#=========================================================
# ENA READ RUNS
#=========================================================
# ----------------------------------------------------------
# ENA SRA READ-RUN CLASSIFICATION
#
# ENA SRA records represent raw sequencing runs rather than
# assembled nucleotide sequences.
#
# These records are therefore classified separately from
# complete genomes, partial sequences, and genome segments.
#
# No sequence-length-based classification is applied.
#
# All ENA SRA records receive:
#
#   record_class = "raw_read_run"
#
# This preserves the distinction between:
#
#   assembled sequences
#   raw sequencing data
#
# and prevents raw-read records from being interpreted as
# assembled genomic sequences.
# ----------------------------------------------------------
runs <- read_csv(
  file.path(
    input_folder,
    "ena_readruns_date_parsed.csv"
  ),
  show_col_types = FALSE
)

runs <- runs %>%
  
  left_join(
    tax_lookup,
    by = c(
      "tax_id" = "taxid"
    )
  ) %>%
  
  mutate(
    
    record_class = "raw_read_run",
    
    dedup_isolate =
      clean_key(isolate),
    
    dedup_strain =
      clean_key(strain),
    
    dedup_biosample =
      clean_key(sample_accession),
    
    dedup_bioproject =
      clean_key(study_accession),
    
    dedup_primary_key = case_when(
      
      !is.na(dedup_biosample)
      ~ paste0(
        "biosample:",
        dedup_biosample
      ),
      
      !is.na(dedup_isolate)
      ~ paste0(
        "isolate:",
        dedup_isolate
      ),
      
      TRUE ~ NA_character_
    )
    
  )

write_csv(
  runs,
  file.path(
    output_folder,
    "ena_sra_classified.csv"
  )
)

#=========================================================
# NCBI
#=========================================================
# ----------------------------------------------------------
# NCBI RECORD CLASSIFICATION
#
# Segment metadata takes precedence over completeness
# metadata.
#
# Segment records are classified as genome_segment even
# when completeness information is present.
# ----------------------------------------------------------
ncbi <- read_csv(
  file.path(
    input_folder,
    "ncbi_virus_date_parsed.csv"
  ),
  show_col_types = FALSE
)

ncbi <- ncbi %>%
  
  left_join(
    tax_lookup,
    by = "taxid"
  ) %>%
  
  mutate(
    
    isolate_name = ifelse(
      isolate_name == "false",
      NA,
      isolate_name
    ),
    
    record_class = case_when(
      
      !is.na(segment)
      &
        segment != ""
      &
        segment != "whole_genome"
      ~ "genome_segment",
      
      completeness == "complete"
      ~ "complete_genome",
      
      completeness == "partial"
      ~ "partial_sequence",
      
      TRUE
      ~ "unknown"
    ),
    
    dedup_isolate =
      clean_key(isolate_name),
    
    dedup_biosample =
      clean_key(biosample),
    
    dedup_bioproject =
      clean_key(bioprojects),
    
    dedup_primary_key = case_when(
      
      !is.na(dedup_biosample)
      ~ paste0(
        "biosample:",
        dedup_biosample
      ),
      
      !is.na(dedup_isolate)
      ~ paste0(
        "isolate:",
        dedup_isolate
      ),
      
      TRUE ~ NA_character_
    )
    
  )

write_csv(
  ncbi,
  file.path(
    output_folder,
    "ncbi_classified.csv"
  )
)

cat("\n====================================\n")
cat("CLASSIFICATION SUMMARY\n")
cat("====================================\n")

cat("\nENA SEQ\n")
print(table(ena$record_class))

cat("\nENA READ RUNS\n")
print(table(runs$record_class))

cat("\nNCBI VIRUS\n")
print(table(ncbi$record_class))

cat("\nSEROTYPE DISTRIBUTION\n")

print(
  sort(
    table(
      c(
        ena$serotype,
        runs$serotype,
        ncbi$serotype
      )
    ),
    decreasing = TRUE
  )
)
