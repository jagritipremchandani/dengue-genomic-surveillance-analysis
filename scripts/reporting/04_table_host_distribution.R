# ==========================================================
# Table 04 – Host Classification Distribution
#
# PURPOSE
#
# Summarize biological host composition across genomic
# archives after host harmonization.
#
# Categories:
#
#   Human
#   Vector
#   Animal
#   CellCulture
#   InvalidHost
#   Unknown
#
# INPUTS
#
#   lookup-applied datasets
#
# OUTPUT
#
#   table_04_host_distribution.csv
#
# RESULTS SECTION
#
# Biological composition of surveillance records
#
# ==========================================================
#==========================================================
  
  
  library(tidyverse)
# ==========================================================
# PATHS
# ==========================================================

data_dir<-"data/intermediate/lookup_applied"
output_dir<-"outputs/tables"


# ==========================================================
# LOAD CLASSIFIED DATASETS
# ==========================================================

ena_seq_lookup_applied <- read_csv(
  file.path(
    data_dir,
    "ena_seq_lookup_applied.csv"
  ),
  show_col_types = FALSE
)

ena_readruns_lookup_applied <- read_csv(
  file.path(
    data_dir,
    "ena_readruns_lookup_applied.csv"
  ),
  show_col_types = FALSE
)

ncbi_virus_lookup_applied <- read_csv(
  file.path(
    data_dir,
    "ncbi_virus_lookup_applied.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# GENERATE HOST DISTRIBUTION TABLE
# ==========================================================

host_distribution <- bind_rows(
  
  ena_seq_lookup_applied %>%
    
    count(
      host_group
    ) %>%
    
    mutate(
      archive = "ENA Sequence"
    ),
  
  ena_readruns_lookup_applied %>%
    
    count(
      host_group
    ) %>%
    
    mutate(
      archive = "ENA SRA"
    ),
  
  ncbi_virus_lookup_applied %>%
    
    count(
      host_group
    ) %>%
    
    mutate(
      archive = "NCBI Virus"
    )
  
) %>%
  
  pivot_wider(
    
    names_from = archive,
    
    values_from = n,
    
    values_fill = 0
    
  )

host_distribution



write_csv(
  host_distribution,
  file.path(
    "outputs",
    "tables",
    "table_04_host_distribution.csv"
  )
)
