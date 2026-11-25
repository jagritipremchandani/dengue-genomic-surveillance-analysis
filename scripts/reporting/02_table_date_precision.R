# ==========================================================
# Table 02 – Collection Date Precision Distribution
#
# PURPOSE
#
# Summarize temporal precision of collection dates across
# genomic archives.
#
# Precision categories:
#
#   day
#   month
#   year
#   month_range
#   year_range
#   year_extracted
#   missing
#
# INPUTS
#
#   ena_seq_date_parsed.csv
#   ena_readruns_date_parsed.csv
#   ncbi_virus_date_parsed.csv
#
# OUTPUT
#
#   table_02_date_precision.csv
#
# RESULTS SECTION
#
# Temporal metadata quality
#
# ==========================================================

library(tidyverse)
# ==========================================================
# PATHS
# ==========================================================

data_dir<-"data/intermediate/dates_parsed"
output_dir<-"outputs/tables"

# ==========================================================
# LOAD CLASSIFIED DATASETS
# ==========================================================

ena_seq <- read_csv(
  file.path(
    data_dir,
    "ena_seq_date_parsed.csv"
  ),
  show_col_types = FALSE
)

ena_readruns <- read_csv(
  file.path(
    data_dir,
    "ena_readruns_date_parsed.csv"
  ),
  show_col_types = FALSE
)

ncbi <- read_csv(
  file.path(
    data_dir,
    "ncbi_virus_date_parsed.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# GENERATE DATE PRECISION TABLE
# ==========================================================
date_precision_table <- bind_rows(
  
  ena_seq_date_parsed %>%
    count(date_precision) %>%
    mutate(
      archive = "ENA Sequence"
    ),
  
  ena_readruns_date_parsed %>%
    count(date_precision) %>%
    mutate(
      archive = "ENA SRA"
    ),
  
  ncbi_virus_date_parsed %>%
    count(date_precision) %>%
    mutate(
      archive = "NCBI Virus"
    )
  
) %>%
  
  pivot_wider(
    names_from = archive,
    values_from = n,
    values_fill = 0
  )

write_csv(
  date_precision_table,
  file.path(
    "outputs",
    "tables",
    "table_02_date_precision.csv"
  )
)
