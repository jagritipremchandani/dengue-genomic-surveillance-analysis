# ==========================================================
# Table 01 – Metadata Completeness by Archive
#
# PURPOSE
#
# Summarize metadata completeness across genomic archives.
#
# This table reports availability of:
#
#   - country metadata
#   - host metadata
#   - collection date metadata
#
# after metadata harmonization and date parsing.
#
# INPUTS
#
#   ena_seq_date_parsed.csv
#   ena_readruns_date_parsed.csv
#   ncbi_virus_date_parsed.csv
#
# OUTPUT
#
#   table_01_metadata_completeness.csv
#
# RESULTS SECTION
#
# Dataset characteristics
# Metadata completeness
#
# ==========================================================


library(tidyverse)
# ==========================================================
# PATHS
# ==========================================================

data_dir<-"data/intermediate/dates_parsed"
output_dir<-"outputs/tables"


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==========================================================
# LOAD CLASSIFIED DATASETS
# ==========================================================

ena_seq_date_parsed <- read_csv(
  file.path(
    data_dir,
    "ena_seq_date_parsed.csv"
  ),
  show_col_types = FALSE
)

ena_readruns_date_parsed <- read_csv(
  file.path(
    data_dir,
    "ena_readruns_date_parsed.csv"
  ),
  show_col_types = FALSE
)

ncbi_virus_date_parsed <- read_csv(
  file.path(
    data_dir,
    "ncbi_virus_date_parsed.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# GENERATE METADATA COMPLETENESS TABLE
# ==========================================================



metadata_completeness <- bind_rows(
  
  ena_seq_date_parsed %>%
    summarise(
      archive = "ENA Sequence",
      records = n(),
      
      country_missing =
        sum(is.na(country_standardized)),
      
      host_missing =
        sum(is.na(host_group)),
      
      date_missing =
        sum(date_precision == "missing")
    ),
  
  ena_readruns_date_parsed %>%
    summarise(
      archive = "ENA SRA",
      records = n(),
      
      country_missing =
        sum(is.na(country_standardized)),
      
      host_missing =
        sum(is.na(host_group)),
      
      date_missing =
        sum(date_precision == "missing")
    ),
  
  ncbi_virus_date_parsed %>%
    summarise(
      archive = "NCBI Virus",
      records = n(),
      
      country_missing =
        sum(is.na(country_standardized)),
      
      host_missing =
        sum(is.na(host_group)),
      
      date_missing =
        sum(date_precision == "missing")
    )
  
) %>%
  
  mutate(
    
    country_completeness_pct =
      round(
        100 *
          (records - country_missing) /
          records,
        1
      ),
    
    host_completeness_pct =
      round(
        100 *
          (records - host_missing) /
          records,
        1
      ),
    
    date_completeness_pct =
      round(
        100 *
          (records - date_missing) /
          records,
        1
      )
    
  )

table_01_metadata_completeness <- metadata_completeness


write_csv(
  table_01_metadata_completeness,
  file.path(
    "outputs",
    "tables",
    "table_01_metadata_completeness.csv"
  )
)


