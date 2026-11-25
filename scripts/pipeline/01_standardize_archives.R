# ==================================================
# 01_standardize_archives.R
# Purpose:
# Standardize ENA sequence, ENA read-run, and NCBI
# Virus exports prior to country, host, date, and
# taxonomy harmonization.
# ==================================================
library(here)
library(tidyverse)
library(janitor)
library(lubridate)

# --------------------------------------------------
# Locate files
# --------------------------------------------------

raw_data_dir <- "data/raw"

# --------------------------------------------------
# Common archive cleaning function
# --------------------------------------------------

clean_archive_data <- function(
    df,
    geography_column,
    release_column,
    updated_column = NULL
) {
  
  out <- df %>%
    clean_names() %>%
    separate(
      all_of(geography_column),
      into = c("country", "sublocation"),
      sep = ":",
      extra = "merge",
      fill = "right",
      remove = FALSE
    ) %>%
    mutate(
      across(
        where(is.character) & !matches("accession"),
        ~ .x %>%
          str_trim() %>%
          str_to_lower() %>%
          str_replace_all("\\s+", "_")
      ),
      
      release_date_parsed =
        ymd(.data[[release_column]])
    )
  
  if (!is.null(updated_column)) {
    out <- out %>%
      mutate(
        last_updated_date =
          ymd(.data[[updated_column]])
      )
  }
  
  out
}

# --------------------------------------------------
# Read raw files
# --------------------------------------------------

ena_seq_raw <- read_tsv(
  file.path(
    raw_data_dir,
    "ena_seq_dengue_2026-11-17.tsv"
  ),
  show_col_types = FALSE
)

ena_readruns_raw <- read_tsv(
  file.path(
    raw_data_dir,
    "ena_sra_readruns_dengue_2026-11-17.tsv"
  ),
  show_col_types = FALSE
)

ncbi_virus_raw <- read_tsv(
  file.path(
    raw_data_dir,
    "ncbi_virus_dengue_2026-11-17.tsv"
  ),
  show_col_types = FALSE
)

# --------------------------------------------------
# Standardize ENA sequences
# --------------------------------------------------

ena_seq_std <- clean_archive_data(
  df = ena_seq_raw,
  geography_column = "country",
  release_column = "first_public",
  updated_column = "last_updated"
)

# --------------------------------------------------
# Standardize ENA read runs
# --------------------------------------------------

ena_readruns_std <- clean_archive_data(
  df = ena_readruns_raw,
  geography_column = "country",
  release_column = "first_public"
)

# --------------------------------------------------
# Standardize NCBI Virus
# --------------------------------------------------

ncbi_virus_std <- clean_archive_data(
  df = ncbi_virus_raw,
  geography_column = "geo_location",
  release_column = "release_date"
)

# --------------------------------------------------
# Output directory
# --------------------------------------------------

output_dir <- "data/intermediate/standardized"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# --------------------------------------------------
# Write outputs
# --------------------------------------------------

write_csv(
  ena_seq_std,
  file.path(
    output_dir,
    "ena_seq_standardized.csv"
  )
)

write_csv(
  ena_readruns_std,
  file.path(
    output_dir,
    "ena_readruns_standardized.csv"
  )
)

write_csv(
  ncbi_virus_std,
  file.path(
    output_dir,
    "ncbi_virus_standardized.csv"
  )
)

# --------------------------------------------------
# QC Summary
# --------------------------------------------------

cat("\nARCHIVE STANDARDIZATION COMPLETE\n")
cat("--------------------------------\n")
cat("ENA sequences    :", nrow(ena_seq_std), "\n")
cat("ENA read runs    :", nrow(ena_readruns_std), "\n")
cat("NCBI records     :", nrow(ncbi_virus_std), "\n")