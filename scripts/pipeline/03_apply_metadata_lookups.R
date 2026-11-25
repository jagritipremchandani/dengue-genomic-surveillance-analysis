# ==========================================================
# 03_apply_metadata_lookups.R
# ==========================================================

library(tidyverse)

# ----------------------------------------------------------
# PATHS
# ----------------------------------------------------------

data_dir <- "data/intermediate/standardized"
lookup_dir<-"data/intermediate/lookup_tables"
lookup_applied_dir<-"data/intermediate/lookup_applied"

dir.create(
  lookup_applied_dir,
  recursive = TRUE,
  showWarnings = FALSE
)
# ----------------------------------------------------------
# LOAD DATASETS
# ----------------------------------------------------------

ena_seq_std <- read_csv(
  file.path(data_dir,
            "ena_seq_standardized.csv"),
  show_col_types = FALSE
)

ena_readruns_std <- read_csv(
  file.path(data_dir,
            "ena_readruns_standardized.csv"),
  show_col_types = FALSE
)

ncbi_virus_std <- read_csv(
  file.path(data_dir,
            "ncbi_virus_standardized.csv"),
  show_col_types = FALSE
)

country_lookup_final <- read_csv(
  file.path(
    lookup_dir,
    "country_lookup_final.csv"
  ),
  show_col_types = FALSE
)

host_lookup_final <- read_csv(
  file.path(
    lookup_dir,
    "host_lookup_final.csv"
  ),
  show_col_types = FALSE
)

# ----------------------------------------------------------
# QC CHECKS
# ----------------------------------------------------------

stopifnot(
  sum(
    duplicated(
      country_lookup_final$country_raw
    )
  ) == 0
)

stopifnot(
  sum(
    duplicated(
      host_lookup_final$host_raw
    )
  ) == 0
)

# ----------------------------------------------------------
# APPLY LOOKUPS
# ----------------------------------------------------------

ena_seq_lookup <- ena_seq_std %>%
  left_join(
    country_lookup_final,
    by = c(
      "country" = "country_raw"
    )
  ) %>%
  left_join(
    host_lookup_final,
    by = c(
      "host" = "host_raw"
    )
  )

ena_readruns_lookup <- ena_readruns_std %>%
  left_join(
    country_lookup_final,
    by = c(
      "country" = "country_raw"
    )
  ) %>%
  left_join(
    host_lookup_final,
    by = c(
      "host" = "host_raw"
    )
  )

ncbi_virus_lookup <- ncbi_virus_std %>%
  left_join(
    country_lookup_final,
    by = c(
      "country" = "country_raw"
    )
  ) %>%
  left_join(
    host_lookup_final,
    by = c(
      "host_name" = "host_raw"
    )
  )

# ----------------------------------------------------------
# COVERAGE AUDITS
# ----------------------------------------------------------

country_audit <- bind_rows(
  
  ena_seq_lookup %>%
    distinct(country) %>%
    mutate(dataset = "ena_seq_std"),
  
  ena_readruns_lookup %>%
    distinct(country) %>%
    mutate(dataset = "ena_readruns_std"),
  
  ncbi_virus_lookup %>%
    distinct(country) %>%
    mutate(dataset = "ncbi_std")
  
) %>%
  left_join(
    country_lookup_final,
    by = c(
      "country" = "country_raw"
    )
  ) %>%
  filter(is.na(location_type))

host_audit <- bind_rows(
  
  ena_seq_lookup %>%
    distinct(host) %>%
    rename(host_value = host) %>%
    mutate(dataset = "ena_seq"),
  
  ena_readruns_lookup %>%
    distinct(host) %>%
    rename(host_value = host) %>%
    mutate(dataset = "ena_readruns"),
  
  ncbi_virus_lookup %>%
    distinct(host_name) %>%
    rename(host_value = host_name) %>%
    mutate(dataset = "ncbi")
  
) %>%
  left_join(
    host_lookup_final,
    by = c(
      "host_value" = "host_raw"
    )
  ) %>%
  filter(is.na(host_group))

# ----------------------------------------------------------
# SAVE OUTPUTS
# ----------------------------------------------------------

write_csv(
  ena_seq_lookup,
  file.path(
    lookup_applied_dir,
    "ena_seq_lookup_applied.csv"
  )
)

write_csv(
  ena_readruns_lookup,
  file.path(
    lookup_applied_dir,
    "ena_readruns_lookup_applied.csv"
  )
)

write_csv(
  ncbi_virus_lookup,
  file.path(
    lookup_applied_dir,
    "ncbi_virus_lookup_applied.csv"
  )
)

write_csv(
  country_audit,
  file.path(
    data_dir,
    "country_lookup_audit.csv"
  )
)

write_csv(
  host_audit,
  file.path(
    data_dir,
    "host_lookup_audit.csv"
  )
)

# ----------------------------------------------------------
# QC SUMMARY
# ----------------------------------------------------------

cat("\n====================================\n")
cat("LOOKUP APPLICATION COMPLETE\n")
cat("====================================\n")

cat(
  "\nCountry audit rows:",
  nrow(country_audit),
  "\n"
)

cat(
  "Host audit rows:",
  nrow(host_audit),
  "\n"
)

cat(
  "\nENA Seq:",
  nrow(ena_seq_lookup),
  "\n"
)

cat(
  "ENA Read Runs:",
  nrow(ena_readruns_lookup),
  "\n"
)

cat(
  "NCBI:",
  nrow(ncbi_virus_lookup),
  "\n"
)