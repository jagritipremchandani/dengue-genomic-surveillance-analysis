# ==========================================================
# 05_taxonomy_lookup.R
#
# PURPOSE
#
# Generate a curated dengue taxonomy lookup table.
#
# Outputs:
#   taxid_lookup_final.csv
#
# Taxonomic assignment is based on TaxID rather than
# scientific_name because multiple scientific-name
# variants may refer to the same TaxID.
# ==========================================================

library(dplyr)
library(readr)

#=========================================================
# PATH
#=========================================================

input_folder <- "data/intermediate/dates_parsed"

#=========================================================
# READ FILES
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
# EXTRACT UNIQUE TAXIDS
#=========================================================

ena_seq_taxids <- ena_seq %>%
  select(
    taxid = tax_id,
    scientific_name
  ) %>%
  distinct() %>%
  mutate(
    ENA_SEQ = TRUE
  )

ena_sra_taxids <- ena_sra %>%
  select(
    taxid = tax_id,
    scientific_name
  ) %>%
  distinct() %>%
  mutate(
    ENA_SRA = TRUE
  )

ncbi_taxids <- ncbi %>%
  select(
    taxid,
    scientific_name = organism_name
  ) %>%
  distinct() %>%
  mutate(
    NCBI_VIRUS = TRUE
  )

#=========================================================
# COMBINE
#=========================================================

tax_lookup <- full_join(
  ena_seq_taxids,
  ena_sra_taxids,
  by = c(
    "taxid",
    "scientific_name"
  )
)

tax_lookup <- full_join(
  tax_lookup,
  ncbi_taxids,
  by = c(
    "taxid",
    "scientific_name"
  )
)

#=========================================================
# REPLACE NA FLAGS
#=========================================================

tax_lookup <- tax_lookup %>%
  mutate(
    ENA_SEQ = ifelse(
      is.na(ENA_SEQ),
      FALSE,
      TRUE
    ),
    
    ENA_SRA = ifelse(
      is.na(ENA_SRA),
      FALSE,
      TRUE
    ),
    
    NCBI_VIRUS = ifelse(
      is.na(NCBI_VIRUS),
      FALSE,
      TRUE
    )
  )

#=========================================================
# ADD CURATION COLUMNS
#=========================================================

tax_lookup <- tax_lookup %>%
  arrange(taxid) %>%
  mutate(
    serotype = NA_character_,
    taxon_level = NA_character_,
    accepted = "YES"
  )

#=========================================================
# SAVE
#=========================================================

write_csv(
  tax_lookup,
  file.path(
    input_folder,
    "taxid_lookup_draft.csv"
  )
)

cat("\n====================================\n")
cat("TAXONOMY LOOKUP SUMMARY\n")
cat("====================================\n")

cat(
  "\nUnique taxids:",
  nrow(tax_lookup),
  "\n"
)

multi_name_taxids <- bind_rows(
  ena_seq %>%
    transmute(
      taxid = tax_id,
      scientific_name
    ),
  ena_sra %>%
    transmute(
      taxid = tax_id,
      scientific_name
    ),
  ncbi %>%
    transmute(
      taxid,
      scientific_name = organism_name
    )
) %>%
  distinct() %>%
  count(taxid) %>%
  filter(n > 1)

cat(
  "\nTaxids with multiple scientific names:",
  nrow(multi_name_taxids),
  "\n"
)