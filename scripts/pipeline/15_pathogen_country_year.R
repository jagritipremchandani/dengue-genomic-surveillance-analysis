# ==========================================================
# 15_pathogen_country_year.R
#
# Create first pathogen-country-year linkage table.
#
# ==========================================================

library(tidyverse)

country_dir <- "data/processed"


burden_country_year <- read_csv(
  file.path(
    country_dir,
    "burden_country_year.csv"
  ),
  show_col_types = FALSE
)

genomic_country_year <- read_csv(
  file.path(
    country_dir,
    "genomic_country_year.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# LINKAGE
#
# Match sequence collection year to burden year.
# ==========================================================

pathogen_country_year <- genomic_country_year %>%
  
  left_join(
    
    burden_country_year,
    
    by = c(
      "iso3" = "iso3",
      "collection_year" = "burden_year"
    )
    
  )

write_csv(
  
  pathogen_country_year,
  
  file.path(
    burden_dir,
    "pathogen_country_year.csv"
  )
  
)

# ==========================================================
# QC
# ==========================================================

# ==========================================================
# LINKAGE QC
# ==========================================================

cat(
  "\n=====================================\n"
)

cat(
  "PATHOGEN-COUNTRY-YEAR LINKAGE QC\n"
)

cat(
  "=====================================\n"
)

cat(
  "\nTotal linked table rows:\n"
)

print(
  nrow(
    pathogen_country_year
  )
)

# ----------------------------------------------------------
# LINKAGE SUCCESS
# ----------------------------------------------------------

linked_rows <- pathogen_country_year %>%
  filter(
    !is.na(measure)
  )

unlinked_rows <- pathogen_country_year %>%
  filter(
    is.na(measure)
  )

cat(
  "\nRows with burden match:\n"
)

print(
  nrow(
    linked_rows
  )
)

cat(
  "\nRows without burden match:\n"
)

print(
  nrow(
    unlinked_rows
  )
)

# ----------------------------------------------------------
# UNLINKED SOURCES
# ----------------------------------------------------------

cat(
  "\nUnlinked source observations:\n"
)

print(
  unlinked_rows %>%
    summarise(
      n_sources = sum(
        n_sources,
        na.rm = TRUE
      )
    )
)

# ----------------------------------------------------------
# UNLINKED ISO3
# ----------------------------------------------------------

cat(
  "\nISO3 codes without burden match:\n"
)

print(
  
  unlinked_rows %>%
    
    distinct(
      iso3
    ) %>%
    
    arrange(
      iso3
    )
  
)

# ----------------------------------------------------------
# UNLINKED YEARS
# ----------------------------------------------------------

cat(
  "\nYears without burden match:\n"
)

print(
  
  unlinked_rows %>%
    
    distinct(
      collection_year
    ) %>%
    
    arrange(
      collection_year
    )
  
)

# ----------------------------------------------------------
# COUNTRY-YEAR COMBINATIONS
# ----------------------------------------------------------

cat(
  "\nUnlinked country-year combinations:\n"
)

print(
  
  unlinked_rows %>%
    
    distinct(
      iso3,
      collection_year
    ) %>%
    
    arrange(
      iso3,
      collection_year
    )
  
)

# ----------------------------------------------------------
# COVERAGE SUMMARY
# ----------------------------------------------------------

coverage_summary <- pathogen_country_year %>%
  
  mutate(
    linked = !is.na(measure)
  ) %>%
  
  summarise(
    
    country_years =
      n_distinct(
        paste(
          iso3,
          collection_year
        )
      ),
    
    linked_country_years =
      n_distinct(
        paste(
          iso3[linked],
          collection_year[linked]
        )
      )
    
  )

cat(
  "\nCoverage summary:\n"
)

print(
  coverage_summary
)