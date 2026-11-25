# ==========================================================
# Table 9 – Linkage Coverage
# ==========================================================

library(tidyverse)

burden_dir <- "data/intermediate/burden"

output_dir <- "outputs/tables"

pathogen_country_year <- read_csv(
  file.path(
    burden_dir,
    "pathogen_country_year.csv"
  ),
  show_col_types = FALSE
)

table_9_linkage_coverage <- tibble(
  
  metric = c(
    "Linked rows",
    "Unlinked rows",
    "Total rows",
    "Linkage success (%)"
  ),
  
  value = c(
    
    sum(
      !is.na(
        pathogen_country_year$measure
      )
    ),
    
    sum(
      is.na(
        pathogen_country_year$measure
      )
    ),
    
    nrow(
      pathogen_country_year
    ),
    
    round(
      100 *
        sum(
          !is.na(
            pathogen_country_year$measure
          )
        ) /
        nrow(
          pathogen_country_year
        ),
      1
    )
    
  )
  
)

write_csv(
  table_9_linkage_coverage,
  file.path(
    output_dir,
    "table_09_linkage_coverage.csv"
  )
)
