# ==========================================================
# 04_date_parsing.R
#
# Inputs:
#   ena_seq_lookup_applied.csv
#   ena_readruns_lookup_applied.csv
#   ncbi_virus_lookup_applied.csv
#
# Outputs:
#   ena_seq_date_parsed.csv
#   ena_readruns_date_parsed.csv
#   ncbi_virus_date_parsed.csv
# ==========================================================

library(tidyverse)
library(lubridate)
library(stringr)

# ==========================================================
# PATHS
# ==========================================================

data_dir <- "data/intermediate/lookup_applied"
date_dir<-"data/intermediate/dates_parsed"


dir.create(
  date_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==========================================================
# LOAD DATA
# ==========================================================

ena_seq_lookup <- read_csv(
  file.path(
    data_dir,
    "ena_seq_lookup_applied.csv"
  ),
  show_col_types = FALSE
)

ena_readruns_lookup <- read_csv(
  file.path(
    data_dir,
    "ena_readruns_lookup_applied.csv"
  ),
  show_col_types = FALSE
)

ncbi_virus_lookup <- read_csv(
  file.path(
    data_dir,
    "ncbi_virus_lookup_applied.csv"
  ),
  show_col_types = FALSE
)

# ==========================================================
# MONTH LOOKUP
# ==========================================================

month_lookup <- c(
  jan = 1,
  feb = 2,
  mar = 3,
  apr = 4,
  may = 5,
  jun = 6,
  jul = 7,
  aug = 8,
  sep = 9,
  oct = 10,
  nov = 11,
  dec = 12
)

# ==========================================================
# DATE PARSER
# ==========================================================

parse_collection_dates <- function(df){
  
  raw_dates <- trimws(df$collection_date)
  
  n <- length(raw_dates)
  
  collection_year <- rep(NA_integer_, n)
  collection_month <- rep(NA_integer_, n)
  collection_day <- rep(NA_integer_, n)
  
  date_precision <- rep(NA_character_, n)
  
  # --------------------------------------------------------
  # MISSING
  # --------------------------------------------------------
  
  missing_idx <- is.na(raw_dates) |
    raw_dates == ""
  
  date_precision[missing_idx] <- "missing"
  
  # --------------------------------------------------------
  # YYYY-MM-DD
  # --------------------------------------------------------
  
  day_idx <- grepl(
    "^\\d{4}-\\d{2}-\\d{2}$",
    raw_dates
  )
  
  if(any(day_idx)){
    
    parsed <- ymd(raw_dates[day_idx])
    
    collection_year[day_idx] <- year(parsed)
    collection_month[day_idx] <- month(parsed)
    collection_day[day_idx] <- day(parsed)
    
    date_precision[day_idx] <- "day"
  }
  
  # --------------------------------------------------------
  # YYYY-MM
  # --------------------------------------------------------
  
  month_idx <- grepl(
    "^\\d{4}-\\d{2}$",
    raw_dates
  )
  
  if(any(month_idx)){
    
    parts <- str_split_fixed(
      raw_dates[month_idx],
      "-",
      2
    )
    
    collection_year[month_idx] <- as.integer(parts[,1])
    collection_month[month_idx] <- as.integer(parts[,2])
    
    date_precision[month_idx] <- "month"
  }
  
  # --------------------------------------------------------
  # YYYY
  # --------------------------------------------------------
  
  year_idx <- grepl(
    "^\\d{4}$",
    raw_dates
  )
  
  if(any(year_idx)){
    
    collection_year[year_idx] <-
      as.integer(
        raw_dates[year_idx]
      )
    
    date_precision[year_idx] <- "year"
  }
  
  # --------------------------------------------------------
  # MON-YYYY
  # --------------------------------------------------------
  
  mon_year_idx <- grepl(
    "^[a-z]{3}-\\d{4}$",
    raw_dates
  )
  
  if(any(mon_year_idx)){
    
    vals <- raw_dates[mon_year_idx]
    
    month_text <- str_sub(vals,1,3)
    years <- str_sub(vals,5,8)
    
    collection_year[mon_year_idx] <-
      as.integer(years)
    
    collection_month[mon_year_idx] <-
      unname(
        month_lookup[month_text]
      )
    
    date_precision[mon_year_idx] <- "month"
  }
  
  # --------------------------------------------------------
  # DD-MON-YYYY
  # --------------------------------------------------------
  
  full_text_idx <- grepl(
    "^\\d{1,2}-[a-z]{3}-\\d{4}$",
    raw_dates
  )
  
  if(any(full_text_idx)){
    
    vals <- raw_dates[full_text_idx]
    
    day_val <- as.integer(
      str_extract(vals,"^\\d{1,2}")
    )
    
    month_val <- str_extract(
      vals,
      "(?<=-)...[a-z]?(?=-\\d{4})"
    )
    
    month_val <- str_sub(
      vals,
      4,
      6
    )
    
    year_val <- as.integer(
      str_sub(vals,8,11)
    )
    
    collection_year[full_text_idx] <- year_val
    collection_month[full_text_idx] <-
      unname(month_lookup[month_val])
    collection_day[full_text_idx] <- day_val
    
    date_precision[full_text_idx] <- "day"
  }
  
  # ========================================================
  # RESCUE: YEAR RANGE
  # ========================================================
  
  unresolved <- is.na(date_precision)
  
  year_range_idx <- unresolved &
    grepl(
      "^\\d{4}/\\d{4}$",
      raw_dates
    )
  
  if(any(year_range_idx)){
    
    years <- str_extract(
      raw_dates[year_range_idx],
      "\\d{4}"
    )
    
    collection_year[year_range_idx] <-
      as.integer(years)
    
    date_precision[year_range_idx] <-
      "year_range"
  }
  
  # ========================================================
  # RESCUE: MONTH RANGE
  # ========================================================
  
  unresolved <- is.na(date_precision)
  
  month_range_idx <- unresolved &
    grepl(
      "^[a-z]{3}-\\d{4}/[a-z]{3}-\\d{4}$",
      raw_dates
    )
  
  if(any(month_range_idx)){
    
    first_half <- str_extract(
      raw_dates[month_range_idx],
      "^[a-z]{3}-\\d{4}"
    )
    
    month_text <- str_sub(
      first_half,
      1,
      3
    )
    
    year_text <- str_sub(
      first_half,
      5,
      8
    )
    
    collection_year[month_range_idx] <-
      as.integer(year_text)
    
    collection_month[month_range_idx] <-
      unname(
        month_lookup[month_text]
      )
    
    date_precision[month_range_idx] <-
      "month_range"
  }
  
  # ========================================================
  # RESCUE: DUPLICATE DATE STRINGS
  # ========================================================
  
  unresolved <- is.na(date_precision)
  
  dup_idx <- unresolved &
    grepl("_", raw_dates)
  
  if(any(dup_idx)){
    
    year_found <- str_extract(
      raw_dates[dup_idx],
      "\\d{4}"
    )
    
    collection_year[dup_idx] <-
      as.integer(year_found)
    
    date_precision[dup_idx] <-
      "year_extracted"
  }
  
  # ========================================================
  # FINAL YEAR EXTRACTION
  # ========================================================
  
  unresolved <- is.na(date_precision)
  
  extract_idx <- unresolved &
    grepl(
      "\\d{4}",
      raw_dates
    )
  
  if(any(extract_idx)){
    
    year_found <- str_extract(
      raw_dates[extract_idx],
      "\\d{4}"
    )
    
    collection_year[extract_idx] <-
      as.integer(year_found)
    
    date_precision[extract_idx] <-
      "year_extracted"
  }
  
  # ========================================================
  # REMAINING UNPARSED
  # ========================================================
  
  date_precision[
    is.na(date_precision)
  ] <- "missing"
  
  df %>%
    mutate(
      collection_year = collection_year,
      collection_month = collection_month,
      collection_day = collection_day,
      date_precision = date_precision
    )
}

# ==========================================================
# APPLY
# ==========================================================

ena_seq_date <- parse_collection_dates(
  ena_seq_lookup
)

ena_readruns_date <- parse_collection_dates(
  ena_readruns_lookup
)

ncbi_virus_date <- parse_collection_dates(
  ncbi_virus_lookup
)

# ==========================================================
# SAVE
# ==========================================================

write_csv(
  ena_seq_date,
  file.path(
    date_dir,
    "ena_seq_date_parsed.csv"
  )
)

write_csv(
  ena_readruns_date,
  file.path(
    date_dir,
    "ena_readruns_date_parsed.csv"
  )
)

write_csv(
  ncbi_virus_date,
  file.path(
    date_dir,
    "ncbi_virus_date_parsed.csv"
  )
)

# ==========================================================
# QC
# ==========================================================

cat("\n====================================\n")
cat("DATE PARSING COMPLETE\n")
cat("====================================\n")

cat("\nENA SEQ\n")
print(table(ena_seq_date$date_precision))

cat("\nENA READRUNS\n")
print(table(ena_readruns_date$date_precision))

cat("\nNCBI VIRUS\n")
print(table(ncbi_virus_date$date_precision))