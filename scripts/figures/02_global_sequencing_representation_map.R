#Map global_representation.csv to latitudes N longitudes
library(tidyverse)

df <- read_csv(
  "C:/Users/pc/Desktop/auditing_dengue/outputs/figure_csvs/global_sequencing_representation_map.csv",
  show_col_types = FALSE
)

# 1. Total rows and summary of n_sources
cat("Total rows:", nrow(df), "\n")
cat("Unique ISO3:", n_distinct(df$iso3), "\n")
print(summary(df$n_sources))

# 2. Check duplicate ISO3 or duplicate country names
df %>%
  count(iso3, name = "n_rows") %>%
  filter(n_rows > 1) %>%
  print()

# 3. Inspect lowest-count countries (e.g., <= 5 sources)
df %>%
  arrange(n_sources, country) %>%
  select(country, iso3, n_sources) %>%
  print(n = 50)

# 4. Inspect non-endemic / high-latitude regions (e.g., Europe & Northern/Southern extremes)
# If your CSV has a region/continent column, filter by it; otherwise check by ISO3 or country name:
df %>%
  filter(iso3 %in% c(
    "GBR", "FRA", "DEU", "ESP", "ITA", "SWE", "NOR", "FIN", "POL", "CAN", "CHL", "ARG", "URY"
  )) %>%
  arrange(desc(n_sources)) %>%
  print(n = 50)
