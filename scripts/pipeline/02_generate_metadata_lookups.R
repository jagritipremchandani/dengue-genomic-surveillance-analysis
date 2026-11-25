# ==========================================================
# 02_generate_metadata_lookups.R
# ==========================================================

library(tidyverse)

data_dir <- "data/intermediate/standardized"

lookup_dir<- "data/intermediate/lookup_tables"


dir.create(
  lookup_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ena_seq_std <- read_csv(
  file.path(data_dir, "ena_seq_standardized.csv"),
  show_col_types = FALSE
)

ena_readruns_std <- read_csv(
  file.path(data_dir, "ena_readruns_standardized.csv"),
  show_col_types = FALSE
)

ncbi_virus_std <- read_csv(
  file.path(data_dir, "ncbi_virus_standardized.csv"),
  show_col_types = FALSE
)

# ==========================================================
# COUNTRY LOOKUP
# ==========================================================

country_values <- bind_rows(
  ena_seq_std %>% select(country),
  ena_readruns_std %>% select(country),
  ncbi_virus_std %>% select(country)
) %>%
  filter(!is.na(country)) %>%
  distinct(country) %>%
  arrange(country)

classify_country <- function(x){
  
  x <- trimws(tolower(x))
  
  # --------------------------------------------------------
  # MISSING
  # --------------------------------------------------------
  
  if(x %in% c(
    "missing",
    "not_collected"
  )){
    return(c(NA, "MISSING"))
  }
  
  # --------------------------------------------------------
  # OCEANS
  # --------------------------------------------------------
  
  if(x %in% c(
    "atlantic_ocean",
    "pacific_ocean"
  )){
    return(c(NA, "OCEAN"))
  }
  
  # --------------------------------------------------------
  # REGIONS
  # --------------------------------------------------------
  
  if(x == "borneo"){
    return(c(NA, "REGION"))
  }
  
  # --------------------------------------------------------
  # SPECIAL COUNTRY FIXES
  # --------------------------------------------------------
  
  if(x == "505_[nicaragua]"){
    return(c("Nicaragua", "COUNTRY"))
  }
  
  if(x == "usa"){
    return(c("United States", "COUNTRY"))
  }
  
  if(x == "laos"){
    return(c("Lao People's Democratic Republic", "COUNTRY"))
  }
  
  if(x == "tanzania"){
    return(c("United Republic Of Tanzania", "COUNTRY"))
  }
  
  if(x == "south_korea"){
    return(c("Republic Of Korea", "COUNTRY"))
  }
  
  if(x == "east_timor"){
    return(c("Timor-Leste", "COUNTRY"))
  }
  
  if(x == "cape_verde"){
    return(c("Cabo Verde", "COUNTRY"))
  }
  
  if(x == "democratic_republic_of_the_congo"){
    return(c("Democratic Republic Of The Congo", "COUNTRY"))
  }
  
  if(x == "republic_of_the_congo"){
    return(c("Republic Of The Congo", "COUNTRY"))
  }
  
  if(x == "micronesia"){
    return(c("Micronesia (Federated States Of)", "COUNTRY"))
  }
  
  if(x == "iran"){
    return(c("Iran (Islamic Republic Of)", "COUNTRY"))
  }
  
  if(x == "brunei"){
    return(c("Brunei Darussalam", "COUNTRY"))
  }
  
  if(x == "russia"){
    return(c("Russian Federation", "COUNTRY"))
  }
  
  # --------------------------------------------------------
  # SUBNATIONAL
  # --------------------------------------------------------
  
  if(x %in% c(
    "new_delhi",
    "new_delhi_india"
  )){
    return(c("India", "SUBNATIONAL"))
  }
  
  # --------------------------------------------------------
  # TERRITORY EXCEPTIONS
  # --------------------------------------------------------
  
  if(x == "france_guadeloupe"){
    return(c("Guadeloupe", "TERRITORY"))
  }
  
  if(x == "france_martinique"){
    return(c("Martinique", "TERRITORY"))
  }
  
  # --------------------------------------------------------
  # CLEAN DUPLICATES
  # --------------------------------------------------------
  
  if(x == "thailand_thailand"){
    return(c("Thailand", "COUNTRY"))
  }
  
  # --------------------------------------------------------
  # TERRITORIES
  # --------------------------------------------------------
  
  territories <- c(
    "american_samoa",
    "guadeloupe",
    "guam",
    "martinique",
    "mayotte",
    "new_caledonia",
    "puerto_rico",
    "reunion",
    "saint_barthelemy",
    "saint_martin",
    "wallis_and_futuna",
    "french_guiana"
  )
  
  if(x %in% territories){
    
    lab <- x %>%
      str_replace_all("_", " ") %>%
      str_to_title()
    
    return(c(lab, "TERRITORY"))
  }
  
  # --------------------------------------------------------
  # DEFAULT COUNTRY STANDARDIZATION
  # --------------------------------------------------------
  
  lab <- x %>%
    str_replace_all("_", " ") %>%
    str_to_title()
  
  return(c(lab, "COUNTRY"))
}


country_mat <- t(
  sapply(
    country_values$country,
    classify_country
  )
)

country_lookup_final <- country_values %>%
  mutate(
    country_standardized = country_mat[,1],
    location_type = country_mat[,2]
  ) %>%
  rename(
    country_raw = country
  )

write_csv(
  country_lookup_final,
  file.path(
    lookup_dir,
    "country_lookup_final.csv"
  )
)


# ==========================================================
# HOST LOOKUP
# ==========================================================

host_values <- bind_rows(
  
  ena_seq_std %>%
    select(host) %>%
    rename(host_raw = host),
  
  ena_readruns_std %>%
    select(host) %>%
    rename(host_raw = host),
  
  ncbi_virus_std %>%
    select(host_name) %>%
    rename(host_raw = host_name)
  
) %>%
  filter(!is.na(host_raw)) %>%
  distinct(host_raw) %>%
  arrange(host_raw)

classify_host <- function(x){
  
  x <- trimws(tolower(x))
  
  human_terms <- c(
    "human",
    "homo_sapiens",
    "homo-sapiens",
    "homo_sapien",
    "homo_sapience",
    "homo_sapiense",
    "homo_sapines",
    "home_sapiens:_male",
    "home_sapiens:_female",
    "acute_febrile_human",
    "dengue_fever_patient",
    "homo_sapiens_homo_sapiens"
  )
  
  if(x %in% human_terms){
    return(c("Homo sapiens","Human"))
  }
  
  if(x=="homo_sapiens_and_aedes_aegypti"){
    return(c("Mixed human/vector","Mixed"))
  }
  
  invalid_terms <- c(
    "denv1",
    "denv2",
    "denv3",
    "denv4",
    "virus",
    "serum",
    "bei_strain",
    "cber_strain",
    "dengue"
  )
  
  if(x %in% invalid_terms){
    return(c(NA,"InvalidHost"))
  }
  
  if(x=="unknown"){
    return(c(NA,"Unknown"))
  }
  
  if(x %in% c(
    "c6-36",
    "c6/36_cells",
    "supernatants_of_c6/36_cells"
  )){
    return(c("C6/36 cells","CellCulture"))
  }
  
  if(x %in% c(
    "vero_cell",
    "vero_81_cells",
    "medium_vero_infected_cells"
  )){
    return(c("Vero cells","CellCulture"))
  }
  
  if(x=="bhk-21_cells"){
    return(c("BHK-21 cells","CellCulture"))
  }
  
  if(x=="in_vitro"){
    return(c("In vitro culture","CellCulture"))
  }
  
  vector_map <- c(
    "ae.","aedes","aedes_sp.",
    "culex","culex_sp.","cx."
  )
  
  if(x %in% vector_map){
    
    if(grepl("culex|cx\\.",x)){
      return(c("Culex sp.","Vector"))
    }
    
    return(c("Aedes sp.","Vector"))
  }
  
  if(x %in% c(
    "mosquito",
    "mosquitoe",
    "mosquitoes",
    "mosquito_suspension"
  )){
    return(c("Mosquito","Vector"))
  }
  
  if(x=="arthropod"){
    return(c("Arthropod","Vector"))
  }
  
  if(x=="culicidae"){
    return(c("Culicidae","Vector"))
  }
  
  if(x=="culicine_mosquito"){
    return(c("Culicine mosquito","Vector"))
  }
  
  if(x=="culicidae:_culex"){
    return(c("Culex sp.","Vector"))
  }
  
  if(grepl("^aedes_",x)){
    return(
      c(
        str_replace_all(x,"_"," ") |>
          str_to_sentence(),
        "Vector"
      )
    )
  }
  
  if(grepl("^culex_",x)){
    return(
      c(
        str_replace_all(x,"_"," ") |>
          str_to_sentence(),
        "Vector"
      )
    )
  }
  
  if(x %in% c("gadfly","midge")){
    return(
      c(
        str_to_title(x),
        "Vector"
      )
    )
  }
  
  animal_name <- x %>%
    str_replace_all("_"," ") %>%
    str_to_sentence()
  
  return(c(animal_name,"Animal"))
}

host_mat <- t(
  sapply(
    host_values$host_raw,
    classify_host
  )
)

host_lookup_final <- host_values %>%
  mutate(
    host_standardized = host_mat[,1],
    host_group = host_mat[,2]
  )

write_csv(
  host_lookup_final,
  file.path(
    lookup_dir,
    "host_lookup_final.csv"
  )
)


# ==========================================================
# QC
# ==========================================================

cat("\n====================================\n")
cat("LOOKUP GENERATION COMPLETE\n")
cat("====================================\n")

cat(
  "\nCountry lookup rows:",
  nrow(country_lookup_final),
  "\n"
)

cat(
  "Host lookup rows:",
  nrow(host_lookup_final),
  "\n"
)

print(
  table(country_lookup_final$location_type)
)

print(
  table(host_lookup_final$host_group)
)