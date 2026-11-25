
# ==========================================================
# CREATE ALL TABLES
# ==========================================================

# MAIN TABLES

source("scripts/reporting/01_table_metadata_completeness.R")
source("scripts/reporting/02_table_date_precision.R")
source("scripts/reporting/03_table_geographic_classification.R")
source("scripts/reporting/04_table_host_distribution.R")
source("scripts/reporting/05_taxonomy_distribution.R")
source("scripts/reporting/06_table_record_class_distribution.R")
source("scripts/reporting/07_table_sequence_redundancy_diagnostics.R")
source("scripts/reporting/08_table_inventory_comparison.R")
source("scripts/reporting/09_table_linkage-coverage.R")


# ==========================================================
# SUPPLEMENTARY TABLES
# ==========================================================

source("scripts/reporting/s1__table_source_key_archive_representation.R")
source("scripts/reporting/s2_table_source_key_hierarchy_utilization.R")
source("scripts/reporting/s3_table_burden_source_coverage.R")
source("scripts/reporting/s4_table_burden_measure_coverage.R")
source("scripts/reporting/s5_table_record_class_representation_among_shared_source_keys.R")


# ==========================================================
# COMPLETION MESSAGE
# ==========================================================

cat("\n=====================================\n")
cat("ALL TABLES GENERATED\n")
cat("=====================================\n")
