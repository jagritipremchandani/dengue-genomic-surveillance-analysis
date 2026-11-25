# Outputs Index

This document maps pipeline scripts to analytical outputs, reporting tables, and figures generated during the dengue genomic surveillance workflow.

## Contents

1. [Pipeline Scripts](#1-pipeline-scripts)
2. [Main Tables](#2-main-tables)
3. [Supplementary Tables](#3-supplementary-tables)
4. [Figures](#4-figures)
5. [Core Analytical Datasets](#5-core-analytical-datasets)
6. [Interpretation Hierarchy](#6-interpretation-hierarchy)

---

## 1. Pipeline Scripts

### Stage A: Metadata standardization and enrichment (01-06)

| Script | Primary Outputs | Purpose |
|---|---|---|
| `01_standardize_archives.R` | `ena_seq_standardized.csv`, `ena_readruns_standardized.csv`, `ncbi_virus_standardized.csv` | Archive-specific metadata standardization |
| `02_generate_metadata_lookups.R` | `country_lookup_final.csv`, `host_lookup_final.csv` | Geographic and host harmonization lookups |
| `03_apply_metadata_lookups.R` | Lookup-applied datasets | Apply country and host classifications |
| `04_date_parsing.R` | Date-parsed datasets | Temporal harmonization and precision assignment |
| `05_taxonomy_lookup.R` | `taxid_lookup_final.csv` | Dengue serotype assignment framework |
| `06_record_classification.R` | Classified datasets | Taxonomy enrichment and record-class assignment |

### Stage B: Inventories (07-09)

| Script | Primary Outputs | Purpose |
|---|---|---|
| `07_inventory_generation.R` | `archive_inventory.csv`, `sequence_inventory.csv` | Archive-level and sequence-level inventories |
| `08_source_key_archive_representation.R` | `source_key_overlap_inventory.csv` | Archive representation of source-level observations |
| `09_source_inventory_generation.R` | `source_inventory.csv` | Source-level surveillance inventory |

### Stage C: Burden preparation (10-13)

| Script | Primary Outputs | Purpose |
|---|---|---|
| `10_prepare_surveillance_burden.R` | `dengue_surveillance_clean.csv` | OpenDengue surveillance burden harmonization |
| `11_prepare_gbd_burden.R` | `dengue_gbd_clean.csv` | GBD burden harmonization |
| `12_burden_harmonization.R` | `burden_master.csv` | Unified burden framework |


### Stage D: Country-year integration (14-16)

| Script | Primary Outputs | Purpose |
|---|---|---|
| `13_burden_country_year.R` |  `burden_country_year.csv` | Canonical country-year burden layer combining annual OpenDengue surveillance observations and annual GBD burden estimates.|
| `14_genomic_country_year.R` | `genomic_country_year.csv` | Country-year genomic surveillance layer |
| `15_pathogen_country_year.R` | `pathogen_country_year.csv` | Burden-linked pathogen-country-year dataset |
| `16_period_matched_inventory.R` | `source_inventory_period_matched.csv` | Burden-compatible source inventory subset |

---

## 2. Main Tables

| Table | Output | Purpose |
|---|---|---|
| Table 01 | `table_01_metadata_completeness.csv` | Metadata completeness by archive |
| Table 02 | `table_02_date_precision.csv` | Date precision distribution |
| Table 03 | `table_03_geographic_classification.csv` | Geographic classification framework |
| Table 04 | `table_04_host_distribution.csv` | Host-group composition |
| Table 05 | `table_05_taxonomy_distribution.csv` | Serotype distribution |
| Table 06 | `table_06_record_class_distribution.csv` | Record-class distribution |
| Table 07 | `table_07_sequence_redundancy.csv` | Sequence redundancy diagnostics |
| Table 08 | `table_09_inventory_comparison.csv` | Inventory comparison across archive, sequence, and source inventories |
| Table 9 | `table_10_linkage_coverage.csv` | Country-year burden linkage coverage |

---

## 3. Supplementary Tables

| Table | Output | Purpose |
|---|---|---|
| Table S1 | `table_S1_source_key_archive_representation` | Archive Distribution of Sources |
| Table S2 | `table_S2_source_key_hierarchy.csv` | Source-key construction hierarchy |
| Table S3 | `table_S3_burden_source_coverage.csv` | Burden dataset coverage by source |
| Table S4 | `table_S4_burden_measure_coverage.csv` | Burden measure coverage |
| Table S5 | `table_S5_record_class_representation.csv` | Record-class representation among shared source observations |

> **Note:** Table S1 was retired after overlap methodology revisions. Archive representation is now reported directly in Table 08.

---

## 4. Figures

| Figure | Title | Question | Dataset | Metric | Visualization |
|---|---|---|---|---|---|
| 01 | Global Distribution of Dengue Genomic Surveillance Sources | Where is dengue genomic surveillance represented? | `global_sequencing_representation.csv` | `n_sources` | Interactive bubble map and publication bubble map |
| 02 | Global Persistence of Dengue Genomic Surveillance | Which countries sustain genomic surveillance through time? | `surveillance_persistence.csv` | `years_represented` | Country-level choropleth or persistence map |
| 03A | Temporal Growth of Dengue Genomic Surveillance | Has dengue genomic surveillance increased through time? | `temporal_surveillance_growth.csv` | `n_sources` per year | Line plot |
| 03B| Temporal Trends in Dengue Genomic Surveillance by Serotype 1905-2005 | How has genomic surveillance evolved across dengue serotypes? | `temporal_surveillance_growth_by_serotype.csv` | `n_sources` per year by serotype | Multi-line serotype trend plot |
| 03C| Temporal Trends in Dengue Genomic Surveillance by Serotype 2005-2025 | How has genomic surveillance evolved across dengue serotypes? | `temporal_surveillance_growth_by_serotype.csv` | `n_sources` per year by serotype | Multi-line serotype trend plot |
| 05 | Burden vs Genomic Surveillance Representation | Do higher-burden country-years receive greater genomic surveillance representation? | `burden_surveillance_correlation.csv` | Cases, incidence, deaths, DALYs | Scatterplots with Spearman correlations |
| 06 | Repository Representation | To what extent are source-level dengue genomic observations represented in both ENA and NCBI versus a single archive? | source_archive_overlap_inventory.csv | archive_status | Horizontal Bar Chart|

---

## 5. Core Analytical Datasets

| Dataset | Description |
|---|---|
| `archive_inventory.csv` | Archive-level representation of genomic surveillance |
| `sequence_inventory.csv` | Unique nucleotide sequence inventory |
| `source_inventory.csv` | Source-level surveillance inventory |
| `source_key_overlap_inventory.csv`| source-level observations, indicating whether source keys are represented in ENA only, NCBI only, or both archives|
| `burden_master.csv` | Harmonized burden framework |
| `burden_country_year.csv` | Annual burden analysis dataset |
| `genomic_country_year.csv` | Annual genomic surveillance analysis dataset |
| `pathogen_country_year.csv` | Burden-linked pathogen-country-year dataset |
| `source_inventory_period_matched.csv` | Burden-compatible source inventory subset |

---

## 6. Interpretation Hierarchy

The workflow intentionally preserves multiple analytical layers:

```
Archive Inventory
      ↓
Sequence Inventory
      ↓
Source Inventory
      ↓
Genomic Country-Year
      ↓
Pathogen-Country-Year
```

Each layer answers a different surveillance question and should not be interpreted as an infection count, patient count, or incidence estimate.
