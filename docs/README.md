# Dengue Genomic Surveillance Inventory Framework

**Author:** Jagriti Premchandani
**Status:** v1 baseline

---

## 1. Project overview

This repository contains a reproducible framework for constructing pathogen genomic surveillance inventories from public genomic archives and integrating them with country-year disease burden datasets.

| | Pathogen |
|---|---|
| **Current implementation** | Dengue virus |
| **Planned** | Lassa virus, *Klebsiella pneumoniae* |

## 2. Repository purpose

The repository is designed to:

- Harmonize metadata
- Classify genomic records
- Construct multiple surveillance inventories
- Harmonize burden datasets
- Create country-year analytical datasets

The framework does **not** estimate infections, patients or transmission events. Genomic records reflect sequencing and submission effort, not disease occurrence.

## 3. Workflow summary

```
Genomic Archives
      ↓
Metadata Harmonization
      ↓
Taxonomy & Classification
      ↓
Inventory Construction
      ↓
Country-Year Aggregation (genomic)


Burden Sources
      ↓
Burden Harmonization
      ↓
Country-Year Aggregation (burden)


Genomic Country-Year
          +
Burden Country-Year
      ↓
Pathogen Country-Year
```

## 4. Data inputs

**Retrieved automatically by pipeline scripts**

- ENA Sequence Records
- ENA SRA Read Runs
- NCBI Virus Records

**Retrieved manually**

- OpenDengue (**reported** surveillance counts)
- Global Burden of Disease, GBD (**modelled** estimates)

The two burden sources are kept separate and are not interchangeable.

Raw source data are not redistributed. The sources have independent licensing and update policies, so this repository distributes code and processed outputs only. Check each source's terms before reuse, in particular GBD/IHME.

## 5. Directory structure

```
README.md
docs/
scripts/
data/
outputs/
```

## 6. Script groups

Scripts are numbered and run sequentially.

| Scripts | Stage |
|---|---|
| 01–06 | Metadata processing |
| 07–09 | Inventory construction |
| 10–13 | Burden harmonization |
| 14–16 | Country-year integration |

## 7. Outputs

**Inventories**

- Archive Inventory
- Sequence Inventory
- Source Inventory

**Burden products**

- Burden Master
- Burden Country-Year

**The project deliberately keeps distinct counting units:**

| Inventory | Counting unit |
|---|---|
| Archive inventory | `accession` / `run_accession` |
| Sequence inventory | `sequence_md5` |
| **Source inventory (final)** | `source_key` + `serotype` + `record_class` |
| Country-year analysis | `ISO3` + `Year` |

**Analytical products**

| Output | Unit | Description | Scripts |
|---|---|---|---|
| Genomic Country-Year | `ISO3` + `Year` | Genomic surveillance records from the source inventory aggregated to country and year. Describes where and when dengue genomes were deposited, independent of any burden data. | 14 |
| Pathogen Country-Year | `ISO3` + `Year` | Genomic Country-Year joined to Burden Country-Year on ISO3 code and year. This is the integrated dataset used to compare genomic representation with reported (OpenDengue) and modelled (GBD) burden. | 15 |
| Period-Matched Inventory | `ISO3` + `Year` | Inventory restricted to the time window covered by the burden data, so that genomic and burden measures are compared over the same period. | 16 |

These datasets describe sequencing and submission effort relative to burden. They do not estimate infections, patients or transmission events.

**Reporting outputs**

- Tables
- Figures
- Validation Workbook

## 8. Documentation

| Document | Contents |
|---|---|
| `docs/01_pipeline_and_methods.md` | Methods, counting units and analytical decisions |
| `docs/02_outputs_index.md` | Script-to-output mapping |

## 9. Reproducibility

- **Genomic archive extraction date:** 17 November 2026. Archive contents change continuously, so later pulls will differ.
- Scripts are intended to be executed sequentially. There is no Makefile or master runner.
- **Language:** R >= 4.3. The genomic metadata download step is the only non-R component.
- **Packages:** tidyverse, readr, dplyr, tidyr, stringr, lubridate, ggplot2, sf, rnaturalearth, leaflet, openxlsx, ggrepel, viridis, webshot2
- **Dependency management:** package versions were managed in a local R environment. No `renv` lockfile is included in the current release.
- **Resources:** roughly 15–30 minutes and under 8 GB RAM for a full dengue run on a modern laptop (an estimate, not a benchmark).

---
