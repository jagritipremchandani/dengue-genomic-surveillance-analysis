# Dengue Genomic Surveillance Inventory Framework

A reproducible R framework for building dengue genomic surveillance inventories from public sequence archives and linking them to country-year disease burden.

**Author:** Jagriti Premchandani
---

## Overview

The workflow harmonizes metadata from:

- ENA Sequence records
- ENA SRA read runs
- NCBI Virus
- OpenDengue surveillance data (**reported** burden)
- Global Burden of Disease (GBD) estimates (**modelled** burden)

and produces archive-level, sequence-level, source-level and country-year analytical datasets.

**Current pathogen:** dengue virus. **Planned:** Lassa virus, *Klebsiella pneumoniae*.

## Data retrieval date

Genomic archive retrievals (ENA Sequence, ENA SRA, NCBI Virus) correspond to the archive state available on **17 November 2026**. Archive contents change continuously, so counts reported here will differ from later pulls.

## Key findings

- 93,826 source-level dengue genomic observations were identified.
- Archive, sequence and source inventories give substantially different counts, so no single deduplicated total adequately describes genomic surveillance.
- Surveillance is geographically widespread but highly uneven, with strongest representation in South Asia, Southeast Asia and Latin America.
- Approximately two-thirds of source-level observations were represented in both ENA and NCBI; about one-third were archive-specific. <!-- VERIFY: see note on 66.9% vs 46,564 -->
- ISO-harmonized country-year linkage achieved approximately 95% coverage.
- Positive ecological associations were observed between genomic surveillance representation and dengue burden across country-years (Figure 5; rank correlation ρ = 0.55 for cases, 0.50 for incidence, 0.43 for deaths, 0.46 for DALYs; P < 0.001). These are descriptive associations and do not indicate that burden drives surveillance effort or the reverse.

## Inventory units

The project deliberately keeps distinct counting units:

| Inventory | Counting unit |
|---|---|
| Archive inventory | `accession` / `run_accession` |
| Sequence inventory | `sequence_md5` |
| **Source inventory (final)** | `source_key` + `serotype` + `record_class` |
| Country-year analysis | `ISO3` + `Year` |

The source inventory is the primary unit for headline counts.

### Two overlap constructs (not interchangeable)

1. **Historical isolate-overlap diagnostics:** an earlier diagnostic of isolate overlap between archives. [TO FILL: definition and count]
2. **Final source-key archive representation:** classification of each final source-level observation as ENA-only, NCBI-only or both. [TO FILL: counts]

Figures from these two constructs are different quantities and should not be compared directly.

## What this framework does not estimate

- infections
- patients
- transmission events
- incidence from genomic records

## Workflow

Scripts are intended to be run sequentially, **01 through 16**. There is no Makefile or master runner.

| Scripts | Stage |
|---|---|
| 01-06 | Metadata harmonization: standardization, country and host lookup, date parsing, taxonomy assignment, record classification |
| 07-09 | Inventory construction: archive, sequence, source inventories; archive representation |
| 10-13 | Burden harmonization: OpenDengue, GBD, burden master, burden country-year |
| 14-16 | Country-year framework: genomic country-year, pathogen country-year, period-matched inventory |

## Repository structure

```
README.md
docs/
    01_pipeline_and_methods.md
    02_outputs_index.md
scripts/
    pipeline/
    qc/
    reporting/
    figures/
data/
    intermediate/
    standardized/
outputs/
    tables/
    figures/
    figure_csvs/
```

## Outputs

9 main tables, 5 supplementary tables, 7 figures. See `docs/02_outputs_index.md` for the script-to-output mapping and `docs/01_pipeline_and_methods.md` for methods and analytical decisions.

## Requirements

- **Language:** R >= 4.3 (genomic metadata download step is the only non-R component: [TO FILL: tool/language])
- **Packages:** tidyverse, readr, dplyr, tidyr, stringr, lubridate, ggplot2, sf, rnaturalearth, leaflet, openxlsx, ggrepel, viridis
- **Reproducibility note:** package versions were managed within a local R environment. No `renv` lockfile is included in the current release.
- **Approximate resources:** roughly 15-30 minutes and under 8 GB RAM for a full dengue run on a modern laptop (estimate, not a benchmark).

## Data access and redistribution

- **Genomic archives** (ENA Sequence, ENA SRA, NCBI Virus) are retrieved by pipeline scripts.
- **OpenDengue and GBD** were manually downloaded from their distribution portals and then processed in the burden harmonization workflow.
- Because source datasets come from external repositories with independent licensing and update policies, this repository distributes code and processed outputs rather than redistributing all raw source data. Users should check the terms of each source, in particular GBD/IHME, before reuse.

## Burden data: reported vs modelled

OpenDengue represents **reported** surveillance counts; GBD represents **modelled** estimates. They are kept as separate sources and are not interchangeable.

## Status

| Component | Status |
|---|---|
| Pipeline | Complete |
| Inventories | Complete |
| Burden integration | Complete |
| Country-year linkage | Complete |
| Descriptive analyses | Complete |



