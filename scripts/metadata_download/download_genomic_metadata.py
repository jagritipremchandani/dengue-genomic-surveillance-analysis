import json
import time
from pathlib import Path
import requests
import pandas as pd


def resolve_project_root() -> Path:
    """Resolve repository root cleanly whether run as a script or interactively."""
    try:
        script_path = Path(__file__).resolve()
        # If placed inside <repo>/scripts/<subdir>/script.py, parents[2] is <repo>.
        # If placed directly in <repo>/ or <repo>/scripts/, walk up until data/ or .git is found.
        for candidate in [script_path.parent, *script_path.parents]:
            if (candidate / ".git").exists() or (
                (candidate / "data").exists() and (candidate / "scripts").exists()
            ):
                return candidate
        return script_path.parents[2] if len(script_path.parents) > 2 else script_path.parent
    except NameError:
        cwd = Path.cwd().resolve()
        for candidate in [cwd, *cwd.parents]:
            if (candidate / ".git").exists() or (
                (candidate / "data").exists() and (candidate / "scripts").exists()
            ):
                return candidate
        return cwd


# ============================================================================
# PATH CONFIGURATION - RELATIVE TO PROJECT FOLDER (data/raw)
# ============================================================================
SNAPSHOT_DATE = "2026-11-17"
PROJECT_ROOT = resolve_project_root()
BASE_RAW_DIR = PROJECT_ROOT / "data" / "raw" / f"{SNAPSHOT_DATE}_snapshot"

VIRAL_DIR = BASE_RAW_DIR / "sequences_viral"
BACT_DIR = BASE_RAW_DIR / "sequences_bacterial"
READS_DIR = BASE_RAW_DIR / "raw_reads_sra_ena"
BURDEN_DIR = BASE_RAW_DIR / "burden_sources"
CURATED_DIR = BASE_RAW_DIR / "curated_resources"

for d in [VIRAL_DIR, BACT_DIR, READS_DIR, BURDEN_DIR, CURATED_DIR]:
    d.mkdir(parents=True, exist_ok=True)

# Broad species/parent nodes. ENA tax_tree() includes descendants.
# IMPORTANT: current NCBI species TaxID for Lassa virus is 3052310.
PILOT_TAXA = {
    "lassa": {
        "taxid": "3052310",
        "domain": "Virus",
        "name": "Lassa virus",
        "scope_note": "Mammarenavirus lassaense (Lassa virus), species plus descendants",
    },
    "dengue": {
        "taxid": "12637",
        "domain": "Virus",
        "name": "Dengue virus",
        "scope_note": "Dengue virus parent plus descendants, including DENV-1 to DENV-4",
    },
    "kpneumoniae": {
        "taxid": "573",
        "domain": "Bacteria",
        "name": "Klebsiella pneumoniae",
        "scope_note": "Klebsiella pneumoniae species plus descendants",
    },
}

ENA_PORTAL_URL = "https://www.ebi.ac.uk/ena/portal/api/search"
NCBI_DATASETS_BASES = [
    "https://api.ncbi.nlm.nih.gov/datasets/v2",
    "https://api.ncbi.nlm.nih.gov/datasets/v2alpha",
]
HEADERS = {
    "User-Agent": "Genomic-Surveillance-Audit-Pilot/1.0",
    "Accept": "application/json",
}


def get_valid_ena_fields(result_type: str) -> set[str]:
    url = "https://www.ebi.ac.uk/ena/portal/api/returnFields"
    try:
        resp = requests.get(
            url,
            params={"result": result_type, "format": "json"},
            headers=HEADERS,
            timeout=30,
        )
        resp.raise_for_status()
        return {item["columnId"] for item in resp.json()}
    except Exception as exc:
        print(f"     [!] Could not validate ENA fields for {result_type}: {exc}")
        return {
            "accession", "run_accession", "experiment_accession",
            "tax_id", "scientific_name", "collection_date", "country",
            "host", "sample_accession", "study_accession"
        }


def fetch_ena_tsv(result_type: str, taxid: str, desired_fields: list[str], out_path: Path) -> int:
    valid_fields = get_valid_ena_fields(result_type)
    fields_to_use = [f for f in desired_fields if f in valid_fields]
    if not fields_to_use:
        fields_to_use = ["accession", "tax_id"]

    params = {
        "result": result_type,
        "query": f"tax_tree({taxid})",
        "fields": ",".join(fields_to_use),
        "format": "tsv",
        "limit": "0",
    }
    print(f"  -> Querying ENA [{result_type}] for TaxID {taxid} using tax_tree()...")
    try:
        resp = requests.get(ENA_PORTAL_URL, params=params, headers=HEADERS, timeout=300)
        resp.raise_for_status()
    except Exception as exc:
        print(f"     [!] ENA error for {result_type}, TaxID {taxid}: {exc}")
        return 0

    text = resp.text.strip()
    if not text:
        print(f"     [!] Empty ENA response for {result_type}, TaxID {taxid}")
        return 0

    out_path.write_text(text + "\n", encoding="utf-8")
    row_count = max(0, len(text.splitlines()) - 1)
    print(f"     [OK] {row_count:,} records -> {out_path.name}")
    return row_count


def _first_present(mapping: dict, *keys):
    for key in keys:
        value = mapping.get(key)
        if value not in (None, "", [], {}):
            return value
    return None


def _nested(mapping: dict, *path):
    current = mapping
    for key in path:
        if not isinstance(current, dict):
            return None
        current = current.get(key)
    return current


def _normalise_virus_report(rec: dict) -> dict:
    virus = rec.get("virus") or rec.get("organism") or {}
    isolate = rec.get("isolate") or {}
    location = rec.get("location") or rec.get("geo_location") or {}
    host = rec.get("host") or {}

    bioprojects = rec.get("bioprojects") or rec.get("bioproject_accessions") or []
    if isinstance(bioprojects, list):
        bioprojects = ";".join(
            str(x.get("accession") if isinstance(x, dict) else x)
            for x in bioprojects
        )

    biosample = rec.get("biosample") or rec.get("biosample_accession")
    if isinstance(biosample, dict):
        biosample = biosample.get("accession")

    return {
        "accession": _first_present(rec, "accession", "nucleotide_accession"),
        "source_database": _first_present(rec, "source_database", "source_db"),
        "taxid": _first_present(virus, "tax_id", "taxid") or rec.get("tax_id"),
        "organism_name": _first_present(virus, "sci_name", "organism_name", "name")
                         or rec.get("organism_name"),
        "isolate_name": _first_present(isolate, "name", "isolate_name") or rec.get("isolate_name"),
        "biosample": biosample,
        "bioprojects": bioprojects,
        "collection_date": _first_present(isolate, "collection_date") or rec.get("collection_date"),
        "release_date": rec.get("release_date"),
        "geo_location": _first_present(location, "geographic_location", "geo_loc_name", "country")
                        if isinstance(location, dict) else location,
        "geographic_region": _first_present(location, "geographic_region", "region")
                             if isinstance(location, dict) else None,
        "host_name": _first_present(host, "sci_name", "organism_name", "name")
                     if isinstance(host, dict) else host,
        "length": _first_present(rec, "length", "sequence_length"),
        "completeness": rec.get("completeness"),
        "segment": rec.get("segment"),
    }


def fetch_ncbi_virus_metadata(taxid: str, out_path: Path) -> int:
    """Fetch paginated NCBI Datasets virus dataset reports, metadata only."""
    print(f"  -> Querying NCBI Virus dataset report for TaxID {taxid}...")

    selected_url = None
    payload = None
    last_error = None

    for base in NCBI_DATASETS_BASES:
        url = f"{base}/virus/taxon/{taxid}/dataset_report"
        try:
            resp = requests.get(
                url,
                params={"page_size": 1000},
                headers=HEADERS,
                timeout=180,
            )
            resp.raise_for_status()
            payload = resp.json()
            selected_url = url
            break
        except Exception as exc:
            last_error = exc

    if selected_url is None:
        print(f"     [!] NCBI Virus endpoint failed for TaxID {taxid}: {last_error}")
        return 0

    rows = []
    page_token = None
    page_num = 1

    while True:
        if page_num > 1:
            params = {"page_size": 1000, "page_token": page_token}
            try:
                resp = requests.get(selected_url, params=params, headers=HEADERS, timeout=180)
                resp.raise_for_status()
                payload = resp.json()
            except Exception as exc:
                print(f"     [!] NCBI Virus page {page_num} failed: {exc}")
                break

        reports = payload.get("reports") or payload.get("virus_reports") or []
        for report in reports:
            rec = report.get("virus") if set(report.keys()) == {"virus"} else report
            rows.append(_normalise_virus_report(rec))

        page_token = (
            payload.get("next_page_token")
            or _nested(payload, "pagination", "next_page_token")
        )
        if not page_token:
            break

        page_num += 1
        time.sleep(0.35)

    rows = [r for r in rows if r.get("accession")]
    if not rows:
        diagnostic = out_path.with_suffix(".response_preview.json")
        diagnostic.write_text(json.dumps(payload, indent=2)[:20000], encoding="utf-8")
        print(f"     [!] NCBI returned data but no reports were parsed. Preview -> {diagnostic.name}")
        return 0

    df = pd.DataFrame(rows).drop_duplicates(subset=["accession"])
    df.to_csv(out_path, sep="\t", index=False)
    print(f"     [OK] {len(df):,} unique NCBI virus records -> {out_path.name}")
    return len(df)


def fetch_ncbi_bacterial_assemblies(taxid: str, out_path: Path) -> int:
    url = f"https://api.ncbi.nlm.nih.gov/datasets/v2/genome/taxon/{taxid}/dataset_report"
    params = {"page_size": 1000}
    rows = []
    page_token = None

    print(f"  -> Querying NCBI bacterial assemblies for TaxID {taxid}...")
    while True:
        if page_token:
            params["page_token"] = page_token
        try:
            resp = requests.get(url, params=params, headers=HEADERS, timeout=180)
            resp.raise_for_status()
            payload = resp.json()
        except Exception as exc:
            print(f"     [!] NCBI Assembly API error for TaxID {taxid}: {exc}")
            break

        for r in payload.get("reports", []):
            asm_info = r.get("assembly_info", {})
            biosample = asm_info.get("biosample", {})
            attrs = {a.get("name"): a.get("value") for a in biosample.get("attributes", [])}
            rows.append({
                "assembly_accession": r.get("accession"),
                "source_database": r.get("source_database"),
                "paired_accession": r.get("paired_accession"),
                "taxid": r.get("organism", {}).get("tax_id"),
                "organism_name": r.get("organism", {}).get("organism_name"),
                "assembly_level": asm_info.get("assembly_level"),
                "release_date": asm_info.get("release_date"),
                "bioproject": asm_info.get("bioproject_accession"),
                "biosample": biosample.get("accession"),
                "collection_date": attrs.get("collection_date"),
                "geo_loc_name": attrs.get("geo_loc_name"),
                "host": attrs.get("host"),
                "isolation_source": attrs.get("isolation_source"),
                "serovar_or_strain": attrs.get("serovar") or attrs.get("strain"),
            })

        page_token = payload.get("next_page_token") or _nested(payload, "pagination", "next_page_token")
        if not page_token:
            break
        time.sleep(0.35)

    if not rows:
        print(f"     [!] No bacterial assemblies retrieved for TaxID {taxid}")
        return 0

    df = pd.DataFrame(rows).drop_duplicates(subset=["assembly_accession"])
    df.to_csv(out_path, sep="\t", index=False)
    print(f"     [OK] {len(df):,} unique assemblies -> {out_path.name}")
    return len(df)


def fetch_opendengue_national(out_path: Path) -> bool:
    raw_url = "https://raw.githubusercontent.com/OpenDengue/master-repo/main/data/releases/V1.3/National_extract_V1_3.csv"
    print("  -> Downloading OpenDengue National Extract CSV...")
    try:
        resp = requests.get(raw_url, headers=HEADERS, timeout=120)
        resp.raise_for_status()
        out_path.write_bytes(resp.content)
        print(f"     [OK] Saved -> {out_path.name}")
        return True
    except Exception as exc:
        print(f"     [!] OpenDengue fetch failed: {exc}")
        return False


def main():
    print(f"=== Starting Pilot Metadata Extraction (Snapshot: {SNAPSHOT_DATE}) ===")
    print(f"=== Project Root: {PROJECT_ROOT} ===")
    print(f"=== Output Dir  : {BASE_RAW_DIR} ===\n")
    audit_summary = []

    ena_seq_fields = [
        "accession", "version", "tax_id", "scientific_name", "collection_date",
        "first_public", "last_updated", "country", "location", "host", "isolate",
        "strain", "sample_accession", "study_accession", "sequence_md5",
        "base_count", "dataclass"
    ]
    ena_asm_fields = [
        "accession", "version", "tax_id", "scientific_name", "collection_date",
        "first_public", "last_updated", "country", "location", "host", "isolate",
        "strain", "sample_accession", "study_accession", "assembly_level", "base_count"
    ]
    ena_read_fields = [
        "run_accession", "experiment_accession", "sample_accession", "study_accession",
        "tax_id", "scientific_name", "collection_date", "first_public", "country",
        "location", "host", "isolate", "strain", "library_strategy",
        "library_source", "instrument_platform"
    ]

    for key in ["lassa", "dengue"]:
        info = PILOT_TAXA[key]
        taxid = info["taxid"]
        print(f"[{info['name']} | TaxID {taxid}]")
        print(f"  -> Scope: {info['scope_note']}")

        n_ena_seq = fetch_ena_tsv(
            "sequence", taxid, ena_seq_fields,
            VIRAL_DIR / f"ena_seq_{key}_{SNAPSHOT_DATE}.tsv"
        )
        n_ncbi_vir = fetch_ncbi_virus_metadata(
            taxid,
            VIRAL_DIR / f"ncbi_virus_{key}_{SNAPSHOT_DATE}.tsv"
        )
        n_ena_reads = fetch_ena_tsv(
            "read_run", taxid, ena_read_fields,
            READS_DIR / f"ena_sra_readruns_{key}_{SNAPSHOT_DATE}.tsv"
        )

        audit_summary.append({
            "pathogen": info["name"],
            "taxid": taxid,
            "taxonomic_scope": info["scope_note"],
            "domain": info["domain"],
            "ena_sequences_count": n_ena_seq,
            "ncbi_records_count": n_ncbi_vir,
            "raw_read_runs_count": n_ena_reads,
            "status": "Success" if min(n_ena_seq, n_ncbi_vir, n_ena_reads) > 0 else "Partial/Failed",
        })
        print()

    kp = PILOT_TAXA["kpneumoniae"]
    print(f"[{kp['name']} | TaxID {kp['taxid']}]")
    print(f"  -> Scope: {kp['scope_note']}")

    n_ena_asm = fetch_ena_tsv(
        "assembly", kp["taxid"], ena_asm_fields,
        BACT_DIR / f"ena_assembly_kpneumoniae_{SNAPSHOT_DATE}.tsv"
    )
    n_ncbi_asm = fetch_ncbi_bacterial_assemblies(
        kp["taxid"],
        BACT_DIR / f"ncbi_assembly_kpneumoniae_{SNAPSHOT_DATE}.tsv"
    )
    n_kp_reads = fetch_ena_tsv(
        "read_run", kp["taxid"], ena_read_fields,
        READS_DIR / f"ena_sra_readruns_kpneumoniae_{SNAPSHOT_DATE}.tsv"
    )

    audit_summary.append({
        "pathogen": kp["name"],
        "taxid": kp["taxid"],
        "taxonomic_scope": kp["scope_note"],
        "domain": kp["domain"],
        "ena_sequences_count": n_ena_asm,
        "ncbi_records_count": n_ncbi_asm,
        "raw_read_runs_count": n_kp_reads,
        "status": "Success" if min(n_ena_asm, n_ncbi_asm, n_kp_reads) > 0 else "Partial/Failed",
    })

    print("\n[Disease Burden Sources]")
    fetch_opendengue_national(BURDEN_DIR / f"opendengue_national_{SNAPSHOT_DATE}.csv")

    audit_df = pd.DataFrame(audit_summary)
    audit_path = BASE_RAW_DIR / f"extraction_audit_log_{SNAPSHOT_DATE}.csv"
    audit_df.to_csv(audit_path, index=False)

    print("\n" + "=" * 70)
    print("                  EXTRACTION AUDIT LOG SUMMARY")
    print("=" * 70)
    for _, row in audit_df.iterrows():
        print(f"Pathogen : {row['pathogen']} (TaxID: {row['taxid']})")
        print(f"  -> Taxonomic scope             : {row['taxonomic_scope']}")
        print(f"  -> ENA records retrieved       : {row['ena_sequences_count']:,}")
        print(f"  -> NCBI records retrieved      : {row['ncbi_records_count']:,}")
        print(f"  -> Raw-read runs retrieved     : {row['raw_read_runs_count']:,}")
        print(f"  -> Endpoint status             : [{row['status']}]")
        print("-" * 70)

    print(f"\n[OK] Detailed audit log saved to:\n     {audit_path}")


if __name__ == "__main__":
    main()
