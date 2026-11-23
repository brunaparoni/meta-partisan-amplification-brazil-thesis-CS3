#!/usr/bin/env python3
"""
Brazilian URL Shares Dataset - Per-URL Engagement Extraction
=============================================================

Pulls aggregated engagement metrics (views, clicks, shares, reactions, etc.)
from the URL Breakdowns table (erc_condor_url_breakdowns_dp_clean_partitioned_v2)
for the Brazilian url_rids in our cluster CSV, restricted to the three months
leading up to each Brazilian general election (plus the election month):

  - 2018 election (Oct 7) : year_month in 2018-07, 2018-08, 2018-09, 2018-10
  - 2022 election (Oct 2) : year_month in 2022-07, 2022-08, 2022-09, 2022-10

Each URL's engagement is summed in SQL across all breakdown dimensions
(age_bracket * gender * political_page_affinity * year_month), producing
exactly ONE ROW PER url_rid per chunk file. The script keeps:
    - url_total_rows      : number of breakdown cells summed (needed for DP SE)
    - months_active       : count of distinct year_months the URL was present
                            in within the 4-month window

Why we keep url_total_rows:
    The breakdown cells have mean-zero Gaussian noise added (Evans & King 2022,
    Messing et al. 2020). When we sum N noisy cells, the URL-level total is
    UNBIASED but has variance N * sigma_action^2. The downstream R script
    uses N (= url_total_rows) and the per-action sigma values listed in
    Evans & King (S_views=2228, S_clicks=40, S_shares=14, S_likes=22,
    S_other=10) to compute correct DP-aware standard errors via the
    PrivacyUnbiased framework.

Why no SNR / PPA filter:
    political_page_affinity is US-only per the codebook, so we cannot apply
    the SNR-based filters from the US pipeline. Inclusion is governed only
    by the cluster CSV from script 01.

Pipeline position:
    01_load_split_clusters_BR.R   (R)         -> data/urlrids_BR_clusters_split.csv
--> 02_extract_engagement_BR.py   (this file) -> chunks_BR/chunk_YYYY_NNN.tsv
    03_combine_aggregate_BR.R (next)      -> data/cluster_engagement_BR.csv

Usage:
    Run all cells in a Meta SRE Jupyter notebook, or:
        python 02_extract_engagement_BR.py
"""

import logging
from pathlib import Path
from typing import List, Tuple, Dict

import pandas as pd

# SRE-only import; guarded so the script can be syntax-checked locally.
try:
    from fbri.private.sql.query import execute
except ImportError:  # pragma: no cover
    execute = None
    print("[warn] fbri.private.sql.query not available - running outside SRE.")

# =============================================================================
# CONFIGURATION
# =============================================================================

# Database / table
DATABASE = "fbri_prod_private"
BREAKDOWNS_TABLE = "erc_condor_url_breakdowns_dp_clean_partitioned_v2"

# Country
COUNTRY = "BR"

# Three months pre-election + election month, for each Brazilian election.
# Brazilian first-round dates: 2018-10-07 (Bolsonaro vs Haddad)
#                              2022-10-02 (Lula vs Bolsonaro)
ELECTION_WINDOWS: Dict[int, Tuple[str, ...]] = {
    2018: ("2018-07", "2018-08", "2018-09", "2018-10"),
    2022: ("2022-07", "2022-08", "2022-09", "2022-10"),
}

# Chunking
CHUNK_SIZE = 10_000

# Paths
INPUT_FILE = "./data/urlrids_BR_clusters_split.csv"
OUTPUT_DIR = Path("./chunks_BR")

# Logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger(__name__)


# =============================================================================
# 1. DATA LOADING
# =============================================================================

def load_url_rids_by_year(input_file: str) -> Dict[int, List[str]]:
    """
    Load the cluster CSV produced by script 01 and return a dict mapping
    election_year -> sorted list of unique url_rids assigned to that year.

    A URL appearing in both 2018 and 2022 clusters (rare but possible) will
    be queried twice, once per window, producing one row per (url_rid, year)
    in the output chunks. That is the desired behaviour: the engagement
    totals belong to different election contexts.
    """
    logger.info(f"Loading URL identifiers from {input_file}")
    df = pd.read_csv(
        input_file,
        dtype={"url_rid": str, "election_year": "Int64", "cluster_id": "Int64"},
    )
    logger.info(f"  Loaded {len(df):,} (url_rid, year_cluster) rows")

    by_year: Dict[int, List[str]] = {}
    for year in sorted(df["election_year"].dropna().unique()):
        year_int = int(year)
        urls = sorted(df.loc[df["election_year"] == year_int, "url_rid"].unique())
        by_year[year_int] = urls
        logger.info(f"  {year_int}: {len(urls):,} unique url_rids")

    return by_year


# =============================================================================
# 2. CHUNKING
# =============================================================================

def create_chunks(url_list: List[str], chunk_size: int) -> List[List[str]]:
    """Split a URL list into approximately equal-sized chunks for SQL IN()."""
    chunks = [url_list[i : i + chunk_size] for i in range(0, len(url_list), chunk_size)]
    logger.info(f"  Created {len(chunks)} chunks of size up to {chunk_size:,}")
    return chunks


# =============================================================================
# 3. SQL QUERY CONSTRUCTION
# =============================================================================

def build_aggregation_query(url_chunk: List[str], year_months: Tuple[str, ...]) -> str:
    """
    Build a SQL query that aggregates one chunk of URLs to URL-level totals
    over the given 4-month window.

    Aggregation rule:
        For each url_rid, sum each engagement count across all (year_month,
        age_bracket, gender, political_page_affinity) cells in the window.
        The result is ONE ROW PER url_rid.

    DP note:
        Each individual cell value carries mean-zero Gaussian noise. The sum
        of N cells has expected value equal to the true sum and variance
        N * sigma_action^2. We therefore also return COUNT(*) as
        url_total_rows so the downstream R script can compute correct SEs.
    """
    url_list_sql = ", ".join(f"'{u}'" for u in url_chunk)
    months_sql = ", ".join(f"'{m}'" for m in year_months)

    query = f"""
        SELECT
            url_rid,
            SUM(views)                AS url_views,
            SUM(clicks)               AS url_clicks,
            SUM(shares)               AS url_shares,
            SUM(share_without_clicks) AS url_share_without_clicks,
            SUM(comments)             AS url_comments,
            SUM(likes)                AS url_likes,
            SUM(loves)                AS url_loves,
            SUM(hahas)                AS url_hahas,
            SUM(wows)                 AS url_wows,
            SUM(sorrys)               AS url_sorrys,
            SUM(angers)               AS url_angers,
            COUNT(*)                  AS url_total_rows,
            COUNT(DISTINCT year_month) AS months_active
        FROM {DATABASE}.{BREAKDOWNS_TABLE}
        WHERE c = '{COUNTRY}'
          AND year_month IN ({months_sql})
          AND url_rid IN ({url_list_sql})
        GROUP BY url_rid
    """
    return query


# =============================================================================
# 4. CHUNK PROCESSING
# =============================================================================

def process_year(year: int, url_list: List[str], year_months: Tuple[str, ...]) -> int:
    """
    Run the SQL aggregation chunk-by-chunk for a single election year.
    Writes one TSV per chunk to OUTPUT_DIR. Returns the number of failed chunks.
    """
    logger.info("=" * 70)
    logger.info(f" Year {year}: {len(url_list):,} URLs | window: {', '.join(year_months)}")
    logger.info("=" * 70)

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    chunks = create_chunks(url_list, CHUNK_SIZE)

    n_success = 0
    n_failed = 0
    n_skipped = 0

    for i, chunk in enumerate(chunks):
        chunk_file = OUTPUT_DIR / f"chunk_{year}_{i:03d}.tsv"

        # Resume-friendly: skip chunks that already exist.
        if chunk_file.exists():
            logger.info(f"  [skip ] {chunk_file.name} already exists")
            n_skipped += 1
            continue

        logger.info(f"  [chunk {i + 1:>3}/{len(chunks)}] {chunk_file.name} "
                    f"({len(chunk):,} URLs)")

        try:
            query = build_aggregation_query(chunk, year_months)
            # SRE convention: execute(sql, output_path) writes a TSV to disk.
            execute(query, str(chunk_file))

            # Quick post-hoc row count for logging (cheap local read, no re-query).
            with open(chunk_file, "r") as f:
                n_rows = max(sum(1 for _ in f) - 1, 0)  # subtract header
            logger.info(f"           -> wrote {n_rows:,} URL-level rows")
            n_success += 1
        except Exception as exc:  # noqa: BLE001
            logger.error(f"           -> FAILED: {exc}")
            n_failed += 1

    logger.info(
        f"Year {year} done | success={n_success} skipped={n_skipped} failed={n_failed}"
    )
    return n_failed


# =============================================================================
# 5. MAIN
# =============================================================================

def main() -> int:
    logger.info("#" * 70)
    logger.info(" BR URL Engagement Extraction")
    logger.info(" 3-month pre-election window per election year (Jul-Oct)")
    logger.info("#" * 70)

    by_year = load_url_rids_by_year(INPUT_FILE)

    total_failed = 0
    for year, url_list in by_year.items():
        if year not in ELECTION_WINDOWS:
            logger.warning(f"No election window defined for year {year}; skipping")
            continue
        total_failed += process_year(year, url_list, ELECTION_WINDOWS[year])

    if total_failed == 0:
        logger.info("ALL CHUNKS PROCESSED SUCCESSFULLY")
        return 0
    else:
        logger.error(f"COMPLETED WITH {total_failed} FAILED CHUNK(S) - investigate")
        return 1


# =============================================================================
# JUPYTER NOTEBOOK ENTRY POINT
# =============================================================================

if __name__ == "__main__":
    main()
else:
    # Sanity-check banner when imported as a notebook cell.
    print("Loaded 02_extract_engagement_BR. Call main() to run the full pipeline,")
    print("or call process_year(year, url_list, year_months) for a single year.")
