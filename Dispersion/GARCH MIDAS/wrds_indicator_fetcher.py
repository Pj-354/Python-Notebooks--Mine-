wrds_indicator_fetcher.py  ────────────────────────────────────────────────
A lightweight utility for pulling a curated set of macro‑finance indicators
from WRDS and saving a tidy DataFrame for downstream analysis of systematic
and idiosyncratic equity drivers.

⚑  REQUIREMENTS
    pip install pandas wrds tqdm python-dateutil

🔧  QUICK START
    $ python wrds_indicator_fetcher.py --start 2000-01-01 --end 2025-05-15 \
          --indicators TERM_SPREAD VIX HY_OAS EPU MOVE \
          --out indicators.feather

    import pandas as pd
    df = pd.read_feather("indicators.feather").set_index("date")

The script logs in with your WRDS username/password (env vars work),
performs one query per physical table (to stay within WRDS query limits),
computes any synthetic series (spreads, realised vols, etc.), and writes the
final panel.
"""

import argparse
import datetime as dt
import os
from typing import Dict, List, Tuple

import pandas as pd
import wrds
from dateutil import parser as dtparser
from tqdm import tqdm


###############################################################################
# ────────────────────────────  CONFIGURATION  ───────────────────────────────
###############################################################################

# Mapping of *logical* indicator keys → instructions for building them.
# Each entry is either a "direct" series (pull a single column) or a
# synthetic series (expr) built from previously‑pulled components.
#
# ▸  schema, table  → WRDS location (schema.table)
# ▸  column        → actual column in table to fetch
# ▸  expr          → pandas expression evaluated after merges
# ▸  depends       → other keys that must be fetched first
# ▸  freq          → 'D' (daily), 'M' (monthly), 'Q' (quarterly)
#
# You can easily extend this list with further variables.
INDICATORS: Dict[str, Dict] = {
    # ─── Term structure ─────────────────────────────────────────────────────
    "DGS3":     dict(schema="fred", table="fred_md", column="dgs3",  freq="D",
                      desc="3‑month U.S. Treasury yield (%)"),
    "DGS10":    dict(schema="fred", table="fred_md", column="dgs10", freq="D",
                      desc="10‑year U.S. Treasury yield (%)"),
    "TERM_SPREAD": dict(expr="DGS10 - DGS3", depends=["DGS10", "DGS3"],
                         desc="10Y minus 3M Treasury term spread (bp)"),

    # ─── Credit risk ────────────────────────────────────────────────────────
    "BAA":      dict(schema="fred", table="fred_md", column="baa",   freq="D"),
    "AAA":      dict(schema="fred", table="fred_md", column="aaa",   freq="D"),
    "IG_SPREAD":    dict(expr="BAA - AAA", depends=["BAA", "AAA"],
                          desc="Baa‑minus‑Aaa IG credit spread (bp)"),
    "HY_OAS":   dict(schema="ice",  table="us_corpmaster", column="bamlh0a0hym2", freq="D",
                      desc="ICE/BofA US High‑Yield Option‑Adj. Spread (bp)"),
    "DEFAULT_RATE": dict(schema="moodys", table="drsvd", column="default_rate", freq="M",
                          desc="Moody's 12‑m spec‑grade default rate (%)"),

    # ─── Volatility & risk aversion ─────────────────────────────────────────
    "VIX":      dict(schema="cboe", table="daily_indices", column="vixcls", freq="D",
                      desc="CBOE VIX close (%%)"),
    "VVIX":     dict(schema="cboe", table="daily_indices", column="vvix",   freq="D"),
    "SKEW":     dict(schema="cboe", table="daily_indices", column="skew",   freq="D"),
    "MOVE":     dict(schema="ice",  table="move_index",  column="move",   freq="D",
                      desc="ICE/BofA MOVE Treasury volatility index"),

    # ─── Macro real activity ────────────────────────────────────────────────
    "INDPRO":   dict(schema="fred", table="fred_md", column="indpro", freq="M"),
    "HOUSING_STARTS": dict(schema="fred", table="fred_md", column="houst", freq="M"),
    "NFP":      dict(schema="fred", table="fred_md", column="payems", freq="M"),

    # ─── Sentiment / uncertainty ────────────────────────────────────────────
    "EPU":      dict(schema="policy_uncertainty", table="daily", column="us_epu", freq="D",
                      desc="Baker‑Bloom‑Davis US Economic Policy Uncertainty index"),

    # Add more…
}

###############################################################################
# ──────────────────────────────  FUNCTIONS  ─────────────────────────────────
###############################################################################

def parse_datestr(s: str) -> dt.date:
    """Convert YYYY‑MM‑DD or similar into datetime.date"""
    return dtparser.parse(s).date()


def wrds_connect() -> wrds.Connection:
    """Wrapper that prints a friendly message and returns a WRDS connection."""
    print("Connecting to WRDS… (set WRDS_USERNAME / WRDS_PASSWORD env vars to skip prompts)")
    return wrds.Connection()


def build_table_map(keys: List[str]) -> Dict[Tuple[str, str], List[str]]:
    """Return dict mapping (schema, table) → list[columns] needed."""
    out: Dict[Tuple[str, str], List[str]] = {}
    for k in keys:
        meta = INDICATORS[k]
        if "column" in meta:  # direct retrieval
            out.setdefault((meta["schema"], meta["table"]), []).append(meta["column"])
        elif "depends" in meta:  # synthetic, gather dependencies
            for dep in meta["depends"]:
                dep_meta = INDICATORS[dep]
                out.setdefault((dep_meta["schema"], dep_meta["table"]), []).append(dep_meta["column"])
    return out


def fetch_table(conn: wrds.Connection, schema: str, table: str, columns: List[str],
                start: dt.date, end: dt.date) -> pd.DataFrame:
    """Pull `columns` from a single WRDS table between start and end dates."""
    cols_sql = ", ".join({"date"}.union(columns))
    sql = (
        f"SELECT date, {cols_sql} \n"
        f"FROM {schema}.{table} \n"
        f"WHERE date BETWEEN '{start}' AND '{end}'"
    )
    return conn.raw_sql(sql, date_cols=["date"]).set_index("date")


def merge_data(frames: List[pd.DataFrame]) -> pd.DataFrame:
    """Outer‑join on index date."""
    out = pd.concat(frames, axis=1, join="outer")
    out.sort_index(inplace=True)
    return out


def compute_synthetic(df: pd.DataFrame, keys: List[str]) -> pd.DataFrame:
    for k in keys:
        meta = INDICATORS[k]
        if "expr" in meta:
            df[k.lower()] = df.eval(meta["expr"].lower())
    return df


def filter_columns(df: pd.DataFrame, keys: List[str]) -> pd.DataFrame:
    wanted = []
    for k in keys:
        meta = INDICATORS[k]
        if "column" in meta:
            wanted.append(meta["column"].lower())
        else:
            wanted.append(k.lower())
    return df[wanted]


def main():
    parser = argparse.ArgumentParser(description="Fetch macro/finance indicators from WRDS.")
    parser.add_argument("--start", default="1990-01-01", help="Start date (YYYY-MM-DD)")
    parser.add_argument("--end",   default=dt.date.today().isoformat(), help="End date (YYYY-MM-DD)")
    parser.add_argument("--indicators", nargs="*", default=["TERM_SPREAD", "HY_OAS", "VIX"],
                        help="List of indicator keys to pull (default: %(default)s)")
    parser.add_argument("--out", default="indicators.feather", help="Output Feather/Parquet/CSV file")

    args = parser.parse_args()
    start, end = parse_datestr(args.start), parse_datestr(args.end)
    keys = args.indicators

    # Validate keys
    unknown = set(keys) - INDICATORS.keys()
    if unknown:
        raise ValueError(f"Unknown indicator keys: {', '.join(unknown)}")

    conn = wrds_connect()
    table_map = build_table_map(keys)

    frames = []
    for (schema, table), cols in tqdm(table_map.items(), desc="Tables"):
        frames.append(fetch_table(conn, schema, table, cols, start, end))

    df = merge_data(frames)
    df = compute_synthetic(df, keys)
    df = filter_columns(df, keys)

    # Persist
    out_path = args.out
    ext = os.path.splitext(out_path)[1].lower()
    if ext in {".feather", ".ft"}:
        df.reset_index().to_feather(out_path)
    elif ext in {".parquet", ".pq"}:
        df.reset_index().to_parquet(out_path)
    else:
        df.to_csv(out_path, index=True)

    print(f"Saved → {out_path}  ({len(df):,} rows × {len(df.columns)} cols)")


if __name__ == "__main__":
    main()
