#!/usr/bin/env python3
"""
Create a timestamped JSON coverage report from Cabal/HPCT HTML output.

  cabal test all --enable-coverage
  python3 analyser/analyse_coverage.py            # from project root
  # or
  python3 analyser/analyse_coverage.py /path/to/dist-newstyle

The report lands in analyser/reports/coverage_<YYYYMMDD_HHMMSS>.json
and contains, for every *.hs.html* module page:

    {
      "executed"        : <count of spans class='istickedoff' OR 'tick'>,
      "always_true"     : <count of spans class='tickonlytrue'>,
      "always_false"    : <count of spans class='tickonlyfalse'>,
      "never_executed"  : <count of spans class='nottickedoff' OR 'cross'>,
      "total"           : executed + always_true + always_false + never_executed,
      "coverage_percent": 100 × (executed+always_true+always_false) / total
    }

Overall project coverage is aggregated across all modules.
"""

from __future__ import annotations
import datetime as _dt
import json
import pathlib as _p
import re
import sys
from bs4 import BeautifulSoup as _BS


# -------------------------------------------------------------------------- #
# Constants                                                                  #
# -------------------------------------------------------------------------- #

DIST_ROOT   = _p.Path("dist-newstyle")
MODULE_GLOB = "**/*.hs.html"                 # files like …/Field.M31.hs.html
# NB hpc_index.html lives in the same tree; we ignore those.


# -------------------------------------------------------------------------- #
# Utilities                                                                   #
# -------------------------------------------------------------------------- #

def _parse_module_html(path: _p.Path) -> dict[str, int | float]:
    """
    Count coverage categories inside one module-level *.hs.html* page.
    Works for both “new” (istickedoff/nottickedoff/…) and older
    (tick/partial/cross) class sets.
    """
    soup = _BS(path.read_text("utf-8", errors="ignore"), "html.parser")

    # ----- new hpc-8+ CSS classes ----------------------------------------
    executed        = len(soup.select("span.istickedoff"))   # normal hits
    always_true     = len(soup.select("span.tickonlytrue"))
    always_false    = len(soup.select("span.tickonlyfalse"))
    never_executed  = len(soup.select("span.nottickedoff"))

    # ----- older GHC/HPC (< 9.6) fallback --------------------------------
    executed       += len(soup.select("span.tick"))
    never_executed += len(soup.select("span.cross"))

    total   = executed + always_true + always_false + never_executed
    covered = executed + always_true + always_false
    pct     = (100.0 * covered / total) if total else 0.0

    return {
        "executed": executed,
        "always_true": always_true,
        "always_false": always_false,
        "never_executed": never_executed,
        "total": total,
        "coverage_percent": pct,
    }


def _collect(tree_root: _p.Path) -> dict[str, dict]:
    """
    Walk dist-newstyle looking for *.hs.html* pages and return
    {relative_path → per-module metrics}.
    """
    records: dict[str, dict] = {}
    for mod_html in tree_root.glob(MODULE_GLOB):
        if mod_html.name == "hpc_index.html":          # ignore dashboard pages
            continue
        rel = mod_html.relative_to(tree_root)
        records[str(rel)] = _parse_module_html(mod_html)
    return records


def _build_report(dist_root: _p.Path) -> dict:
    """Aggregate module metrics into a project-wide report dict."""
    files = _collect(dist_root)
    covered_sum = sum(
        m["executed"] + m["always_true"] + m["always_false"] for m in files.values()
    )
    total_sum = sum(m["total"] for m in files.values())
    overall_pct = (100.0 * covered_sum / total_sum) if total_sum else 0.0

    return {
        "generated_at": _dt.datetime.now().isoformat(timespec="seconds"),
        "overall_coverage_percent": overall_pct,
        "files": files,
    }


# -------------------------------------------------------------------------- #
# Entrypoint                                                                  #
# -------------------------------------------------------------------------- #

def main() -> None:
    dist_root = (
        _p.Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else DIST_ROOT
    )
    if not dist_root.exists():
        sys.exit(f"[error] {dist_root} does not exist – run cabal tests first?")

    report = _build_report(dist_root)

    out_dir = _p.Path("/workspace/repo-utils/analyser/reports")
    out_dir.mkdir(parents=True, exist_ok=True)
    stamp = _dt.datetime.now().strftime("%Y%m%d_%H%M%S")
    out_file = out_dir / f"coverage_{stamp}.json"
    out_file.write_text(json.dumps(report, indent=2))

    print(f"[ok] wrote {out_file.relative_to(_p.Path.cwd())}")


if __name__ == "__main__":
    main()
