#!/usr/bin/env python3

from __future__ import annotations

import argparse
import csv
import re
import statistics
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


@dataclass(frozen=True)
class PlotItem:
    param: str
    slug: str
    kind: str
    filename: str


_PLOT_RE = re.compile(r"^(?P<param>[a-z0-9]+)_abs_(?P<rest>.+)\.pdf$", re.IGNORECASE)
_OUTPUT_RE = re.compile(
    r"^(?P<param>[a-z0-9]+)_abs_(?P<slug>[a-z0-9]+_[a-z0-9]+)_(?P<kind>.+)$", re.IGNORECASE
)
_KNOWN_SUFFIXES = tuple(
    sorted(
        {
            "anova_effect_sizes",
            "anova_main_effects",
            "corr_wavelength",
            "diff_vs_wavelength",
            "effects_by_color",
            "effects_by_system",
            "lm_system_wavelength",
            "optics_vs_tactile",
            "slope_by_system_vs_nm",
            "slopes_by_system",
            "tech_distance_abs_pct",
            "tech_distance_heatmap",
        },
        key=len,
        reverse=True,
    )
)

_MANUSCRIPT_PARAMS = ("ra", "rq", "rsk", "rsm", "rt", "rv", "rz", "rz1max")
_PARAM_LABELS = {
    "ra": r"$R_a$",
    "rq": r"$R_q$",
    "rsk": r"$R_{sk}$",
    "rsm": r"$R_{sm}$",
    "rt": r"$R_t$",
    "rv": r"$R_v$",
    "rz": r"$R_z$",
    "rz1max": r"$R_{z1max}$",
}

_MATERIAL_LABELS = {
    "c45": "C45 steel",
    "14301": "AISI 304 / EN 1.4301",
    "ti6al4v": "Ti6Al4V",
    "al7075": "Al7075",
    "mo58a": "MO58A brass",
    "ellor": "ELLOR graphite",
}

_PROCESS_LABELS = {
    "mr": ("MR", "rough milling"),
    "mf": ("MF", "finish milling"),
    "tr": ("TR", "rough face turning"),
    "tf": ("TF", "finish face turning"),
    "b": ("B", "burnishing after MF"),
    "wedm_r": ("WEDM_R", "rough wire EDM"),
    "wedm_f": ("WEDM_F", "finish wire EDM"),
    "gri": ("GRI", "grinding"),
    "gla": ("GLA", "glass bead blasting"),
    "hon": ("HON", "honing"),
}


def _latex_escape(text: str) -> str:
    return (
        text.replace("\\", "\\textbackslash{}")
        .replace("_", "\\_")
        .replace("%", "\\%")
        .replace("&", "\\&")
        .replace("#", "\\#")
        .replace("$", "\\$")
        .replace("{", "\\{")
        .replace("}", "\\}")
        .replace("~", "\\textasciitilde{}")
        .replace("^", "\\textasciicircum{}")
    )


def _pretty_kind(kind: str) -> str:
    kind = kind.replace("_", " ")
    kind = re.sub(r"\s+", " ", kind).strip()
    return kind


def _material_label(material: str) -> str:
    return _MATERIAL_LABELS.get(material.lower(), material)


def _process_label(process: str) -> str:
    process = process.lower()
    code, description = _PROCESS_LABELS.get(process, (process.upper(), process.replace("_", " ")))
    return description


def _slug_label(slug: str) -> str:
    parts = slug.split("_", 1)
    if len(parts) != 2:
        return slug
    material, process = parts
    return f"{_material_label(material)}; {_process_label(process)}"


def _parse_param_slug_kind(stem: str) -> tuple[str, str, str] | None:
    """Parse filenames like '{param}_abs_{material}_{proc}_{kind}' into components.

    Process identifiers can themselves contain underscores (for example 'wedm_r'), so the
    split is anchored from the known suffixes used by the analysis pipeline rather than by
    a fixed number of slug tokens.
    """
    parts = stem.split("_abs_", 1)
    if len(parts) == 2:
        param = parts[0].lower()
        rest = parts[1]
        rest_lower = rest.lower()
        for suffix in _KNOWN_SUFFIXES:
            marker = f"_{suffix}"
            if rest_lower.endswith(marker):
                slug = rest[: -len(marker)]
                if slug:
                    return (param, slug, suffix)

    m = _OUTPUT_RE.match(stem)
    if m:
        return (m.group("param").lower(), m.group("slug"), m.group("kind"))

    # Fallback for historical plot naming issues: split after 'param_abs_'
    parts = stem.split("_abs_", 1)
    if len(parts) != 2:
        return None
    param = parts[0].lower()
    rest = parts[1]
    tokens = rest.split("_")
    if len(tokens) < 3:
        return None
    slug = "_".join(tokens[:2])
    kind = "_".join(tokens[2:])
    return (param, slug, kind)


def _discover_plot_items(plots_dir: Path) -> list[PlotItem]:
    items: list[PlotItem] = []
    for path in sorted(plots_dir.glob("*.pdf")):
        m = _PLOT_RE.match(path.name)
        if not m:
            continue

        parsed = _parse_param_slug_kind(path.stem)
        if not parsed:
            continue
        param, slug, kind = parsed
        items.append(PlotItem(param=param, slug=slug, kind=kind, filename=path.name))
    return items


def _kind_sort_key(kind: str) -> tuple[int, str]:
    preferred = [
        "optics_vs_tactile",
        "effects_by_system",
        "effects_by_color",
        "diff_vs_wavelength",
        "slope_by_system_vs_nm",
        "slopes_by_system",
        "tech_distance_heatmap",
    ]
    base = kind.lower()
    for idx, prefix in enumerate(preferred):
        if base.startswith(prefix):
            return (idx, base)
    return (len(preferred), base)


def _write_plot_figures_tex(items: list[PlotItem], out_path: Path) -> None:
    # Group by param then slug
    by_param: dict[str, dict[str, list[PlotItem]]] = {}
    for it in items:
        by_param.setdefault(it.param, {}).setdefault(it.slug, []).append(it)

    lines: list[str] = []
    if not by_param:
        lines.append("% No plot PDFs found in ../plots\n")
        out_path.write_text("".join(lines), encoding="utf-8")
        return

    lines.append("% Auto-generated by generate_supplement.py. Do not edit by hand.\n")

    for param in sorted(by_param.keys()):
        lines.append(f"\\section{{Full figure set for { _latex_escape(param) }}}\n")
        for slug in sorted(by_param[param].keys()):
            slug_label = _slug_label(slug)
            lines.append(f"\\subsection{{Material/process: { _latex_escape(slug_label) }}}\n")
            plots = sorted(by_param[param][slug], key=lambda x: _kind_sort_key(x.kind))
            for p in plots:
                caption = f"{_param_label(param)} ({_latex_escape(slug_label)}): {_latex_escape(_pretty_kind(p.kind))}"
                rel = f"../plots/{p.filename}"
                lines.append("\\begin{figure}[p]\\centering\n")
                lines.append(f"\\includegraphics[width=0.95\\linewidth]{{{rel}}}\n")
                lines.append(f"\\caption{{{caption}}}\n")
                lines.append("\\end{figure}\n\\clearpage\n\n")

    out_path.write_text("".join(lines), encoding="utf-8")


def _discover_ml_images(ml_dir: Path) -> list[Path]:
    if not ml_dir.exists():
        return []
    paths: list[Path] = []
    for ext in ("*.pdf", "*.png", "*.jpg", "*.jpeg"):
        paths.extend(ml_dir.rglob(ext))
    # Keep only files (skip dirs), sorted for stable output
    paths = [p for p in paths if p.is_file()]
    return sorted(paths, key=lambda p: (str(p.parent).lower(), p.name.lower()))


def _write_ml_figures_tex(paths: list[Path], out_path: Path, root_dir: Path) -> None:
    lines: list[str] = []
    if not paths:
        lines.append("% No ML figures found in ../outputs/ml\n")
        out_path.write_text("".join(lines), encoding="utf-8")
        return

    lines.append("% Auto-generated by generate_supplement.py. Do not edit by hand.\n")
    lines.append("\\section{Machine learning diagnostics}\n")

    # Group by parent folder relative to outputs/ml
    base = root_dir / "outputs" / "ml"
    by_folder: dict[str, list[Path]] = {}
    for p in paths:
        rel_folder = str(p.parent.relative_to(base)) if p.parent != base else "."
        by_folder.setdefault(rel_folder, []).append(p)

    for folder in sorted(by_folder.keys()):
        title = "ML outputs" if folder == "." else f"ML outputs: {folder}"
        lines.append(f"\\subsection{{{_latex_escape(title)}}}\n")
        for p in by_folder[folder]:
            rel = p.relative_to(root_dir).as_posix()
            caption = _latex_escape(p.name)
            lines.append("\\begin{figure}[p]\\centering\n")
            lines.append(f"\\includegraphics[width=0.95\\linewidth]{{../{rel}}}\n")
            lines.append(f"\\caption{{{caption}}}\n")
            lines.append("\\end{figure}\n\\clearpage\n\n")

    out_path.write_text("".join(lines), encoding="utf-8")


def _iter_csv_rows(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        rows = list(reader)
    if reader.fieldnames and reader.fieldnames[0] == "":
        # Common pattern in our exports where the first column is an unnamed row label.
        for row in rows:
            row["term"] = row.get("", "")
            row.pop("", None)
    return rows


def _format_float(value: str, digits: int = 4) -> str:
    value = (value or "").strip()
    if value in {"", "NA", "NaN", "nan"}:
        return "--"
    try:
        x = float(value)
    except ValueError:
        return _latex_escape(value)

    # Compact formatting: keep scientific if very small.
    if x != 0.0 and (abs(x) < 1e-3 or abs(x) >= 1e4):
        return f"{x:.{digits}e}"
    return f"{x:.{digits}f}".rstrip("0").rstrip(".")


def _parse_float(value: str) -> float | None:
    value = (value or "").strip()
    if value in {"", "NA", "NaN", "nan"}:
        return None
    try:
        return float(value)
    except ValueError:
        return None


def _param_sort_key(param: str) -> tuple[int, str]:
    try:
        return (_MANUSCRIPT_PARAMS.index(param.lower()), param.lower())
    except ValueError:
        return (len(_MANUSCRIPT_PARAMS), param.lower())


def _param_label(param: str) -> str:
    return _PARAM_LABELS.get(param.lower(), _latex_escape(param))


def _format_repeat_count(values: list[int]) -> str:
    if not values:
        return "--"
    unique = sorted(set(values))
    if len(unique) == 1:
        return str(unique[0])
    return f"{unique[0]}--{unique[-1]}"


def _quartile_summary(values: list[float]) -> tuple[float, float, float]:
    if len(values) == 1:
        only = values[0]
        return (only, only, only)
    quartiles = statistics.quantiles(values, n=4, method="inclusive")
    return (statistics.median(values), quartiles[0], quartiles[2])


def _discover_outputs(outputs_dir: Path) -> dict[tuple[str, str], dict[str, Path]]:
    """Map (param, slug) -> kind -> file path."""
    by_key: dict[tuple[str, str], dict[str, Path]] = {}
    for path in sorted(outputs_dir.glob("*.csv")):
        parsed = _parse_param_slug_kind(path.stem)
        if not parsed:
            continue
        param, slug, kind = parsed
        by_key.setdefault((param, slug), {})[kind.lower()] = path
    return by_key


def _write_tables_tex(outputs_dir: Path, out_path: Path) -> None:
    items = _discover_outputs(outputs_dir)

    lines: list[str] = []
    lines.append("% Auto-generated by generate_supplement.py. Do not edit by hand.\n")
    lines.append("\\section{Supplementary tables}\n")
    lines.append(
        "\\noindent The tables below are derived from the processed results produced by the analysis pipeline. "
        "They summarise the ANOVA results, wavelength-screening regression outputs, and descriptive summaries reported in the manuscript. "
        "Archive identifiers in the Material/process column are expanded to full material names and full process descriptions. "
        "The process codes used in the archive are MR (rough milling), MF (finish milling), TR/TF (rough/finish face turning), B (burnishing after MF), WEDM\\_R/WEDM\\_F (rough/finish wire EDM), GRI (grinding), GLA (glass bead blasting), and HON (honing).\n\n"
    )

    # Table S1: ANOVA main effects + eta2
    anova_rows: list[dict[str, str]] = []
    for (param, slug), kinds in items.items():
        main_path = kinds.get("anova_main_effects")
        eta_path = kinds.get("anova_effect_sizes")
        if not main_path or not eta_path:
            continue

        main = _iter_csv_rows(main_path)
        eta = {r.get("term", "").strip().lower(): r.get("eta2", "") for r in _iter_csv_rows(eta_path)}

        df_den = ""
        for r in main:
            if r.get("term", "").strip().lower() == "residuals":
                df_den = (r.get("Df") or "").strip()
                break

        for r in main:
            term = r.get("term", "").strip().lower()
            if term not in {"system", "color"}:
                continue
            anova_rows.append(
                {
                    "param": param,
                    "slug": slug,
                    "term": term,
                    "df_num": (r.get("Df") or "").strip(),
                    "df_den": df_den,
                    "F": (r.get("F value") or "").strip(),
                    "p": (r.get("Pr(>F)") or "").strip(),
                    "eta2": (eta.get(term, "") or "").strip(),
                }
            )

    lines.append("\\subsection{Table S1: Two-way ANOVA main effects and effect sizes}\n")
    if not anova_rows:
        lines.append("% No ANOVA outputs found.\n\n")
    else:
        lines.append("\\small\\setlength\\LTleft{0pt}\\setlength\\LTright{0pt}\n")
        lines.append("\\begin{longtable}{lp{0.28\\linewidth}lrrrrr}\n")
        lines.append(
            "\\caption{Two-way ANOVA for factors system and colour. Reported are numerator/denominator degrees of freedom, $F$ statistic, $p$-value, and $\\eta^2$ effect size.}\\label{tab:supp-anova}\\\\\n"
        )
        lines.append("\\toprule\n")
        lines.append("Parameter & Material/process & Term & $\\mathrm{df}_1$ & $\\mathrm{df}_2$ & $F$ & $p$ & $\\eta^2$\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endfirsthead\n")
        lines.append("\\toprule\n")
        lines.append("Parameter & Material/process & Term & $\\mathrm{df}_1$ & $\\mathrm{df}_2$ & $F$ & $p$ & $\\eta^2$\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endhead\n")
        lines.append("\\midrule\n")
        lines.append("\\multicolumn{8}{r}{\\emph{Continued on next page}}\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endfoot\n")
        lines.append("\\bottomrule\n")
        lines.append("\\endlastfoot\n")

        for r in sorted(anova_rows, key=lambda x: (x["param"], x["slug"], x["term"])):
            lines.append(
                "{} & {} & {} & {} & {} & {} & {} & {}\\\\\n".format(
                    _param_label(r["param"]),
                    _latex_escape(_slug_label(r["slug"])),
                    _latex_escape(r["term"]),
                    _latex_escape(r["df_num"]),
                    _latex_escape(r["df_den"]),
                    _format_float(r["F"], digits=3),
                    _format_float(r["p"], digits=3),
                    _format_float(r["eta2"], digits=3),
                )
            )
        lines.append("\\end{longtable}\n\\normalsize\n\n")

    # Table S2: slopes by system vs wavelength
    slope_rows: list[dict[str, str]] = []
    for (param, slug), kinds in items.items():
        slope_path = kinds.get("slope_by_system_vs_nm")
        if not slope_path:
            continue
        for r in _iter_csv_rows(slope_path):
            slope_rows.append(
                {
                    "param": param,
                    "slug": slug,
                    "system": (r.get("system") or "").strip(),
                    "n_points": (r.get("n_points") or "").strip(),
                    "slope_per_nm": (r.get("slope_per_nm") or "").strip(),
                    "ci_low": (r.get("ci_low") or "").strip(),
                    "ci_high": (r.get("ci_high") or "").strip(),
                    "p": (r.get("p_value") or "").strip(),
                }
            )

    lines.append("\\subsection{Table S2: System-wise slope versus wavelength}\n")
    if not slope_rows:
        lines.append("% No slope outputs found.\n\n")
    else:
        lines.append("\\small\\setlength\\LTleft{0pt}\\setlength\\LTright{0pt}\n")
        lines.append("\\begin{longtable}{lp{0.28\\linewidth}lrrrrr}\n")
        lines.append(
            "\\caption{Linear slope (per nm) of $|\\mathrm{diff}|(\\%)$ versus wavelength, estimated separately for each system. Reported are the number of wavelength levels $n$, 95\\% confidence interval bounds, and $p$-values; these fits should be interpreted as low-degree-of-freedom screening summaries.}\\label{tab:supp-slopes}\\\\\n"
        )
        lines.append("\\toprule\n")
        lines.append("Parameter & Material/process & System & $n$ & Slope/nm & CI low & CI high & $p$\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endfirsthead\n")
        lines.append("\\toprule\n")
        lines.append("Parameter & Material/process & System & $n$ & Slope/nm & CI low & CI high & $p$\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endhead\n")
        lines.append("\\midrule\n")
        lines.append("\\multicolumn{8}{r}{\\emph{Continued on next page}}\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endfoot\n")
        lines.append("\\bottomrule\n")
        lines.append("\\endlastfoot\n")

        for r in sorted(slope_rows, key=lambda x: (x["param"], x["slug"], x["system"])):
            lines.append(
                "{} & {} & {} & {} & {} & {} & {} & {}\\\\\n".format(
                    _param_label(r["param"]),
                    _latex_escape(_slug_label(r["slug"])),
                    _latex_escape(r["system"]),
                    _latex_escape(r["n_points"]),
                    _format_float(r["slope_per_nm"], digits=4),
                    _format_float(r["ci_low"], digits=4),
                    _format_float(r["ci_high"], digits=4),
                    _format_float(r["p"], digits=3),
                )
            )
        lines.append("\\end{longtable}\n\\normalsize\n\n")

    # Table S3: summary by system (mean/sd)
    sys_rows: list[dict[str, str]] = []
    for (param, slug), kinds in items.items():
        sys_path = kinds.get("effects_by_system")
        if not sys_path:
            continue
        for r in _iter_csv_rows(sys_path):
            sys_rows.append(
                {
                    "param": param,
                    "slug": slug,
                    "system": (r.get("system") or "").strip(),
                    "n": (r.get("n") or "").strip(),
                    "mean": (r.get("mean") or "").strip(),
                    "sd": (r.get("sd") or "").strip(),
                    "se": (r.get("se") or "").strip(),
                }
            )

    lines.append("\\subsection{Table S3: Descriptive statistics by system}\n")
    if not sys_rows:
        lines.append("% No effects-by-system outputs found.\n\n")
    else:
        lines.append("\\small\\setlength\\LTleft{0pt}\\setlength\\LTright{0pt}\n")
        lines.append("\\begin{longtable}{lp{0.28\\linewidth}lrrrr}\n")
        lines.append(
            "\\caption{Per-system descriptive statistics for $|\\mathrm{diff}|(\\%)$ within each parameter and material/process group.}\\label{tab:supp-system-stats}\\\\\n"
        )
        lines.append("\\toprule\n")
        lines.append("Parameter & Material/process & System & $n$ & Mean & SD & SE\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endfirsthead\n")
        lines.append("\\toprule\n")
        lines.append("Parameter & Material/process & System & $n$ & Mean & SD & SE\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endhead\n")
        lines.append("\\midrule\n")
        lines.append("\\multicolumn{7}{r}{\\emph{Continued on next page}}\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endfoot\n")
        lines.append("\\bottomrule\n")
        lines.append("\\endlastfoot\n")

        for r in sorted(sys_rows, key=lambda x: (x["param"], x["slug"], x["system"])):
            lines.append(
                "{} & {} & {} & {} & {} & {} & {}\\\\\n".format(
                    _param_label(r["param"]),
                    _latex_escape(_slug_label(r["slug"])),
                    _latex_escape(r["system"]),
                    _latex_escape(r["n"]),
                    _format_float(r["mean"], digits=4),
                    _format_float(r["sd"], digits=4),
                    _format_float(r["se"], digits=4),
                )
            )
        lines.append("\\end{longtable}\n\\normalsize\n\n")

    # Table S4: tactile repeatability summary by parameter
    tactile_rows: dict[str, list[dict[str, float | int]]] = {}
    for (param, _slug), kinds in items.items():
        if param not in _MANUSCRIPT_PARAMS:
            continue
        tactile_path = kinds.get("optics_vs_tactile")
        if not tactile_path:
            continue

        raw_rows = _iter_csv_rows(tactile_path)
        if not raw_rows:
            continue

        first = raw_rows[0]
        reference = _parse_float(first.get("tactile_reference") or "")
        n_value = _parse_float(first.get("tactile_n") or "")
        se_value = _parse_float(first.get("tactile_se") or "")
        if reference is None or n_value is None or se_value is None:
            continue

        tactile_rows.setdefault(param, []).append(
            {
                "reference": reference,
                "n": int(round(n_value)),
                "se": se_value,
            }
        )

    lines.append("\\subsection{Table S4: Internal tactile repeatability summary}\n")
    if not tactile_rows:
        lines.append("% No tactile repeatability summaries found.\n\n")
    else:
        lines.append("\\small\\setlength\\LTleft{0pt}\\setlength\\LTright{0pt}\n")
        lines.append("\\begin{longtable}{lrrl}\n")
        lines.append(
            "\\caption{Internal repeatability summary of the tactile reference across the eight-parameter manuscript subset. Each row aggregates one archived tactile descriptor per parameter--surface group from the corresponding optics-vs-tactile export. For strictly positive parameters, the reported summary is the relative standard error of the tactile mean, $100\\,u_S/|S|$, in percent; for $R_{sk}$, the reported summary is $u_S$ in native parameter units because relative normalisation becomes unstable near zero.}\\label{tab:supp-tactile-repeatability}\\\\\n"
        )
        lines.append("\\toprule\n")
        lines.append("Parameter & Groups & $n_S$ & Median [Q1, Q3]\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endfirsthead\n")
        lines.append("\\toprule\n")
        lines.append("Parameter & Groups & $n_S$ & Median [Q1, Q3]\\\\\n")
        lines.append("\\midrule\n")
        lines.append("\\endhead\n")
        lines.append("\\bottomrule\n")
        lines.append("\\endlastfoot\n")

        for param in sorted(tactile_rows.keys(), key=_param_sort_key):
            entries = tactile_rows[param]
            repeat_counts = [int(entry["n"]) for entry in entries]
            if param == "rsk":
                se_values = [float(entry["se"]) for entry in entries]
                median, q1, q3 = _quartile_summary(se_values)
                summary = "{} [{}, {}] ($u_S$)".format(
                    _format_float(str(median), digits=4),
                    _format_float(str(q1), digits=4),
                    _format_float(str(q3), digits=4),
                )
            else:
                relative_se = [100.0 * float(entry["se"]) / abs(float(entry["reference"])) for entry in entries if float(entry["reference"]) != 0.0]
                if not relative_se:
                    summary = "--"
                else:
                    median, q1, q3 = _quartile_summary(relative_se)
                    summary = "{} [{}, {}]\\%".format(
                        _format_float(str(median), digits=2),
                        _format_float(str(q1), digits=2),
                        _format_float(str(q3), digits=2),
                    )

            lines.append(
                "{} & {} & {} & {}\\\\\n".format(
                    _param_label(param),
                    len(entries),
                    _format_repeat_count(repeat_counts),
                    summary,
                )
            )

        lines.append("\\end{longtable}\n\\normalsize\n\n")

    out_path.write_text("".join(lines), encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description="Generate LaTeX include files for the supplement.")
    parser.add_argument(
        "--include-plots",
        action="store_true",
        help="Generate supplement_figures.tex by scanning ../plots (disabled by default).",
    )
    parser.add_argument(
        "--include-ml",
        action="store_true",
        help="Generate supplement_ml_figures.tex by scanning ../outputs/ml (disabled by default).",
    )
    args = parser.parse_args()

    paper_dir = Path(__file__).resolve().parent
    root_dir = paper_dir.parent

    plots_dir = root_dir / "plots"
    ml_dir = root_dir / "outputs" / "ml"
    outputs_dir = root_dir / "outputs"

    figures_tex = paper_dir / "supplement_figures.tex"
    ml_tex = paper_dir / "supplement_ml_figures.tex"
    tables_tex = paper_dir / "supplement_tables.tex"

    _write_tables_tex(outputs_dir, tables_tex)
    print(f"[generate_supplement] Wrote {tables_tex.name} (tables-first supplement)")

    if args.include_plots:
        plot_items = _discover_plot_items(plots_dir)
        _write_plot_figures_tex(plot_items, figures_tex)
        print(f"[generate_supplement] Wrote {figures_tex.name} with {len(plot_items)} plot figures")

    if args.include_ml:
        ml_paths = _discover_ml_images(ml_dir)
        _write_ml_figures_tex(ml_paths, ml_tex, root_dir=root_dir)
        print(f"[generate_supplement] Wrote {ml_tex.name} with {len(ml_paths)} ML figures")


if __name__ == "__main__":
    main()
