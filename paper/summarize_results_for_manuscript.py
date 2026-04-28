#!/usr/bin/env python3

from __future__ import annotations

import csv
import statistics as st
from pathlib import Path


def _quantile(values: list[float], p: float) -> float:
    values = sorted(values)
    if not values:
        return float("nan")
    k = (len(values) - 1) * p
    f = int(k)
    c = min(f + 1, len(values) - 1)
    if f == c:
        return values[f]
    return values[f] + (values[c] - values[f]) * (k - f)


def _extract_slug(stem: str, suffix: str) -> tuple[str, str] | None:
    if "_abs_" not in stem or not stem.endswith(suffix):
        return None
    param, rest = stem.split("_abs_", 1)
    slug = rest[: -len(suffix)]
    if not param or not slug:
        return None
    return param, slug


def main() -> None:
    root = Path(__file__).resolve().parent.parent
    outputs = root / "outputs"

    anova: list[tuple[str, str, str, float]] = []
    for path in outputs.glob("*_anova_effect_sizes.csv"):
        stem = path.stem
        parsed = _extract_slug(stem, "_anova_effect_sizes")
        if parsed is None:
            continue
        param, slug = parsed

        with path.open(newline="", encoding="utf-8") as f:
            rdr = csv.DictReader(f)
            for r in rdr:
                term = (r.get("term") or "").strip().lower()
                if term not in {"system", "color"}:
                    continue
                try:
                    eta2 = float((r.get("eta2") or "").strip())
                except Exception:
                    continue
                anova.append((param, slug, term, eta2))

    by_pair: dict[tuple[str, str], dict[str, float]] = {}
    for param, slug, term, eta2 in anova:
        by_pair.setdefault((param, slug), {})[term] = eta2

    wins = {"system": 0, "color": 0, "tie": 0}
    for _, d in by_pair.items():
        if "system" not in d or "color" not in d:
            continue
        if abs(d["system"] - d["color"]) < 1e-12:
            wins["tie"] += 1
        elif d["system"] > d["color"]:
            wins["system"] += 1
        else:
            wins["color"] += 1

    terms: dict[str, list[float]] = {"system": [], "color": []}
    for *_, term, eta2 in anova:
        terms[term].append(eta2)

    lines: list[str] = []
    for term in ("system", "color"):
        xs = terms[term]
        lines.append(
            "ETA2_{term}: n={n} median={med:.3f} q25={q25:.3f} q75={q75:.3f} ge0.5={ge05} ge0.3={ge03}".format(
                term=term,
                n=len(xs),
                med=st.median(xs) if xs else float("nan"),
                q25=_quantile(xs, 0.25),
                q75=_quantile(xs, 0.75),
                ge05=sum(v >= 0.5 for v in xs),
                ge03=sum(v >= 0.3 for v in xs),
            )
        )

    lines.append(
        "ETA2_WINNERS: system={system} color={color} tie={tie} pairs={pairs}".format(
            system=wins["system"],
            color=wins["color"],
            tie=wins["tie"],
            pairs=len(by_pair),
        )
    )

    # Per-parameter ANOVA summaries
    params = sorted({p for p, _, _, _ in anova})
    for param in params:
        sys_vals = [eta2 for p, _, t, eta2 in anova if p == param and t == "system"]
        col_vals = [eta2 for p, _, t, eta2 in anova if p == param and t == "color"]

        pair_wins = {"system": 0, "color": 0, "tie": 0}
        for (p, _slug), d in by_pair.items():
            if p != param:
                continue
            if "system" not in d or "color" not in d:
                continue
            if abs(d["system"] - d["color"]) < 1e-12:
                pair_wins["tie"] += 1
            elif d["system"] > d["color"]:
                pair_wins["system"] += 1
            else:
                pair_wins["color"] += 1

        lines.append(
            "ETA2_BY_PARAM {param}: system_median={sm:.3f} color_median={cm:.3f} system_q25={sq25:.3f} system_q75={sq75:.3f} color_q25={cq25:.3f} color_q75={cq75:.3f} pairs system_win={sw} color_win={cw}"
            .format(
                param=param,
                sm=st.median(sys_vals) if sys_vals else float("nan"),
                cm=st.median(col_vals) if col_vals else float("nan"),
                sq25=_quantile(sys_vals, 0.25),
                sq75=_quantile(sys_vals, 0.75),
                cq25=_quantile(col_vals, 0.25),
                cq75=_quantile(col_vals, 0.75),
                sw=pair_wins["system"],
                cw=pair_wins["color"],
            )
        )

    slopes: list[tuple[str, str, str, float]] = []
    for path in outputs.glob("*_slope_by_system_vs_nm.csv"):
        stem = path.stem
        parsed = _extract_slug(stem, "_slope_by_system_vs_nm")
        if parsed is None:
            continue
        param, slug = parsed

        with path.open(newline="", encoding="utf-8") as f:
            rdr = csv.DictReader(f)
            for r in rdr:
                try:
                    pv = float((r.get("p_value") or "").strip())
                except Exception:
                    continue
                slopes.append((param, slug, (r.get("system") or "").strip(), pv))

    sig = sum(1 for *_, pv in slopes if pv < 0.05)
    lines.append(
        "SLOPES: n={n} sig_p_lt_0.05={sig} pct={pct:.1f}".format(
            n=len(slopes),
            sig=sig,
            pct=(sig / len(slopes) * 100.0) if slopes else 0.0,
        )
    )

    # Per-parameter slope significance rate
    slope_params = sorted({p for p, *_ in slopes})
    for param in slope_params:
        xs = [pv for p, *_rest, pv in slopes if p == param]
        if not xs:
            continue
        s = sum(1 for pv in xs if pv < 0.05)
        lines.append(
            "SLOPES_BY_PARAM {param}: n={n} sig_p_lt_0.05={sig} pct={pct:.1f}".format(
                param=param,
                n=len(xs),
                sig=s,
                pct=s / len(xs) * 100.0,
            )
        )

    # ML top features/shap
    for rel in ("outputs/ml/feature_importance.csv", "outputs/ml/shap_importance.csv"):
        path = root / rel
        if not path.exists():
            continue
        rows: list[tuple[str, float]] = []
        with path.open(newline="", encoding="utf-8") as f:
            rdr = csv.DictReader(f)
            for r in rdr:
                feat = (
                    r.get("feature")
                    or r.get("Feature")
                    or r.get("name")
                    or r.get("Name")
                    or r.get("var")
                    or r.get("Var")
                    or r.get("")
                )
                feat = str(feat) if feat is not None else ""

                num = None
                for k, v in r.items():
                    kl = (k or "").lower()
                    if kl in {"importance", "mean_importance", "value", "mean_abs_shap", "shap", "mean_shap"}:
                        try:
                            num = float(v)
                            break
                        except Exception:
                            pass
                if num is None:
                    for v in r.values():
                        try:
                            num = float(v)
                            break
                        except Exception:
                            continue
                if num is None:
                    continue
                rows.append((feat, float(num)))

        rows.sort(key=lambda x: x[1], reverse=True)
        top = "; ".join([f"{k}={v:.3g}" for k, v in rows[:10]])
        lines.append(f"TOP_{path.name}: {top}")

    out_txt = outputs / "_manuscript_summary.txt"
    out_txt.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(out_txt.as_posix())


if __name__ == "__main__":
    main()
