#!/usr/bin/env python3
import argparse
import json
import re
from collections import defaultdict

LINE = re.compile(r"^(\S+ \S+) UTC (\w+)>\s+\[(.*?)\] \|> (.*)$", re.S)
STEP = re.compile(r"^[A-Za-z]+:[A-Za-z0-9_.]+$")


def pct(values, p):
    s = sorted(values)
    return s[min(len(s) - 1, int(round(p / 100 * (len(s) - 1))))]


def parse(paths, since, until, groups):
    steps = defaultdict(list)
    for path in paths:
        with open(path, errors="replace") as f:
            for raw in f:
                raw = raw.strip()
                if not raw:
                    continue
                if raw.startswith("{"):
                    try:
                        raw = json.loads(raw).get("log", "")
                    except ValueError:
                        continue
                m = LINE.match(raw)
                if not m:
                    continue
                ts, _level, tags, msg = m.groups()
                if (since and ts < since) or (until and ts > until):
                    continue
                tag = tags.split(",")[-1].strip()
                if not STEP.match(tag):
                    continue
                if groups and tag.split(":")[0] not in groups:
                    continue
                try:
                    steps[tag].append(float(msg.strip()))
                except ValueError:
                    continue
    return steps


def main():
    ap = argparse.ArgumentParser(
        description="Per-step latency from withTimeAPI log lines (tag 'group:step', message = milliseconds)."
    )
    ap.add_argument("logs", nargs="+")
    ap.add_argument("-g", "--group", action="append", help="only these groups, e.g. -g rideSearch -g search")
    ap.add_argument("--since", help="skip lines before this timestamp, e.g. '2026-09-16 04:10:00'")
    ap.add_argument("--until", help="skip lines after this timestamp")
    ap.add_argument("--json", help="also write the table as JSON here")
    args = ap.parse_args()

    steps = parse(args.logs, args.since, args.until, args.group)
    if not steps:
        print("no withTimeAPI step lines found (is enableAPILatencyLogging on and the level INFO or lower?)")
        return
    rows = []
    for tag, v in steps.items():
        rows.append(
            {
                "step": tag,
                "n": len(v),
                "mean": sum(v) / len(v),
                "p50": pct(v, 50),
                "p90": pct(v, 90),
                "p99": pct(v, 99),
                "max": max(v),
                "total": sum(v),
            }
        )
    rows.sort(key=lambda r: (r["step"].split(":")[0], -r["total"]))
    print(f"{'step':<58} {'n':>6} {'mean':>8} {'p50':>7} {'p90':>7} {'p99':>7} {'max':>7} {'total s':>8}")
    group = None
    for r in rows:
        g = r["step"].split(":")[0]
        if g != group:
            group = g
            print()
        print(
            f"{r['step'][:58]:<58} {r['n']:>6} {r['mean']:>8.1f} {r['p50']:>7.0f} {r['p90']:>7.0f}"
            f" {r['p99']:>7.0f} {r['max']:>7.0f} {r['total'] / 1000:>8.2f}"
        )
    if args.json:
        with open(args.json, "w") as f:
            json.dump(rows, f, indent=2)


if __name__ == "__main__":
    main()
