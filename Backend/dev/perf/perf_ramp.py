#!/usr/bin/env python3
import argparse
import json
import os
import shutil
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
BACKEND = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, HERE)
from perf_watch import explain, scrape, summarize  # noqa: E402


def find_oha():
    local = os.path.join(BACKEND, "dist-perf-tools", "bin", "oha")
    if os.access(local, os.X_OK):
        return local
    found = shutil.which("oha")
    if found:
        return found
    out = subprocess.run(["bash", os.path.join(HERE, "prof.sh"), "tools"], capture_output=True, text=True)
    if out.returncode != 0:
        sys.exit(f"perf-ramp: could not fetch oha: {out.stderr.strip()}")
    return os.path.join(out.stdout.strip().splitlines()[-1], "oha")


def run_oha(oha, args, conc, duration):
    cmd = [oha, "--no-tui", "-j", "-z", duration, "-c", str(conc), "-m", args.method]
    for h in args.header:
        cmd += ["-H", h]
    if args.data is not None:
        cmd += ["-d", args.data]
    if args.data_file:
        cmd += ["-D", args.data_file]
    if args.content_type:
        cmd += ["-T", args.content_type]
    if args.timeout:
        cmd += ["-t", args.timeout]
    cmd.append(args.url)
    out = subprocess.run(cmd, capture_output=True, text=True)
    if out.returncode != 0:
        sys.exit(f"perf-ramp: oha failed ({out.returncode}): {out.stderr.strip()[:500]}")
    return json.loads(out.stdout)


def safe_scrape(url):
    if not url:
        return None
    try:
        return scrape(url)
    except OSError as e:
        print(f"perf-ramp: metrics scrape failed ({explain(e)}); continuing without server-side numbers", file=sys.stderr)
        return None


def fmt(v, spec):
    return "-" if v is None else format(v, spec)


def level_row(conc, res, server):
    s = res["summary"]
    lp = res.get("latencyPercentiles") or {}
    codes = res.get("statusCodeDistribution") or {}
    total = sum(codes.values())
    non2xx = sum(n for c, n in codes.items() if not c.startswith("2"))
    row = {
        "conc": conc,
        "rps": s.get("requestsPerSec") or 0.0,
        "avg_ms": 1000 * (s.get("average") or 0.0),
        "p50_ms": 1000 * (lp.get("p50") or 0.0),
        "p90_ms": 1000 * (lp.get("p90") or 0.0),
        "p99_ms": 1000 * (lp.get("p99") or 0.0),
        "success_pct": 100 * (s.get("successRate") or 0.0),
        "non2xx": non2xx,
        "responses": total,
        "errors": res.get("errorDistribution") or {},
    }
    if server:
        for k in ("cpu_cores", "gc_wall_pct", "alloc_mb_s", "live_mb", "rss_mb"):
            row[k] = server.get(k)
        row["datastore"] = {f"{a} {b}": v for (a, b), v in server["datastore"].items()}
        row["external"] = {f"{a} {b}": v for (a, b), v in server["external"].items()}
    return row


HEADER = (
    f"{'conc':>5} {'req/s':>9} {'p50 ms':>8} {'p90 ms':>8} {'p99 ms':>8} {'ok%':>6} {'non2xx':>7} |"
    f" {'cpu':>5} {'gc%':>5} {'alloc MB/s':>10} {'live MB':>8}"
)


def print_row(r):
    print(
        f"{r['conc']:>5} {r['rps']:>9.1f} {r['p50_ms']:>8.1f} {r['p90_ms']:>8.1f} {r['p99_ms']:>8.1f}"
        f" {r['success_pct']:>6.1f} {r['non2xx']:>7} | {fmt(r.get('cpu_cores'), '5.2f'):>5}"
        f" {fmt(r.get('gc_wall_pct'), '5.1f'):>5} {fmt(r.get('alloc_mb_s'), '10.0f'):>10} {fmt(r.get('live_mb'), '8.0f'):>8}",
        flush=True,
    )


def breakdown(first, last, key, title):
    a, b = first.get(key) or {}, last.get(key) or {}
    if not b:
        return
    print(f"\n{title} (server side, c={first['conc']} -> c={last['conc']}):")
    print(f"  {'avg ms':>8} -> {'avg ms':>8} {'x':>6} {'calls/s':>8}  op")
    for name, v in sorted(b.items(), key=lambda kv: -kv[1]["sum"])[:8]:
        before = a.get(name, {}).get("avg")
        ratio = v["avg"] / before if before else None
        print(
            f"  {fmt(before and 1000 * before, '8.2f'):>8} -> {1000 * v['avg']:>8.2f} {fmt(ratio, '6.1f'):>6}"
            f" {v['count'] / v['dt']:>8.1f}  {name[:90]}"
        )


def analyse(rows):
    if not rows:
        return
    peak = max(rows, key=lambda r: r["rps"])
    knee = None
    for prev, cur in zip(rows, rows[1:]):
        if cur["rps"] < 1.10 * prev["rps"] and cur["p50_ms"] > 1.5 * prev["p50_ms"]:
            knee = prev
            break
    print("\n== analysis")
    print(f"  throughput ceiling  ~{peak['rps']:.0f} req/s (at c={peak['conc']})")
    if knee:
        print(
            f"  saturation          from c~{knee['conc']} on, extra concurrency no longer buys throughput and"
            " latency grows with it: requests are queueing on a shared resource"
        )
        print(
            f"  Little's law        c = req/s x latency: {knee['conc']} ~ {knee['rps']:.0f} x {knee['avg_ms'] / 1000:.3f}s"
            " - past this point every added client just adds wait time"
        )
    else:
        print("  saturation          not reached; raise the top concurrency level")

    top = rows[-1]
    ncpu = os.cpu_count() or 1
    verdicts = []
    if top.get("cpu_cores") is not None and top["cpu_cores"] >= 0.8 * ncpu:
        verdicts.append(
            f"CPU-bound: the service used {top['cpu_cores']:.1f} of {ncpu} cores (the load generator shares this "
            "machine). Profile with `, prof run <exe> time` to find the hot code."
        )
    if top.get("gc_wall_pct") is not None and top["gc_wall_pct"] >= 15:
        verdicts.append(
            f"GC-bound: the service was stopped for GC {top['gc_wall_pct']:.0f}% of the time at c={top['conc']}."
            " Try +RTS -A64m, and look at the alloc flamegraph (time mode)."
        )
    if not verdicts and top.get("cpu_cores") is not None:
        verdicts.append(
            f"Waiting-bound: at c={top['conc']} the service used only {top['cpu_cores']:.1f} cores and "
            f"{fmt(top.get('gc_wall_pct'), '.0f')}% GC, yet latency grew. Requests wait on something that is not "
            "Haskell CPU; the breakdown below shows which datastore/external calls slowed down. Pools to compare "
            "with the concurrency: POSTGRES_POOL_MAX (default 5) x POSTGRES_POOL_STRIPES (default 1), esqDB "
            "connectionPoolCount (dhall), Redis connectMaxConnections (dhall). `, prof run <exe> rts` shows blocked threads."
        )
    if top["non2xx"] or top["errors"]:
        verdicts.append(f"errors at c={top['conc']}: {top['non2xx']} non-2xx, transport errors {top['errors']}")
    for v in verdicts:
        print(f"  - {v}")
    breakdown(rows[0], top, "datastore", "datastore ops")
    breakdown(rows[0], top, "external", "external calls")


def main():
    ap = argparse.ArgumentParser(
        description="Drive an endpoint at increasing concurrency and show where throughput stops scaling and why."
    )
    ap.add_argument("url")
    ap.add_argument("-H", "--header", action="append", default=[], help="request header, e.g. -H 'token: abc'")
    ap.add_argument("-m", "--method", default="GET")
    ap.add_argument("-d", "--data", help="request body")
    ap.add_argument("-D", "--data-file", help="request body from file")
    ap.add_argument("-T", "--content-type")
    ap.add_argument("-t", "--timeout", help="per-request timeout, e.g. 30s")
    ap.add_argument("-c", "--levels", default="1,2,4,8,16,32,64", help="comma-separated concurrency levels")
    ap.add_argument("-z", "--duration", default="20s", help="time per level")
    ap.add_argument("--warmup", default="5s", help="unrecorded warm-up at the first level ('0' to skip)")
    ap.add_argument("-p", "--metrics-port", type=int, default=9999, help="service metrics port (rider 9999, driver 9997)")
    ap.add_argument("--metrics-url", help="full metrics URL; overrides --metrics-port")
    ap.add_argument("--no-metrics", action="store_true", help="client-side numbers only")
    ap.add_argument("--out", help="directory for results.json (default Backend/perf-runs/ramp/<timestamp>)")
    args = ap.parse_args()

    levels = [int(x) for x in args.levels.split(",") if x.strip()]
    metrics = None if args.no_metrics else (args.metrics_url or f"http://localhost:{args.metrics_port}/metrics")
    oha = find_oha()
    out_dir = args.out or os.path.join(BACKEND, "perf-runs", "ramp", time.strftime("%Y%m%d-%H%M%S"))
    os.makedirs(out_dir, exist_ok=True)

    print(f"perf-ramp: {args.method} {args.url}  levels {levels}  {args.duration} each  metrics {metrics or 'off'}")
    if args.warmup not in ("0", "0s", ""):
        run_oha(oha, args, levels[0], args.warmup)
    print(HEADER)
    rows = []
    for conc in levels:
        before = safe_scrape(metrics)
        res = run_oha(oha, args, conc, args.duration)
        after = safe_scrape(metrics) if before else None
        server = summarize(before[1], after[1], after[0] - before[0]) if before and after else None
        if server:
            for group in ("datastore", "external"):
                for v in server[group].values():
                    v["dt"] = server["dt"]
        row = level_row(conc, res, server)
        rows.append(row)
        print_row(row)
    analyse(rows)
    with open(os.path.join(out_dir, "results.json"), "w") as f:
        json.dump({"url": args.url, "method": args.method, "levels": rows}, f, indent=2, default=str)
    print(f"\nperf-ramp: results in {out_dir}/results.json")


if __name__ == "__main__":
    main()
