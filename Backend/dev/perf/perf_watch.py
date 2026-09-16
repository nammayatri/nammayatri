#!/usr/bin/env python3
import argparse
import math
import re
import sys
import time
import urllib.request
from collections import defaultdict

SAMPLE = re.compile(r"^([a-zA-Z_:][a-zA-Z0-9_:]*)(?:\{(.*)\})?\s+(\S+)")
LABEL = re.compile(r'([a-zA-Z_][a-zA-Z0-9_]*)="((?:[^"\\]|\\.)*)"')
MB = 1024 * 1024


def scrape(url, timeout=5):
    with urllib.request.urlopen(url, timeout=timeout) as resp:
        text = resp.read().decode("utf-8", "replace")
    samples = {}
    for line in text.splitlines():
        if not line or line.startswith("#"):
            continue
        m = SAMPLE.match(line)
        if not m:
            continue
        try:
            value = float(m.group(3))
        except ValueError:
            continue
        labels = tuple(sorted(LABEL.findall(m.group(2) or "")))
        samples[(m.group(1), labels)] = value
    return time.time(), samples


def explain(err):
    if getattr(err, "code", None) == 500:
        return (
            f"{err}. A metrics collector threw while scraping. On macOS this is prometheus-proc (procMetrics, "
            "registered by Kernel.Tools.Metrics.Init.serve) reading the Linux-only /proc/<pid>/stat, which fails "
            "every scrape; on Linux (dev boxes, pods) it works. See Backend/Profiling.md."
        )
    return str(err)


def value(samples, name, default=None):
    return samples.get((name, ()), default)


def delta(a, b, name):
    va, vb = value(a, name), value(b, name)
    if va is None or vb is None:
        return None
    return vb - va


def quantile(buckets, q):
    total = buckets[-1][1] if buckets else 0
    if total <= 0:
        return None
    target = q * total
    prev_le = 0.0
    for le, c in buckets:
        if c >= target:
            return le, prev_le
        prev_le = le
    return math.inf, prev_le


def fmt_bound(b):
    if b is None:
        return "-"
    le, prev = b
    if math.isinf(le):
        return f">{1000 * prev:g}"
    return f"<={1000 * le:g}"


def histograms(a, b, name, key_labels):
    groups = defaultdict(lambda: {"count": 0.0, "sum": 0.0, "buckets": defaultdict(float)})
    for (metric, labels), vb in b.items():
        if not metric.startswith(name):
            continue
        suffix = metric[len(name):]
        if suffix not in ("_count", "_sum", "_bucket"):
            continue
        d = vb - a.get((metric, labels), 0.0)
        if d <= 0:
            continue
        lab = dict(labels)
        key = tuple(lab.get(k, "") for k in key_labels)
        g = groups[key]
        if suffix == "_count":
            g["count"] += d
        elif suffix == "_sum":
            g["sum"] += d
        else:
            g["buckets"][float(lab.get("le", "inf").replace("+Inf", "inf"))] += d
    out = {}
    for key, g in groups.items():
        if g["count"] <= 0:
            continue
        bs = sorted(g["buckets"].items())
        out[key] = {
            "count": g["count"],
            "sum": g["sum"],
            "avg": g["sum"] / g["count"],
            "p95": quantile(bs, 0.95),
            "p99": quantile(bs, 0.99),
        }
    return out


def http_errors(a, b):
    errs = 0.0
    for (metric, labels), vb in b.items():
        if metric == "http_request_duration_seconds_count" and dict(labels).get("status_code", "").startswith("5"):
            errs += vb - a.get((metric, labels), 0.0)
    return errs


def summarize(a, b, dt):
    s = {"dt": dt}
    handlers = histograms(a, b, "http_request_duration_seconds", ("handler",))
    s["handlers"] = handlers
    s["rps"] = sum(h["count"] for h in handlers.values()) / dt
    s["err_rps"] = http_errors(a, b) / dt
    s["datastore"] = histograms(a, b, "datastore_operation_duration", ("datastore", "operation"))
    s["external"] = histograms(a, b, "external_request_duration", ("host", "service"))

    cpu = delta(a, b, "process_cpu_seconds_total")
    if cpu is None:
        cpu = delta(a, b, "ghc_cpu_seconds_total")
    s["cpu_cores"] = cpu / dt if cpu is not None else None
    gc_cpu, all_cpu = delta(a, b, "ghc_gc_cpu_seconds_total"), delta(a, b, "ghc_cpu_seconds_total")
    s["gc_cpu_pct"] = 100 * gc_cpu / all_cpu if gc_cpu is not None and all_cpu else None
    gc_wall, all_wall = delta(a, b, "ghc_gc_elapsed_seconds_total"), delta(a, b, "ghc_elapsed_seconds_total")
    s["gc_wall_pct"] = 100 * gc_wall / all_wall if gc_wall is not None and all_wall else None
    alloc = delta(a, b, "ghc_allocated_bytes_total")
    s["alloc_mb_s"] = alloc / MB / dt if alloc is not None else None
    gcs, majors = delta(a, b, "ghc_gcs_total"), delta(a, b, "ghc_major_gcs_total")
    s["gcs_s"] = gcs / dt if gcs is not None else None
    s["major_gcs_s"] = majors / dt if majors is not None else None
    for key, name in (
        ("live_mb", "ghc_gcdetails_live_bytes"),
        ("max_live_mb", "ghc_max_live_bytes"),
        ("heap_mb", "ghc_gcdetails_mem_in_use_bytes"),
        ("rss_mb", "process_resident_memory_bytes"),
    ):
        v = value(b, name)
        s[key] = v / MB if v is not None else None
    s["last_gc_gen"] = value(b, "ghc_gcdetails_gen")
    pause = value(b, "ghc_gcdetails_elapsed_seconds")
    s["last_gc_ms"] = pause * 1000 if pause is not None else None
    s["fds"] = value(b, "process_open_fds")
    s["has_ghc"] = value(b, "ghc_gcs_total") is not None
    return s


def fmt(v, spec, missing="-"):
    return missing if v is None else format(v, spec)


def print_table(title, rows, key_fmt, top):
    if not rows:
        return
    print(f"  {title}")
    print(f"    {'req/s':>8} {'avg ms':>8} {'p95 ms':>8} {'p99 ms':>8} {'time%':>6}")
    total = sum(r["sum"] for r in rows.values()) or 1
    for key, r in sorted(rows.items(), key=lambda kv: -kv[1]["sum"])[:top]:
        print(
            f"    {r['count'] / r['dt']:>8.1f} {1000 * r['avg']:>8.2f} {fmt_bound(r['p95']):>8}"
            f" {fmt_bound(r['p99']):>8} {100 * r['sum'] / total:>5.0f}%  {key_fmt(key)[:90]}"
        )


def render(s, major_live, top):
    for group in ("handlers", "datastore", "external"):
        for r in s[group].values():
            r["dt"] = s["dt"]
    print(time.strftime("[%H:%M:%S]") + f"  window {s['dt']:.1f}s")
    print(
        f"  load     {s['rps']:8.1f} req/s   5xx {s['err_rps']:.1f}/s   cpu {fmt(s['cpu_cores'], '.2f')} cores"
        f"   open fds {fmt(s['fds'], '.0f')}"
    )
    if s["has_ghc"]:
        print(
            f"  gc       paused {fmt(s['gc_wall_pct'], '.1f')}% of wall   gc cpu {fmt(s['gc_cpu_pct'], '.1f')}%"
            f"   alloc {fmt(s['alloc_mb_s'], '.0f')} MB/s   {fmt(s['gcs_s'], '.1f')} GCs/s"
            f" (major {fmt(s['major_gcs_s'], '.2f')}/s)   last gen{fmt(s['last_gc_gen'], '.0f')} pause {fmt(s['last_gc_ms'], '.2f')} ms"
        )
        trend = ""
        if len(major_live) >= 3:
            (t0, v0), (t1, v1) = major_live[0], major_live[-1]
            if t1 > t0:
                trend = f"   live-after-major {v0:.0f}->{v1:.0f} MB ({60 * (v1 - v0) / (t1 - t0):+.1f} MB/min)"
        print(
            f"  memory   live after last GC {fmt(s['live_mb'], '.0f')} MB   peak live at a major GC"
            f" {fmt(s['max_live_mb'], '.0f')} MB   heap in use {fmt(s['heap_mb'], '.0f')} MB"
            f"   rss {fmt(s['rss_mb'], '.0f')} MB{trend}"
        )
    else:
        print("  gc       no ghc_* metrics (binary not run with +RTS -T, or ghcMetrics not registered)")
    print_table("handlers", s["handlers"], lambda k: k[0] or "(no handler label)", top)
    print_table("datastore ops", s["datastore"], lambda k: f"{k[0]} {k[1]}", top)
    print_table("external calls", s["external"], lambda k: f"{k[0]} {k[1]}", top)
    print(flush=True)


CSV_COLUMNS = [
    "rps", "err_rps", "cpu_cores", "gc_wall_pct", "gc_cpu_pct", "alloc_mb_s", "gcs_s", "major_gcs_s",
    "live_mb", "max_live_mb", "heap_mb", "rss_mb", "last_gc_ms", "fds",
]


def main():
    ap = argparse.ArgumentParser(description="Live RTS + HTTP telemetry from a service's Prometheus endpoint.")
    ap.add_argument("-p", "--port", type=int, default=9999, help="metrics port (rider-app 9999, driver-app 9997)")
    ap.add_argument("--url", help="full metrics URL; overrides --port")
    ap.add_argument("-i", "--interval", type=float, default=5.0, help="seconds between samples")
    ap.add_argument("-n", "--top", type=int, default=8, help="rows per table")
    ap.add_argument("--csv", help="append one row per sample to this CSV file")
    args = ap.parse_args()
    url = args.url or f"http://localhost:{args.port}/metrics"

    try:
        prev_t, prev = scrape(url)
    except OSError as e:
        sys.exit(f"perf-watch: cannot scrape {url}: {explain(e)}")
    print(f"perf-watch: {url} every {args.interval:.0f}s (Ctrl-C to stop)\n", flush=True)
    csv = open(args.csv, "a") if args.csv else None
    if csv and csv.tell() == 0:
        csv.write("ts," + ",".join(CSV_COLUMNS) + "\n")
    major_live = []
    last_major = value(prev, "ghc_major_gcs_total")
    try:
        while True:
            time.sleep(args.interval)
            try:
                t, cur = scrape(url)
            except OSError as e:
                print(f"perf-watch: scrape failed: {explain(e)}", flush=True)
                continue
            s = summarize(prev, cur, t - prev_t)
            majors = value(cur, "ghc_major_gcs_total")
            if majors is not None and majors != last_major and value(cur, "ghc_gcdetails_gen") and s["live_mb"] is not None:
                major_live.append((t, s["live_mb"]))
                last_major = majors
            render(s, major_live, args.top)
            if csv:
                csv.write(f"{t:.0f}," + ",".join(fmt(s[c], ".3f", "") for c in CSV_COLUMNS) + "\n")
                csv.flush()
            prev_t, prev = t, cur
    except KeyboardInterrupt:
        pass


if __name__ == "__main__":
    main()
