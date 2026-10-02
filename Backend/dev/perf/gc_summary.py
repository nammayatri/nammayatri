#!/usr/bin/env python3
import re
import statistics
import sys

ROW = re.compile(
    r"^\s*(\d+)\s+(\d+)\s+(\d+)\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)\s+\d+\s+\d+\s+\(Gen:\s+(\d+)\)"
)
MB = 1024 * 1024


def pct(values, p):
    if not values:
        return 0.0
    s = sorted(values)
    return s[min(len(s) - 1, int(round(p / 100 * (len(s) - 1))))]


def slope_per_min(points):
    if len(points) < 3:
        return None
    xs = [x for x, _ in points]
    ys = [y for _, y in points]
    mx, my = statistics.fmean(xs), statistics.fmean(ys)
    den = sum((x - mx) ** 2 for x in xs)
    if den == 0:
        return None
    return sum((x - mx) * (y - my) for x, y in zip(xs, ys)) / den * 60


def pause_line(label, pauses):
    if not pauses:
        return f"  {label:<6} none"
    ms = [p * 1000 for p in pauses]
    return (
        f"  {label:<6} {len(ms):>7} colls   p50 {pct(ms, 50):7.2f} ms   p99 {pct(ms, 99):7.2f} ms"
        f"   max {max(ms):7.2f} ms   total {sum(ms) / 1000:7.2f} s"
    )


def main(path):
    rows = []
    footer = []
    in_footer = False
    with open(path, errors="replace") as f:
        for line in f:
            m = ROW.match(line)
            if m:
                alloc, copied, live, _gc_user, gc_elap, _tot_user, tot_elap, gen = m.groups()
                rows.append((int(alloc), int(copied), int(live), float(gc_elap), float(tot_elap), int(gen)))
                continue
            if "bytes allocated in the heap" in line:
                in_footer = True
            if in_footer and line.strip():
                footer.append(line.rstrip())

    print(f"== GC ({path})")
    if not rows:
        print("  no per-GC rows found (was the run started with -S?)")
        return

    wall = rows[-1][4]
    minor = [r[3] for r in rows if r[5] == 0]
    major = [r[3] for r in rows if r[5] > 0]
    paused = sum(r[3] for r in rows)
    allocated = sum(r[0] for r in rows)
    major_rows = [r for r in rows if r[5] > 0]
    if footer and major_rows and major_rows[-1] is rows[-1]:
        major_rows = major_rows[:-1]
    major_live = [(r[4], r[2] / MB) for r in major_rows]
    max_live = max(r[2] for r in rows) / MB

    print(f"  run length   {wall:.1f} s wall, {len(rows)} collections ({len(rows) / max(wall, 1e-9):.1f}/s)")
    print(pause_line("gen0", minor))
    print(pause_line("gen1+", major))
    print(f"  paused       {paused:.2f} s = {100 * paused / max(wall, 1e-9):.1f}% of wall time (every capability stops during a GC)")
    print(f"  allocation   {allocated / (1024 * MB):.2f} GB total, {allocated / MB / max(wall, 1e-9):.0f} MB/s")

    trend = slope_per_min(major_live)
    if major_live:
        first, last = major_live[0][1], major_live[-1][1]
        trend_s = f", trend {trend:+.1f} MB/min" if trend is not None else ""
        print(
            f"  live data    after major GCs (exit GC excluded): first {first:.1f} MB -> last {last:.1f} MB,"
            f" max {max_live:.1f} MB{trend_s}"
        )
    else:
        print(f"  live data    no major GC happened; max live {max_live:.1f} MB")

    worst = sorted(rows, key=lambda r: r[3], reverse=True)[:5]
    print("  worst pauses:")
    for r in worst:
        print(f"    at {r[4]:8.1f} s   gen{r[5]}   {r[3] * 1000:8.2f} ms   live {r[2] / MB:8.1f} MB   copied {r[1] / MB:8.1f} MB")

    hints = []
    share = paused / max(wall, 1e-9)
    if share > 0.10:
        hints.append(
            f"GC stops the world {100 * share:.0f}% of the time. Fewer minor GCs: try +RTS -A64m (bigger nursery); "
            "then cut allocation in the hot path (time mode, alloc flamegraph)."
        )
    if major and max(major) > 0.1:
        hints.append(
            f"Major GC pauses reach {max(major) * 1000:.0f} ms; the copying collector's pause grows with live data "
            f"({max_live:.0f} MB). Shrink long-lived data (heap-* modes) or try +RTS --nonmoving-gc."
        )
    if trend is not None and len(major_live) >= 5 and trend > 1 and major_live[-1][1] > 1.2 * major_live[0][1]:
        hints.append(
            "Live data after major GCs keeps rising. Unless that is warm-up (caches filling), something is retained: "
            "run heap-info (fast) or heap-cc / heap-retainer to see what and who holds it."
        )
    if minor and pct(minor, 99) > 0.02:
        hints.append(
            "Minor GC p99 above 20 ms: a lot of nursery data survives (copied) or parallel GC sync is slow; "
            "try +RTS -qn2 or -qg and compare."
        )
    if hints:
        print("  hints:")
        for h in hints:
            print(f"    - {h}")

    if footer:
        print()
        print("== RTS summary")
        for line in footer:
            print(line)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit("usage: gc_summary.py <gc.log written by +RTS -S<file>>")
    main(sys.argv[1])
