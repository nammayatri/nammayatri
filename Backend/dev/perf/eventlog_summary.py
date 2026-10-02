#!/usr/bin/env python3
import re
import sys
from collections import Counter, defaultdict

EVENT = re.compile(r"^\s*(\d+): (.*)$")
STOP = re.compile(r"^cap (\d+): stopping thread (\d+) \((.*)\)$")
RUN = re.compile(r"^cap (\d+): running thread (\d+)$")
CREATE = re.compile(r"^cap \d+: creating thread \d+$")
REQUEST_GC = re.compile(r"^cap \d+: requesting (parallel|sequential) GC$")
ALL_STOPPED = re.compile(r"^cap \d+: all caps stopped for GC$")
START_GC = re.compile(r"^cap \d+: starting GC$")
END_GC = re.compile(r"^cap \d+: finished GC$")
OWNED_BY = re.compile(r" owned by thread \d+$")


def pct(values, p):
    if not values:
        return 0.0
    s = sorted(values)
    return s[min(len(s) - 1, int(round(p / 100 * (len(s) - 1))))]


def main(stream):
    first = last = None
    stops = Counter()
    blocked_since = {}
    blocked = defaultdict(list)
    running = {}
    busy = defaultdict(int)
    caps = set()
    created = finished = alive = peak = 0
    gc_active = 0
    gc_begin = None
    request_ts = None
    pauses = []
    syncs = []

    for line in stream:
        m = EVENT.match(line)
        if not m:
            continue
        ts = int(m.group(1))
        body = m.group(2)
        if first is None:
            first = ts
        last = ts

        r = RUN.match(body)
        if r:
            cap, tid = int(r.group(1)), int(r.group(2))
            caps.add(cap)
            running[cap] = ts
            since = blocked_since.pop(tid, None)
            if since:
                blocked[since[1]].append(ts - since[0])
            continue

        s = STOP.match(body)
        if s:
            cap, tid, reason = int(s.group(1)), int(s.group(2)), OWNED_BY.sub("", s.group(3))
            caps.add(cap)
            stops[reason] += 1
            started = running.pop(cap, None)
            if started is not None:
                busy[cap] += ts - started
            if reason == "thread finished":
                finished += 1
                alive -= 1
            elif reason.startswith("blocked") or reason.startswith("waiting"):
                blocked_since[tid] = (ts, reason)
            continue

        if CREATE.match(body):
            created += 1
            alive += 1
            peak = max(peak, alive)
        elif REQUEST_GC.match(body):
            if gc_active == 0 and request_ts is None:
                request_ts = ts
        elif START_GC.match(body):
            if gc_active == 0:
                gc_begin = request_ts if request_ts is not None else ts
            gc_active += 1
            last_start = ts
        elif END_GC.match(body):
            gc_active = max(0, gc_active - 1)
            if gc_active == 0 and gc_begin is not None:
                pauses.append(ts - gc_begin)
                if request_ts is not None:
                    syncs.append(last_start - request_ts)
                gc_begin = None
                request_ts = None

    if first is None:
        print("== Eventlog: no events (empty or unreadable eventlog)")
        return

    span = max(last - first, 1)
    secs = span / 1e9
    print(f"== Eventlog scheduler view ({secs:.1f} s, {len(caps)} capabilities)")

    utils = {c: 100 * busy[c] / span for c in sorted(caps)}
    avg_util = sum(utils.values()) / max(len(utils), 1)
    print("  capability busy (running Haskell code):")
    print("    " + "  ".join(f"cap{c} {u:4.0f}%" for c, u in utils.items()) + f"   avg {avg_util:.0f}%")
    print(f"  threads      created {created}, finished {finished}, peak alive (created during run) {peak}")

    print("  why threads stopped:")
    for reason, n in stops.most_common(12):
        print(f"    {n:>10}  {n / secs:>10.1f}/s  {reason}")

    if blocked:
        print("  time spent blocked, by reason:")
        print(f"    {'count':>9}  {'total s':>9}  {'p50 ms':>8}  {'p99 ms':>8}  {'max ms':>9}  reason")
        for reason, ds in sorted(blocked.items(), key=lambda kv: -sum(kv[1])):
            ms = [d / 1e6 for d in ds]
            print(
                f"    {len(ms):>9}  {sum(ms) / 1000:>9.2f}  {pct(ms, 50):>8.2f}  {pct(ms, 99):>8.2f}  {max(ms):>9.2f}  {reason}"
            )
        mvar = [d / 1e6 for d in blocked.get("blocked on an MVar", [])]
        short = [d for d in mvar if d < 1000]
        if mvar and len(short) < len(mvar):
            print(
                f"    MVar waits under 1 s: {len(short)} ({sum(short) / 1000:.2f} s total, p99 {pct(short, 99):.2f} ms);"
                " the longer ones are mostly idle keep-alive connections, timers and background loops"
            )
    still = Counter(reason for _, reason in blocked_since.values())
    if still:
        print("  still blocked when the log ended: " + ", ".join(f"{n} {r}" for r, n in still.most_common()))

    if pauses:
        pm = [p / 1e6 for p in pauses]
        print(
            f"  GC           {len(pm)} pauses, p50 {pct(pm, 50):.2f} ms, p99 {pct(pm, 99):.2f} ms, max {max(pm):.2f} ms,"
            f" {100 * sum(pauses) / span:.1f}% of wall time"
        )
    if syncs:
        sm = [x / 1e6 for x in syncs]
        print(f"  GC sync      (request -> every capability joined) p50 {pct(sm, 50):.2f} ms, p99 {pct(sm, 99):.2f} ms, max {max(sm):.2f} ms")

    hints = []
    total_blocked = {r: sum(d for d in ds if d < 1e9) / 1e9 for r, ds in blocked.items()}
    gc_share = sum(pauses) / span
    if avg_util > 85:
        hints.append("Capabilities are ~fully busy: CPU-bound. Use `time` mode to find the hot code, or give it more cores (-N).")
    elif gc_share > 0.3:
        hints.append(
            f"GC dominates: every capability is stopped for GC {100 * gc_share:.0f}% of the time, which is why they "
            "look idle. See the gc.log hints (nursery size, allocation rate, live data)."
        )
    elif avg_util < 50 and sum(total_blocked.values()) > 0.5 * secs:
        hints.append(
            "Capabilities are mostly idle while threads are blocked: the limit is not Haskell CPU. Look at what the "
            "requests wait on - DB/Redis pools, external HTTP calls, or a shared MVar/STM lock."
        )
    if total_blocked.get("blocked on black hole", 0) > 0.05 * secs:
        hints.append(
            "Threads queue on black holes: many requests force the same unevaluated thunk (a lazily built shared "
            "value such as a cache or config). Build it strictly / ahead of time."
        )
    if total_blocked.get("blocked in STM retry", 0) > 0.05 * secs:
        hints.append("Threads wait in STM retry: something gates requests through a TVar (queue, semaphore, cache).")
    if total_blocked.get("blocked on an MVar", 0) > 0.1 * secs:
        hints.append(
            "`blocked on an MVar` mixes three kinds of waiting: socket I/O (the threaded RTS I/O manager parks "
            "threads on MVars), resource-pool 0.4 DB/Redis connection pools when every connection is taken, and "
            "plain locks. If it grows with concurrency while capabilities stay idle, compare pool sizes "
            "(POSTGRES_POOL_MAX x POSTGRES_POOL_STRIPES, esqDB connectionPoolCount, Redis connectMaxConnections) "
            "with the load you are driving."
        )
    if syncs and pct([x / 1e6 for x in syncs], 99) > 5:
        hints.append(
            "Slow GC sync: some thread takes long to reach a safe point (tight non-allocating loop or an unsafe "
            "foreign call), stalling every other capability."
        )
    if stops.get("heap overflow", 0) / secs > 500:
        hints.append("Very frequent heap-overflow stops: the nursery fills fast. Try +RTS -A64m and compare.")
    if hints:
        print("  hints:")
        for h in hints:
            print(f"    - {h}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit("usage: ghc-events show <file.eventlog> | eventlog_summary.py -")
    main(sys.stdin if sys.argv[1] == "-" else open(sys.argv[1], errors="replace"))
