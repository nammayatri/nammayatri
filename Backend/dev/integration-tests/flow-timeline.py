#!/usr/bin/env python3
"""
Local debugging tool (not run by CI): print a time-ordered failure timeline for collection runs captured
by `run-tests.sh -d` (debug-runner.py step folders), so a failed suite can be read as one story. Use it on
your own data/test-logs/ or on a failed CI run's downloaded `integration-test-logs` artifact.

Usage:
    flow-timeline.py [--failed-only] <test-logs dir>/<collection>-<env> [more dirs...]
    flow-timeline.py --failed-only data/test-logs/*      # every collection that has a failed step
    flow-timeline.py --list-failed data/test-logs/*      # only print the folders that have a failed step

Each step folder (NN-<step>) holds _summary.json, request.json, response.json and the service log
lines written during that step. The timeline merges, sorted by time:
  - every API step with its status (and errorCode when it failed)
  - non-noise ERROR lines from every captured service (rider, driver, allocator, gateway, ...)
  - allocator dispatch events (job start, already-tried drivers, LTS nearby, who got the request, ...)
Times are shown relative to the first failed step; driver ids become D1 / D2 when the steps name them.
"""

import json, os, re, sys
from email.utils import parsedate_to_datetime

LOG_RE = re.compile(r"(\d{2}:\d{2}:\d{2}\.\d{3})\d* UTC (\w+)>\s*(?:\[([^\]]*)\]\s*)?\|>\s*(.*)")
UUID_RE = re.compile(r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}")
ALLOCATOR_EVENTS = [
    (re.compile(r"Starting job execution"), "job started"),
    (re.compile(r"Search request is either assigned, cancelled or expired"), "search try no longer active → job stops"),
    (re.compile(r"PreviousBatchesDrivers-(.*)"), "already-tried drivers: {0}"),
    (re.compile(r"lts nearBy: (.*)"), "LTS nearby drivers: {0}"),
    (re.compile(r"DriverPool\[[^\]]*\] (.*)"), "pool: {0}"),
    (re.compile(r"Send search requests to driver pool batch-(.*)"), "request SENT to: {0}"),
    (re.compile(r"Drivers exhausted mid-search.*"), "drivers exhausted (nobody left to send to)"),
    (re.compile(r"No driver accepted"), "no driver accepted"),
]
NOISE_RE = re.compile(
    r"Clickhouse error|syncDriverPoolDataToLTS|whatsapp_opt_api_call|GUPSHUP|AuthToken error"
    r"|Triggering onboarding messages|DriverSupplyMetrics|finalize distance to pickup"
)
MAX_PER_STEP = 10 # ERROR / allocator lines kept per step, so one noisy step can't flood the timeline


def load(path, default=None):
    try:
        with open(path) as f:
            return json.load(f)
    except (OSError, ValueError):
        return default


def log_lines(path):
    """Yield (time, level, message) from a service log slice (plain or process-compose wrapped)."""
    try:
        with open(path, errors="replace") as f:
            for raw in f:
                m = LOG_RE.search(raw.replace('\\\\\\"', '"').replace('\\"', '"'))
                if m:
                    yield m.group(1), m.group(2), re.sub(r'",\s*"timestamp".*$', "", m.group(4))
    except OSError:
        return


def secs(t):
    h, m, s = t.split(":")
    return int(h) * 3600 + int(m) * 60 + float(s)


def step_failed(summary):
    """Same rule as debug-runner: a failed assertion, or a 4xx/5xx with no assertions at all."""
    assertions = summary.get("assertions") or []
    if any(not a.get("passed") for a in assertions):
        return True
    return not assertions and (summary.get("status") or 0) >= 400


def step_time(step_dir, url):
    """Time the service logged this request; falls back to the response Date header (second precision)."""
    path = re.sub(r"^https?://[^/]+", "", url).split("?")[0]
    for name in sorted(os.listdir(step_dir)):
        if not name.endswith(".log"):
            continue
        for t, _, msg in log_lines(os.path.join(step_dir, name)):
            if "rawPathInfo" in msg and path and path in msg:
                return t, True
    resp = load(os.path.join(step_dir, "response.json"), {})
    for h in resp.get("headers", []):
        if str(h.get("key", "")).lower() == "date":
            try:
                return parsedate_to_datetime(h["value"]).strftime("%H:%M:%S.000"), False
            except (TypeError, ValueError):
                pass
    return None, False


def driver_names(steps):
    """Map driver UUIDs to D1 / D2 from step names like "D2 Add Vehicle" whose URL carries the id."""
    names = {}
    for s in steps:
        m = re.match(r"(D\d)\b", s["name"])
        url = s["req"].get("url", "")
        ids = UUID_RE.findall(url)
        if m and ids and "/driver/" in url:
            names.setdefault(ids[-1], m.group(1))
    return names


def shorten(text, names):
    """Driver lists become [D1, D2]; other ids are cut to 8 chars."""
    ids = re.findall(r'driverId = Id \{getId = "([^"]+)"', text) or (
        re.findall(r'getId = "([^"]+)"', text) if text.lstrip().startswith("[Id {") else [])
    if ids or text.strip() == "[]":
        return "[" + ", ".join(dict.fromkeys(names.get(i, i[:8]) for i in ids)) + "]"
    for uid, label in names.items():
        text = text.replace(uid, label)
    return UUID_RE.sub(lambda m: m.group(0)[:8], text)[:220]


def load_steps(run_dir):
    steps = []
    for d in sorted(os.listdir(run_dir)):
        full = os.path.join(run_dir, d)
        if not (os.path.isdir(full) and d[:2].isdigit()):
            continue
        summary = load(os.path.join(full, "_summary.json"), {})
        steps.append({"name": summary.get("name", d), "summary": summary, "dir": full,
                      "req": load(os.path.join(full, "request.json"), {}),
                      "resp": load(os.path.join(full, "response.json"), {})})
    return steps


def timeline(run_dir, failed_only):
    title = os.path.basename(run_dir.rstrip("/"))
    steps = load_steps(run_dir)
    failed = [s for s in steps if step_failed(s["summary"])]
    if failed_only and not failed:
        return False
    if not steps:
        print(f"═══ {title}: no step folders (was it run with run-tests.sh -d?) ═══\n")
        return True
    names = driver_names(steps)

    events = []
    for s in steps:
        t, exact = step_time(s["dir"], s["req"].get("url", ""))
        status = s["summary"].get("status", s["resp"].get("status", ""))
        extra = ""
        if step_failed(s["summary"]):
            code = re.search(r'"errorCode"\s*:\s*"([^"]+)"', s["resp"].get("body") or "")
            extra = "❌ " + (code.group(1) if code else "")
        if t:
            events.append((t, f"{s['name']}  → {status}", extra.strip(), "" if exact else "~"))
        extra_lines = 0
        for name in sorted(os.listdir(s["dir"])):
            if not name.endswith(".log") or extra_lines >= MAX_PER_STEP:
                continue
            svc = name[:-4]
            for lt, lvl, msg in log_lines(os.path.join(s["dir"], name)):
                if extra_lines >= MAX_PER_STEP:
                    break
                label = None
                if svc == "driver-offer-allocator":
                    for rx, text in ALLOCATOR_EVENTS:
                        m = rx.search(msg)
                        if m:
                            label = "allocator: " + text.format(*(shorten(g, names) for g in m.groups()))
                            break
                if label is None and lvl == "ERROR" and not NOISE_RE.search(msg):
                    label = f"{svc} ERROR: " + shorten(msg, names)
                if label:
                    events.append((lt, label, "", ""))
                    extra_lines += 1

    events.sort(key=lambda e: secs(e[0]))
    first_failed = failed[0] if failed else None
    anchor = next((e for e in events if first_failed and e[1].startswith(first_failed["name"] + "  →")), None)
    t0 = secs(anchor[0]) if anchor else (secs(events[0][0]) if events else None)
    head = f"❌ FAILED at {first_failed['name']}" if first_failed else "✅ PASSED"
    print(f"═══ {title} — {head} ═══")
    print(f"{'time (UTC)':<14}{'Δ failure' if anchor else 'Δ start':>10}  event")
    for t, what, extra, approx in events:
        delta = f"{secs(t) - t0:+.1f}s" if t0 is not None else ""
        print(f"{approx}{t:<13}{delta:>10}  {what}" + (f"   {extra}" if extra else ""))
    if first_failed:
        code = re.search(r'"errorCode"\s*:\s*"([^"]+)"', first_failed["resp"].get("body") or "")
        print(f"verdict: failed at \"{first_failed['name']}\" → {first_failed['summary'].get('status', '')}"
              + (f" {code.group(1)}" if code else ""))
    print()
    return True


def main():
    args = sys.argv[1:]
    failed_only = "--failed-only" in args
    dirs = [d for d in args if not d.startswith("--") and os.path.isdir(d)]
    if "--list-failed" in args:  # just print the run folders that have a failed step (used for the upload)
        for d in dirs:
            if any(step_failed(s["summary"]) for s in load_steps(d)):
                print(d.rstrip("/"))
        return
    shown = sum(timeline(d, failed_only) for d in dirs)
    if not shown:
        print("flow-timeline: no " + ("failed " if failed_only else "") + "collection runs found (nothing to show)")


if __name__ == "__main__":
    main()
