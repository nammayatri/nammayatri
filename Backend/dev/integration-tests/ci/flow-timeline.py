#!/usr/bin/env python3
"""
CI: print a driver-cancel -> reallocation timeline for a collection run captured by
`run-tests.sh -d` (debug-runner.py step folders), so passing and failing runs can be compared.

Usage:
    flow-timeline.py <test-logs dir>/03-AutoDriverCancellation-NY_Bangalore [more dirs...]

Each step folder (NN-<step>) holds _summary.json, request.json, response.json and the service
log lines written during that step. The timeline merges:
  - each API step (time from the service's own Request&Response log line, else the Date header)
  - allocator events (job start, excluded drivers, LTS nearby result, who got the request, exhaustion)
  - non-noise driver-app ERROR lines
and prints times relative to "D1 Cancel Ride", with driver ids shown as D1 / D2.
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
    (re.compile(r"Unexpected empty driver pool batch"), "empty driver batch"),
]
NOISE_RE = re.compile(
    r"Clickhouse error|syncDriverPoolDataToLTS|whatsapp_opt_api_call|GUPSHUP|AuthToken error"
    r"|Triggering onboarding messages|DriverSupplyMetrics|finalize distance to pickup"
)


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
    """Map driver UUIDs to D1 / D2 from the step names that carry the driver id in the URL."""
    names = {}
    for name, req, _ in steps:
        m = re.match(r"(D\d)\b", name)
        ids = UUID_RE.findall(req.get("url", ""))
        if m and ids and "/driver/" in req.get("url", ""):
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


def timeline(run_dir):
    title = os.path.basename(run_dir.rstrip("/"))
    step_dirs = sorted(d for d in os.listdir(run_dir) if os.path.isdir(os.path.join(run_dir, d)) and d[:2].isdigit())
    steps = []
    for d in step_dirs:
        full = os.path.join(run_dir, d)
        summary = load(os.path.join(full, "_summary.json"), {})
        steps.append((summary.get("name", d), load(os.path.join(full, "request.json"), {}), full))
    if not steps:
        print(f"═══ {title}: no step folders (was it run with run-tests.sh -d?) ═══")
        return
    names = driver_names(steps)

    events, failed_steps = [], []
    for name, req, full in steps:
        summary = load(os.path.join(full, "_summary.json"), {})
        resp = load(os.path.join(full, "response.json"), {})
        t, exact = step_time(full, req.get("url", ""))
        status = summary.get("status", resp.get("status", ""))
        ok = not summary.get("failed") or all(a.get("passed") for a in summary.get("assertions", [])) and summary.get("assertions")
        extra = ""
        body = resp.get("body", "")
        if name.endswith("Get Nearby Ride Requests"):
            tries = re.findall(r'"searchTryId"\s*:\s*"([^"]+)"', body or "")
            extra = f"searchTryId {tries[0][:8]}" if tries else "no ride requests"
        elif not ok:
            code = re.search(r'"errorCode"\s*:\s*"([^"]+)"', body or "")
            extra = code.group(1) if code else ""
            failed_steps.append(name)
        if t:
            events.append((t, f"{name}  → {status}", extra, "" if exact else "~"))
        alloc = os.path.join(full, "driver-offer-allocator.log")
        for at, _, msg in log_lines(alloc):
            for rx, label in ALLOCATOR_EVENTS:
                m = rx.search(msg)
                if m:
                    events.append((at, "allocator: " + label.format(*(shorten(g, names) for g in m.groups())), "", ""))
                    break
        drv = os.path.join(full, "dynamic-offer-driver-app.log")
        for dt, lvl, msg in log_lines(drv):
            if lvl == "ERROR" and not NOISE_RE.search(msg):
                events.append((dt, "driver-app ERROR: " + shorten(msg, names), "", ""))

    events.sort(key=lambda e: secs(e[0]))
    cancel = next((e for e in events if "Cancel Ride" in e[1]), None)
    t0 = secs(cancel[0]) if cancel else None
    verdict = "❌ FAILED at " + ", ".join(failed_steps) if failed_steps else "✅ PASSED"
    print(f"═══ {title} — {verdict} ═══")
    print(f"{'time (UTC)':<14}{'Δ cancel':>9}  event")
    for t, what, extra, approx in events:
        delta = f"{secs(t) - t0:+.1f}s" if t0 is not None else ""
        print(f"{approx}{t:<13}{delta:>9}  {what}" + (f"   {extra}" if extra else ""))
    sent = [e for e in events if "request SENT" in e[1] and t0 is not None and secs(e[0]) >= t0]
    accept = next((e for e in events if e[1].startswith("D2 Accept Ride")), None)
    if t0 is not None:
        parts = [f"D1 cancelled at {cancel[0]}"]
        parts.append(f"reallocation sent to {sent[0][1].split(': ', 2)[-1]} at +{secs(sent[0][0]) - t0:.1f}s" if sent
                     else "reallocation never sent a request")
        if accept:
            parts.append(f"D2 accepted at +{secs(accept[0]) - t0:.1f}s → {accept[1].split('→ ')[-1]}")
        print("verdict: " + "; ".join(parts))
    print()


def main():
    dirs = [d for d in sys.argv[1:] if os.path.isdir(d)]
    if not dirs:
        print("flow-timeline: no captured runs found (nothing to show)")
        return
    for d in dirs:
        timeline(d)


if __name__ == "__main__":
    main()
