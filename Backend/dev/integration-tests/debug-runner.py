#!/usr/bin/env python3
"""
Debug runner for NammaYatri integration tests.
Wraps Newman to capture per-API service logs.

Usage:
    python3 debug-runner.py <collection> <env-file> <test-logs-dir> [--errors-only] [--verbose] [--bail]

After each API call, service logs generated during that call are saved to:
    <test-logs-dir>/<collection>-<env>/<NN>-<sanitized-api>/<service>.log
It also saves request.json / response.json for each printed step and prints its request and
response (credential headers masked, bodies clipped): every step by default, only failed steps
with --errors-only. Failed steps also get the service ERROR lines and (in GitHub Actions) an
annotation + job-summary entry, so a CI failure can be debugged without logging into the runner.
"""

import subprocess, json, os, sys, re, shutil, tempfile, time

# ── Service logs to capture ──
SERVICE_LOG_DIR = os.environ.get("SERVICE_LOG_DIR", "/tmp")
SERVICE_LOGS = [
    "rider-app",
    "dynamic-offer-driver-app",
    "driver-offer-allocator",
    "beckn-gateway",
    "mock-google",
    "search-result-aggregator",
]


def _repo_root():
    try:
        return subprocess.run(["git", "rev-parse", "--show-toplevel"], capture_output=True,
                              text=True, check=True).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return ""


# The allocator writes no /tmp log; fall back to its process-compose log in the repo root.
FALLBACK_LOGS = {"driver-offer-allocator": "driver-offer-allocator-exe.log"}
REPO_ROOT = _repo_root()


def log_path(svc):
    path = os.path.join(SERVICE_LOG_DIR, f"{svc}.log")
    if not os.path.exists(path) and svc in FALLBACK_LOGS and REPO_ROOT:
        path = os.path.join(REPO_ROOT, FALLBACK_LOGS[svc])
    return path


# Service ERROR lines that show up in every run and never explain a test failure.
NOISE_RE = re.compile(
    r"Clickhouse error|syncDriverPoolDataToLTS|whatsapp_opt_api_call|GUPSHUP|AuthToken error"
    r"|Triggering onboarding messages|DriverSupplyMetrics|finalize distance to pickup"
)
ERROR_LINE_RE = re.compile(r"(\d{2}:\d{2}:\d{2}\.\d{3})\d* UTC ERROR>\s*(?:\[[^\]]*\]\s*)?\|>\s*(.*)")
MAX_BODY = 2000

# ANSI escape code pattern
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")

# Newman request boundary: "→ <name>" (U+2192 RIGHTWARDS ARROW)
REQUEST_RE = re.compile(r"^[→►▸]\s+(.+)$")


def get_log_offsets():
    """Record current end-of-file position for each service log."""
    offsets = {}
    for svc in SERVICE_LOGS:
        try:
            offsets[svc] = os.path.getsize(log_path(svc))
        except OSError:
            offsets[svc] = 0
    return offsets


def save_logs_since(offsets, dest_dir):
    """Copy only the bytes written since the recorded offsets. Returns files copied."""
    os.makedirs(dest_dir, exist_ok=True)
    copied = 0
    for svc in SERVICE_LOGS:
        src = log_path(svc)
        start = offsets.get(svc, 0)
        try:
            size = os.path.getsize(src)
            if size < start:
                start = 0  # log-cleaner truncated the file during this step; keep what was written since
            if size > start:
                with open(src, "rb") as f:
                    f.seek(start)
                    data = f.read()
                if data and data.strip(b"\x00"):  # skip if only null bytes
                    dest = os.path.join(dest_dir, f"{svc}.log")
                    with open(dest, "wb") as out:
                        out.write(data)
                    copied += 1
        except OSError:
            pass
    return copied


def sanitize_name(name):
    return re.sub(r"[^a-zA-Z0-9_\-]", "_", name).strip("_")


def strip_ansi(text):
    return ANSI_RE.sub("", text)


# ── Failure details (request / response / service errors) ──

def _url(req):
    url = req.get("url", "")
    if isinstance(url, str):
        return url
    raw = url.get("raw")
    if raw:
        return raw
    host = ".".join(url.get("host", [])) if isinstance(url.get("host"), list) else url.get("host", "")
    port = f":{url['port']}" if url.get("port") else ""
    path = "/".join(url.get("path", [])) if isinstance(url.get("path"), list) else url.get("path", "")
    query = "&".join(f"{q.get('key')}={q.get('value')}" for q in url.get("query", []) if not q.get("disabled"))
    return f"{url.get('protocol', 'http')}://{host}{port}/{path}" + (f"?{query}" if query else "")


def _response_text(resp):
    stream = resp.get("stream")
    if isinstance(stream, dict) and isinstance(stream.get("data"), list):
        return bytes(stream["data"]).decode("utf-8", errors="replace")
    return resp.get("body") or ""


def _pretty(text):
    try:
        return json.dumps(json.loads(text), indent=2)
    except (ValueError, TypeError):
        return text


def _clip(text, limit=MAX_BODY):
    return text if len(text) <= limit else text[:limit] + f"\n… ({len(text) - limit} more chars, see artifact)"


def _error_code(body):
    try:
        js = json.loads(body)
        if isinstance(js, dict):
            return js.get("errorCode") or js.get("error") or ""
    except (ValueError, TypeError):
        pass
    return ""


def service_errors(api_dir, limit=15):
    """ERROR lines the services logged during this step, minus known noise."""
    out, hidden = {}, 0
    for svc in SERVICE_LOGS:
        path = os.path.join(api_dir, f"{svc}.log")
        if not os.path.exists(path):
            continue
        lines = []
        with open(path, "r", errors="replace") as f:
            for raw in f:
                if "ERROR>" not in raw:
                    continue
                if NOISE_RE.search(raw):
                    hidden += 1
                    continue
                m = ERROR_LINE_RE.search(raw.replace('\\"', '"').replace("\\\\", "\\"))
                text = re.sub(r'",\s*"timestamp".*$', "", m.group(2)) if m else raw.strip()
                lines.append(f"{m.group(1)} {text[:300]}" if m else text[:300])
        if lines:
            out[svc] = lines[-limit:]
    return out, hidden


def gh_escape(text):
    return text.replace("%", "%25").replace("\r", "").replace("\n", "%0A")


SECRET_HEADER_RE = re.compile(r"token|auth|secret|password|cookie|api-?key", re.I)


def _mask(key, value):
    """Hide credentials in the (public) CI log; the artifact files keep the full value."""
    value = str(value)
    if not SECRET_HEADER_RE.search(str(key)):
        return value
    return value[:4] + "…" + value[-4:] if len(value) > 12 else "****"


# Transport headers Postman/servers add to every call; hidden in the log (kept in request/response.json).
STANDARD_HEADERS = {"user-agent", "accept", "cache-control", "postman-token", "host", "accept-encoding",
                    "connection", "content-length"}


def _header_lines(headers, indent):
    out = []
    for h in headers or []:
        if isinstance(h, dict) and not h.get("disabled") and str(h.get("key", "")).lower() not in STANDARD_HEADERS:
            out.append(f"{indent}{h.get('key')}: {_mask(h.get('key'), h.get('value', ''))}")
    return out


def report_step(suite, idx, total, step, req, resp, api_dir, failed, details=True):
    """Print one step's request/response (headers masked, bodies clipped) and save the full versions.
    Failed steps also get the service ERROR lines, a GitHub annotation and a job-summary entry."""
    method = req.get("method", "")
    url = _url(req)
    req_body = (req.get("body") or {}).get("raw", "") if isinstance(req.get("body"), dict) else ""
    status = resp.get("code", 0)
    status_text = resp.get("status", "")
    body = _response_text(resp)
    code = _error_code(body)
    errors, hidden = service_errors(api_dir) if (api_dir and failed) else ({}, 0)

    if api_dir:
        os.makedirs(api_dir, exist_ok=True)
        with open(os.path.join(api_dir, "request.json"), "w") as f:
            json.dump({"method": method, "url": url, "headers": req.get("header", []), "body": req_body}, f, indent=2)
        with open(os.path.join(api_dir, "response.json"), "w") as f:
            json.dump({"status": status, "statusText": status_text, "responseTimeMs": resp.get("responseTime"),
                       "headers": resp.get("header", []), "body": body}, f, indent=2)

    if not details:
        return
    pad = " " * 12
    mark = "❌" if failed else "✅"
    lines = [f"── {mark} {idx}/{total} · {step} ".ljust(78, "─"),
             f"REQUEST   {method} {url}"]
    hdrs = _header_lines(req.get("header"), pad)
    if hdrs:
        lines += ["  headers"] + hdrs
    if req_body:
        lines += ["  body"] + [pad + l for l in _clip(_pretty(req_body)).splitlines()]
    ms = resp.get("responseTime")
    lines.append(f"RESPONSE  {status} {status_text}" + (f"   ({ms} ms)" if ms is not None else ""))
    hdrs = _header_lines(resp.get("header"), pad)
    if hdrs:
        lines += ["  headers"] + hdrs
    if body:
        lines += ["  body"] + [pad + l for l in _clip(_pretty(body)).splitlines()]
    for svc, errs in errors.items():
        lines.append(f"  {svc} ERROR lines during this step:")
        lines += [pad + e for e in errs]
    if hidden:
        lines.append(f"  ({hidden} known-noise ERROR lines hidden: Clickhouse, LTS sync, WhatsApp, FCM, …)")
    print("\n".join(lines), flush=True)

    if not failed:
        return
    if os.environ.get("GITHUB_ACTIONS") == "true":
        print(f"::error title={suite} · {step}::" + gh_escape(f"{method} {url} → {status} {code}".strip()), flush=True)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a") as f:
            f.write(f"\n<details><summary>❌ <b>{suite}</b> · {step} → <b>{status}</b> {code}</summary>\n\n")
            f.write(f"**Request** `{method} {url}`\n\n")
            if req_body:
                f.write("```json\n" + _clip(_pretty(req_body), 800) + "\n```\n\n")
            f.write(f"**Response** `{status} {status_text}`\n\n```json\n{_clip(_pretty(body))}\n```\n\n")
            for svc, errs in errors.items():
                f.write(f"**{svc} ERROR lines during this step**\n\n```\n" + "\n".join(errs) + "\n```\n\n")
            f.write("</details>\n")


def main():
    import argparse

    parser = argparse.ArgumentParser(description="Debug runner for integration tests")
    parser.add_argument("collection", help="Path to Newman collection JSON")
    parser.add_argument("env_file", help="Path to Newman environment JSON")
    parser.add_argument("test_logs_dir", help="Base directory for test logs")
    parser.add_argument("--errors-only", action="store_true", help="Only capture logs for failed APIs")
    parser.add_argument("--verbose", action="store_true", help="Pass --verbose to Newman")
    parser.add_argument("--bail", action="store_true", default=True, help="Stop on first failure")
    parser.add_argument("--timeout-request", type=int, default=15000)
    args = parser.parse_args()

    collection_name = os.path.basename(args.collection).replace(".json", "")
    env_name = os.path.basename(args.env_file).replace(".postman_environment.json", "").replace("Local_", "")
    base_dir = os.path.join(args.test_logs_dir, f"{collection_name}-{env_name}")

    # Clean previous logs for this collection-env
    if os.path.exists(base_dir):
        shutil.rmtree(base_dir)
    os.makedirs(base_dir, exist_ok=True)

    # JSON report temp file
    json_report = tempfile.NamedTemporaryFile(suffix=".json", delete=False).name

    # Build Newman command
    cmd = [
        "newman", "run", args.collection,
        "-e", args.env_file,
        "--timeout-request", str(args.timeout_request),
        "--reporters", "cli,json",
        "--reporter-json-export", json_report,
    ]
    if args.bail:
        cmd.append("--bail")
    if args.verbose:
        cmd.append("--verbose")

    # Record initial log offsets (don't truncate — just track positions)
    current_offsets = get_log_offsets()

    # Run Newman, parse stdout line-by-line to detect request boundaries
    api_index = 0
    current_api = None
    api_dirs = {}  # name -> dir path
    api_dir_list = []  # dir paths in request order (step names can repeat)

    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1)

    for line in proc.stdout:
        sys.stdout.write(line)
        sys.stdout.flush()

        # Strip ANSI codes before matching
        clean = strip_ansi(line).strip()
        match = REQUEST_RE.match(clean)
        if match:
            new_api = match.group(1).strip()

            # Save logs for the PREVIOUS request (if any)
            if current_api is not None:
                api_index += 1
                api_dir = os.path.join(base_dir, f"{api_index:02d}-{sanitize_name(current_api)}")
                save_logs_since(current_offsets, api_dir)
                api_dirs[current_api] = api_dir
                api_dir_list.append(api_dir)
                current_offsets = get_log_offsets()

            current_api = new_api

    proc.wait()
    exit_code = proc.returncode

    # Small delay to let services flush logs
    time.sleep(0.3)

    # Save logs for the LAST request
    if current_api is not None:
        api_index += 1
        api_dir = os.path.join(base_dir, f"{api_index:02d}-{sanitize_name(current_api)}")
        save_logs_since(current_offsets, api_dir)
        api_dirs[current_api] = api_dir
        api_dir_list.append(api_dir)

    # Read JSON report to determine errors and write summaries
    error_apis = set()
    try:
        with open(json_report) as f:
            report = json.load(f)

        executions = report.get("run", {}).get("executions", [])
        by_order = len(executions) == len(api_dir_list)
        printed = []

        def print_header_once():
            if not printed:
                mode = "every step" if not args.errors_only else "failed steps"
                print(f"\n══ {collection_name} · request / response for {mode} ══", flush=True)
                printed.append(True)
        for idx, item in enumerate(executions):
            name = item.get("item", {}).get("name", "unknown")
            resp = item.get("response", {})
            status_code = resp.get("code", 0)
            assertions = item.get("assertions", [])
            has_failure = any(a.get("error") for a in assertions)
            is_error = status_code >= 400 or has_failure
            api_dir = api_dir_list[idx] if by_order else api_dirs.get(name)

            if is_error:
                error_apis.add(api_dir or name)

            # A step failed when an assertion failed (or it got 4xx/5xx with no assertions at all);
            # expected-error steps whose assertions pass count as passed.
            failed = has_failure or (status_code >= 400 and not assertions)
            if not args.errors_only or failed:
                print_header_once()
                report_step(collection_name, idx + 1, len(executions), name, item.get("request", {}), resp,
                            api_dir, failed)

            # Write summary to the API dir
            if api_dir:
                summary = {
                    "name": name,
                    "method": item.get("request", {}).get("method", ""),
                    "status": status_code,
                    "failed": is_error,
                    "assertions": [
                        {"name": a.get("assertion", ""), "passed": a.get("error") is None}
                        for a in assertions
                    ],
                }
                summary_path = os.path.join(api_dir, "_summary.json")
                with open(summary_path, "w") as sf:
                    json.dump(summary, sf, indent=2)
        # Keep the full newman report (every request/response of the run) next to the step logs.
        shutil.copy(json_report, os.path.join(base_dir, "newman-report.json"))
    except (json.JSONDecodeError, FileNotFoundError):
        pass
    finally:
        try:
            os.unlink(json_report)
        except OSError:
            pass

    # For errors-only mode: remove directories for non-error APIs
    if args.errors_only:
        for dir_path in api_dir_list:
            if dir_path not in error_apis:
                shutil.rmtree(dir_path, ignore_errors=True)

    # Print summary
    mode = "errors-only" if args.errors_only else "all"
    api_count = len([d for d in os.listdir(base_dir) if os.path.isdir(os.path.join(base_dir, d))]) if os.path.exists(base_dir) else 0
    if api_count > 0:
        print(f"\n  Debug logs ({mode}): {api_count} API(s) → {base_dir}")
    elif not os.path.exists(os.path.join(base_dir, "newman-report.json")):
        shutil.rmtree(base_dir, ignore_errors=True)

    sys.exit(exit_code)


if __name__ == "__main__":
    main()
