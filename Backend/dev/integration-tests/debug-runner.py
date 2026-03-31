#!/usr/bin/env python3
"""
Debug runner for NammaYatri integration tests.
Wraps Newman to capture per-API service logs.

Usage:
    python3 debug-runner.py <collection> <env-file> <test-logs-dir> [--errors-only] [--verbose] [--bail]

After each API call, service logs generated during that call are saved to:
    <test-logs-dir>/<collection>-<env>/<NN>-<sanitized-api>/<service>.log
"""

import subprocess, json, os, sys, re, shutil, tempfile, time

# ── Service logs to capture ──
SERVICE_LOG_DIR = "/tmp"
SERVICE_LOGS = [
    "rider-app",
    "dynamic-offer-driver-app",
    "driver-offer-allocator",
    "beckn-gateway",
    "mock-payment",
    "mock-google",
    "search-result-aggregator",
]

# ANSI escape code pattern
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")

# Newman request boundary: "→ <name>" (U+2192 RIGHTWARDS ARROW)
REQUEST_RE = re.compile(r"^[→►▸]\s+(.+)$")


def get_log_offsets():
    """Record current end-of-file position for each service log."""
    offsets = {}
    for svc in SERVICE_LOGS:
        path = os.path.join(SERVICE_LOG_DIR, f"{svc}.log")
        try:
            offsets[svc] = os.path.getsize(path)
        except OSError:
            offsets[svc] = 0
    return offsets


def save_logs_since(offsets, dest_dir):
    """Copy only the bytes written since the recorded offsets. Returns files copied."""
    os.makedirs(dest_dir, exist_ok=True)
    copied = 0
    for svc in SERVICE_LOGS:
        src = os.path.join(SERVICE_LOG_DIR, f"{svc}.log")
        start = offsets.get(svc, 0)
        try:
            size = os.path.getsize(src)
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

    # Read JSON report to determine errors and write summaries
    error_apis = set()
    try:
        with open(json_report) as f:
            report = json.load(f)

        for item in report.get("run", {}).get("executions", []):
            name = item.get("item", {}).get("name", "unknown")
            resp = item.get("response", {})
            status_code = resp.get("code", 0)
            assertions = item.get("assertions", [])
            has_failure = any(a.get("error") for a in assertions)
            is_error = status_code >= 400 or has_failure

            if is_error:
                error_apis.add(name)

            # Write summary to the API dir
            if name in api_dirs:
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
                summary_path = os.path.join(api_dirs[name], "_summary.json")
                with open(summary_path, "w") as sf:
                    json.dump(summary, sf, indent=2)
    except (json.JSONDecodeError, FileNotFoundError):
        pass
    finally:
        try:
            os.unlink(json_report)
        except OSError:
            pass

    # For errors-only mode: remove directories for non-error APIs
    if args.errors_only:
        for name, dir_path in list(api_dirs.items()):
            if name not in error_apis:
                shutil.rmtree(dir_path, ignore_errors=True)

    # Remove empty API dirs (no log files, only _summary.json or nothing)
    for dirpath in [os.path.join(base_dir, d) for d in os.listdir(base_dir)]:
        if os.path.isdir(dirpath):
            files = os.listdir(dirpath)
            log_files = [f for f in files if f.endswith(".log")]
            if not log_files:
                # Keep dir if it has _summary.json (errors-only mode still useful)
                pass

    # Print summary
    mode = "errors-only" if args.errors_only else "all"
    api_count = len([d for d in os.listdir(base_dir) if os.path.isdir(os.path.join(base_dir, d))]) if os.path.exists(base_dir) else 0
    if api_count > 0:
        print(f"\n  Debug logs ({mode}): {api_count} API(s) → {base_dir}")
    else:
        shutil.rmtree(base_dir, ignore_errors=True)

    sys.exit(exit_code)


if __name__ == "__main__":
    main()
