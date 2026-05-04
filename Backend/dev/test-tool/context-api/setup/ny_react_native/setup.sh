#!/usr/bin/env bash
# Thin shim — defers to the Python orchestrator under
#   Backend/dev/test-tool/context-api/setup/ny_react_native/__main__.py
# which in turn execs the inner build runner inside a nix shell with the
# right tools (Node 22, Yarn, JDK 17) on PATH.
#
# Driven by env vars (set by test-context-api before Popen):
#   NY_RN_APP       customer | driver | both
#   NY_RN_PLATFORM  android | ios
#   NY_RN_VARIANT   <Brand>   (e.g. lynx, NammaYatri, BharatTaxi)
#   NY_RN_PATH      optional explicit checkout path (default: data/ny-react-native)
#
# The script's last log line is the sentinel
#   ny-react-native: launched <apps>
# which test-context-api looks for to flip ready=true.
#
# Migration history: this file used to be ~1300 lines of inline orchestration.
# Stage 1 extracted embedded data + Python heredocs to setup/ny_react_native/.
# Stage 2 split the orchestration in two — Python handles ssh+clone+submodules
# (this layer), the inner ``_build_runner.sh`` handles the per-app prebuild
# + build + install + launch loop inside the nix shell.

set -euo pipefail

# Resolve repo root (this script now lives at
# Backend/dev/test-tool/context-api/setup/ny_react_native/).
REPO_ROOT="$(cd "$(dirname "$0")/../../../../../.." && pwd)"
export REPO_ROOT

# Hand off to Python. The orchestrator does ssh setup, clone, submodule
# init, iOS sub-podspec sanity check, then exec's into the nix shell
# build runner. Use `exec` so the Python process replaces us — keeps
# test-context-api's Popen pipe alive for live log streaming.
SETUP_PARENT="$REPO_ROOT/Backend/dev/test-tool/context-api"
export PYTHONPATH="$SETUP_PARENT${PYTHONPATH:+:$PYTHONPATH}"
exec python3 -m setup.ny_react_native
