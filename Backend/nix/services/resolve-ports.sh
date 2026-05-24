#!/usr/bin/env bash
# resolve-ports.sh — Generate data/ports-resolved.nix from the base ports.nix.
#
# Reads Backend/nix/services/ports.nix (the base template, checked into git),
# checks each port for availability, resolves conflicts, and writes the result
# to <repo-root>/data/ports-resolved.nix (gitignored via /data).
#
# Usage:
#   ./resolve-ports.sh                  # generate data/ports-resolved.nix
#   ./resolve-ports.sh --check-only     # just print, don't write
#
# Called automatically before `, run-mobility-stack-dev` so that
# multiple backend instances on the same devbox get non-conflicting ports.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
BASE_PORTS_FILE="$SCRIPT_DIR/ports.nix"
RESOLVED_PORTS_FILE="$REPO_ROOT/data/ports-resolved.nix"

CHECK_ONLY=false
if [[ "${1:-}" == "--check-only" ]]; then
  CHECK_ONLY=true
fi

# --- Helpers ---

# Check if a port is in use (returns 0 if in use, 1 if free)
is_port_in_use() {
  local port=$1
  # Try ss first (Linux), fall back to lsof (macOS)
  if command -v ss &>/dev/null; then
    ss -tln 2>/dev/null | grep -q ":${port} " && return 0
  fi
  if command -v lsof &>/dev/null; then
    lsof -iTCP:"${port}" -sTCP:LISTEN -P -n &>/dev/null && return 0
  fi
  return 1
}

# Find the next free port starting from the given port.
# Strategy:
#   Round 1: try base, base+1, base+2 ... base+99      (100 nearby)
#   Round 2: try base+1000, base+1001 ... base+1099     (100 nearby at +1000)
#   Round 3: try base+2000, base+2001 ... base+2099     (100 nearby at +2000)
#   ... keeps jumping by 1000 until a free port is found or hits 65535.
#
# If check_bus_port is "true", also verifies that port+10000 (the Redis cluster
# bus port) is free — Redis binds this automatically so we must reserve it.
find_free_port() {
  local base=$1
  local check_bus=${2:-false}
  local jump=0
  while true; do
    local start=$((base + jump))
    local attempt=0
    local port=$start
    while [[ $attempt -lt 100 ]]; do
      if [[ $port -ge 65535 ]]; then
        echo "ERROR: No free port found for base $base (exhausted all ranges)" >&2
        exit 1
      fi
      if ! is_port_in_use "$port"; then
        # For Redis cluster ports, also check the bus port (port + 10000)
        if [[ "$check_bus" == "true" ]]; then
          local bus_port=$((port + 10000))
          if [[ $bus_port -lt 65535 ]] && ! is_port_in_use "$bus_port"; then
            echo "$port"
            return
          fi
        else
          echo "$port"
          return
        fi
      fi
      port=$((port + 1))
      attempt=$((attempt + 1))
    done
    jump=$((jump + 1000))
    if [[ $((base + jump)) -ge 65535 ]]; then
      echo "ERROR: No free port found for base $base (exhausted all ranges)" >&2
      exit 1
    fi
  done
}

# --- Main ---

if [[ ! -f "$BASE_PORTS_FILE" ]]; then
  echo "ERROR: $BASE_PORTS_FILE not found" >&2
  exit 1
fi

# Track all ports we've already assigned in this run to avoid
# assigning the same free port to two different services.
declare -A assigned_ports

# Read base ports.nix line by line, resolve each port
output=""
changed=0

while IFS= read -r line; do
  # Match lines like: `  service-name = 12345;`
  if [[ "$line" =~ ^([[:space:]]*([a-zA-Z][a-zA-Z0-9_-]*)[[:space:]]*=[[:space:]]*)([0-9]+)(;.*)$ ]]; then
    prefix="${BASH_REMATCH[1]}"
    name="${BASH_REMATCH[2]}"
    base_port="${BASH_REMATCH[3]}"
    suffix="${BASH_REMATCH[4]}"

    # Redis cluster nodes need their bus port (port+10000) to also be free
    check_bus=false
    if [[ "$name" == redis-cluster-* ]]; then
      check_bus=true
    fi

    # Find a free port starting from the base port
    resolved_port=$(find_free_port "$base_port" "$check_bus")

    # Make sure we haven't already assigned this port (or its bus port) to another service
    while [[ -n "${assigned_ports[$resolved_port]:-}" ]]; do
      resolved_port=$(find_free_port $((resolved_port + 1)) "$check_bus")
    done
    assigned_ports[$resolved_port]=1

    # Reserve the bus port too so no other service lands on it
    if [[ "$check_bus" == "true" ]]; then
      assigned_ports[$((resolved_port + 10000))]=1
    fi

    if [[ "$resolved_port" != "$base_port" ]]; then
      echo "  $name: $base_port → $resolved_port (was in use)"
      changed=1
    fi

    output+="${prefix}${resolved_port}${suffix}"$'\n'
  else
    output+="$line"$'\n'
  fi
done < "$BASE_PORTS_FILE"

# Remove trailing newline
output="${output%$'\n'}"

if [[ "$CHECK_ONLY" == true ]]; then
  if [[ $changed -eq 0 ]]; then
    echo "All ports are free — no changes needed."
  fi
  echo "$output"
else
  mkdir -p "$(dirname "$RESOLVED_PORTS_FILE")"
  echo "$output" > "$RESOLVED_PORTS_FILE"
  if [[ $changed -eq 0 ]]; then
    echo "All ports are free. Generated $(basename "$RESOLVED_PORTS_FILE")"
  else
    echo "Generated $(basename "$RESOLVED_PORTS_FILE") with resolved ports."
  fi
fi
