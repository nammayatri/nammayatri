# resolve-ports.nix — Nix wrapper around the resolve-ports shell logic.
#
# Probes each port in the base ports.nix for availability and substitutes free
# alternatives, then emits the resolved ports as a JSON object {"name":port,...}
# on STDOUT (all diagnostics go to STDERR). The run-mobility-stack-dev preflight
# captures that JSON and records it in the devbox registry under DEVBOX_KEY;
# nammayatri.nix + the Caddyfile then read the ports from the registry. There is
# no longer a data/ports-resolved.nix file.
#
# Usage:
#   ${resolvePorts}/bin/resolve-ports                 # JSON map on stdout
#   ${resolvePorts}/bin/resolve-ports --check-only    # human-readable list on stdout
#
# Env vars:
#   FLAKE_ROOT       — repo root (provided by mission-control).
#   BASE_PORTS_FILE  — optional override for the base template.
#   DEVBOX_KEY       — this checkout's registry key (so we exclude our OWN
#                      previously-claimed ports when reading the registry).
{ pkgs, lib ? pkgs.lib }:

pkgs.writeShellApplication {
  name = "resolve-ports";
  runtimeInputs = with pkgs; [ coreutils gnugrep gnused lsof jq ]
    ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.iproute2 ];
  text = ''
    set -euo pipefail

    : "''${FLAKE_ROOT:?FLAKE_ROOT must be set}"
    BASE_PORTS_FILE="''${BASE_PORTS_FILE:-''${FLAKE_ROOT}/Backend/nix/services/ports.nix}"

    CHECK_ONLY=false
    if [[ "''${1:-}" == "--check-only" ]]; then
      CHECK_ONLY=true
    fi

    # ── Load ports claimed by OTHER developers from devbox-registry.json ──
    # This avoids the timing race where another dev's services haven't
    # fully bound yet when we probe with ss/lsof.
    declare -A registry_ports
    _registry_count=0
    REGISTRY="/tmp/devbox-registry.json"
    # Our registry key: DEVBOX_KEY from the preflight, else derive from a devbox
    # path (/tmp/<name>/nammayatri → <name>) for standalone invocation.
    MY_DEV_NAME="''${DEVBOX_KEY:-}"
    if [[ -z "$MY_DEV_NAME" ]]; then
      MY_DEV_NAME=$(echo "''${FLAKE_ROOT}" | sed -n 's|^/tmp/\([^/]*\)/nammayatri.*|\1|p')
    fi
    if [[ -f "$REGISTRY" ]] && [[ -n "$MY_DEV_NAME" ]]; then
      # Extract all ports from every developer EXCEPT ourselves.
      while IFS= read -r p; do
        if [[ -n "$p" ]]; then
          registry_ports[$p]=1
          _registry_count=$((_registry_count + 1))
        fi
      done < <(jq -r --arg me "$MY_DEV_NAME" \
        '[.users // {} | to_entries[] | select(.key != $me) | .value.ports // {} | to_entries[].value] | .[]' \
        "$REGISTRY" 2>/dev/null || true)
      if [[ $_registry_count -gt 0 ]]; then
        echo "  Loaded $_registry_count port(s) claimed by other developers from devbox-registry.json" >&2
      fi
    fi

    is_port_in_use() {
      local port=$1
      # Check the devbox registry first (other developers' claimed ports).
      if [[ -n "''${registry_ports[$port]:-}" ]]; then
        return 0
      fi
      if command -v ss &>/dev/null; then
        ss -tln 2>/dev/null | grep -qE ":''${port}(\s|$)" && return 0
      fi
      if command -v lsof &>/dev/null; then
        lsof -iTCP:"''${port}" -sTCP:LISTEN -P -n &>/dev/null && return 0
      fi
      return 1
    }

    # Strategy: try base..base+99, then +1000..+1099, then +2000..+2099, ...
    # check_bus=true also reserves port+10000 (Redis cluster bus port).
    find_free_port() {
      local base=$1
      local check_bus=''${2:-false}
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

    if [[ ! -f "$BASE_PORTS_FILE" ]]; then
      echo "ERROR: $BASE_PORTS_FILE not found" >&2
      exit 1
    fi

    declare -A assigned_ports
    # Accumulate resolved "name<TAB>port" pairs; the final JSON is built from
    # these (comments / non-port lines in ports.nix are not needed anymore).
    pairs=""
    changed=0

    while IFS= read -r line; do
      if [[ "$line" =~ ^([[:space:]]*([a-zA-Z][a-zA-Z0-9_-]*)[[:space:]]*=[[:space:]]*)([0-9]+)(\;.*)$ ]]; then
        name="''${BASH_REMATCH[2]}"
        base_port="''${BASH_REMATCH[3]}"

        check_bus=false
        if [[ "$name" == redis-cluster-* ]]; then
          check_bus=true
        fi

        resolved_port=$(find_free_port "$base_port" "$check_bus")

        while [[ -n "''${assigned_ports[$resolved_port]:-}" ]]; do
          resolved_port=$(find_free_port $((resolved_port + 1)) "$check_bus")
        done
        assigned_ports[$resolved_port]=1

        if [[ "$check_bus" == "true" ]]; then
          assigned_ports[$((resolved_port + 10000))]=1
        fi

        if [[ "$resolved_port" != "$base_port" ]]; then
          echo "  $name: $base_port → $resolved_port (was in use)" >&2
          changed=1
        fi

        pairs+="$name"$'\t'"$resolved_port"$'\n'
      fi
    done < "$BASE_PORTS_FILE"

    if [[ $changed -eq 0 ]]; then
      echo "All ports are free — no changes needed." >&2
    fi

    if [[ "$CHECK_ONLY" == true ]]; then
      # Human-readable list on stdout for standalone debugging.
      printf '%s' "$pairs" | while IFS=$'\t' read -r n p; do
        [[ -n "$n" ]] && echo "$n = $p"
      done
    else
      # JSON map {"name":port,...} on stdout — the caller records it in the registry.
      printf '%s' "$pairs" | jq -Rn '[inputs | select(length > 0) | split("\t") | {key: .[0], value: (.[1] | tonumber)}] | from_entries'
    fi
  '';
}
