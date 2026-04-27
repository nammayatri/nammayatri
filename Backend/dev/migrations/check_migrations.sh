#!/usr/bin/env bash
# check_migrations.sh — validate dev/migrations + dev/migrations-read-only
# without booting the full stack. Boots a standalone Postgres-with-postgis
# against the existing dev data dir, recreates atlas_dev, runs all seeds in
# `, run-mobility-stack-dev` order, then runs each service's migrations in the
# order the app applies them at startup (per Backend/dhall-configs/dev/<svc>.dhall).
# Reports errors per service.
#
# Prereq: `, run-mobility-stack-dev` must have been run at least once so that
# postgresql-14 + postgis-3 nix packages exist in /nix/store, and the data dir
# (repo-root)/data/db-primary is initialized.
#
# Usage:
#   ./check_migrations.sh                    # full pipeline, all services
#   ./check_migrations.sh --service=rider-app,provider-dashboard
#   ./check_migrations.sh --top-errors       # also print top error categories
#   ./check_migrations.sh --stop             # stop the standalone postgres
#
# Logs:
#   /tmp/pg_migration_check.log              # postgres server log
#   /tmp/seed_run.log                        # seed phase log
#   /tmp/migrate_<service>.log               # per-service migration log

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"            # Backend/ — used to resolve seed/migration paths
REPO_ROOT="$(cd "$ROOT/.." && pwd)"                # repo root (one level above Backend/)

INST=/tmp/pg_install
PORT=5434
SOCKET_DIR="$HOME/NY/socket/db-primary"
DATA_DIR="$REPO_ROOT/data/db-primary"              # data dir lives under repo root, not Backend/
HBA=/tmp/pg_hba_local.conf
SERVER_LOG=/tmp/pg_migration_check.log
PIDFILE=/tmp/pg_migration_check.pid
SEED_LOG=/tmp/seed_run.log

# ---------- visuals ----------
# Disable colors when stdout isn't a TTY or when NO_COLOR is set.
if [ -t 1 ] && [ -z "${NO_COLOR:-}" ]; then
  C_RESET=$'\033[0m'
  C_BOLD=$'\033[1m'
  C_DIM=$'\033[2m'
  C_RED=$'\033[31m'
  C_GREEN=$'\033[32m'
  C_YELLOW=$'\033[33m'
  C_BLUE=$'\033[34m'
  C_MAGENTA=$'\033[35m'
  C_CYAN=$'\033[36m'
  USE_TTY=1
else
  C_RESET=''; C_BOLD=''; C_DIM=''; C_RED=''; C_GREEN=''
  C_YELLOW=''; C_BLUE=''; C_MAGENTA=''; C_CYAN=''
  USE_TTY=0
fi

# Box-drawing helpers (degrade gracefully on non-TTY)
TERM_COLS=$( ( [ "$USE_TTY" = "1" ] && tput cols 2>/dev/null ) || echo 80 )
HR_CHAR='─'; CORNER_TL='╭'; CORNER_TR='╮'; CORNER_BL='╰'; CORNER_BR='╯'; VBAR='│'

hr()      { printf '%s' "${C_DIM}"; printf "${HR_CHAR}%.0s" $(seq 1 "$TERM_COLS"); printf '%s\n' "${C_RESET}"; }
banner()  { # bold cyan section header inside a top-corner ASCII box
  local title="$1"
  local pad=$(( TERM_COLS - ${#title} - 4 ))
  [ $pad -lt 0 ] && pad=0
  printf '%s%s%s %s%s%s ' "${C_CYAN}" "${CORNER_TL}${HR_CHAR}" "${C_RESET}" "${C_BOLD}${C_CYAN}" "$title" "${C_RESET}"
  printf "${C_DIM}${HR_CHAR}%.0s" $(seq 1 "$pad")
  printf '%s%s%s\n' "${C_DIM}" "${CORNER_TR}" "${C_RESET}"
}
phase()   { printf '\n%s▸ %s%s\n' "${C_BOLD}${C_MAGENTA}" "$1" "${C_RESET}"; }
ok()      { printf '  %s✔%s %s\n' "${C_GREEN}" "${C_RESET}" "$1"; }
warn()    { printf '  %s⚠%s %s\n' "${C_YELLOW}" "${C_RESET}" "$1"; }
fail()    { printf '  %s✗%s %s\n' "${C_RED}" "${C_RESET}" "$1"; }
info()    { printf '  %s%s%s\n' "${C_DIM}" "$1" "${C_RESET}"; }

# Spinner: spin <message> while a background pid is alive.
spin() {
  local pid=$1; local msg=$2
  local frames='⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏'
  local i=0
  if [ "$USE_TTY" != "1" ]; then
    wait "$pid" 2>/dev/null; return $?
  fi
  while kill -0 "$pid" 2>/dev/null; do
    local frame=${frames:i:1}
    printf '\r  %s%s%s %s' "${C_CYAN}" "$frame" "${C_RESET}" "$msg"
    i=$(( (i+1) % ${#frames} ))
    sleep 0.1
  done
  printf '\r\033[K'
  wait "$pid" 2>/dev/null
}

# Progress bar: progress <current> <total> <label>
# Clears the line first so a previous longer label can't bleed through, and
# truncates the label so the whole line fits in TERM_COLS (otherwise the
# terminal wraps it onto a second physical line and our \r/\033[K can only
# clear the current line — leaving the wrapped tail behind).
progress() {
  local cur=$1 total=$2 label=$3
  if [ "$USE_TTY" != "1" ]; then return; fi
  local width=30
  local filled=$(( cur * width / (total > 0 ? total : 1) ))
  local empty=$(( width - filled ))
  local bar=''
  [ $filled -gt 0 ] && bar=$(printf "%${filled}s" | tr ' ' '█')
  [ $empty  -gt 0 ] && bar="${bar}$(printf "%${empty}s" | tr ' ' '░')"
  local pct=$(( cur * 100 / (total > 0 ? total : 1) ))
  # Approx fixed-prefix width: 2 spaces + "[" + 30 + "]" + " " + 4 (pct) + " (" + (digits*2 + 1) + ") "
  local digits=${#total}
  local prefix=$(( 2 + 1 + width + 1 + 1 + 4 + 2 + digits*2 + 1 + 2 ))
  local maxlbl=$(( TERM_COLS - prefix - 2 ))
  [ $maxlbl -lt 10 ] && maxlbl=10
  if [ ${#label} -gt $maxlbl ]; then
    label="…${label: -$((maxlbl-1))}"
  fi
  printf '\r\033[K  %s[%s%s%s]%s %3d%% (%d/%d) %s%s%s' \
    "${C_DIM}" "${C_CYAN}" "$bar" "${C_DIM}" "${C_RESET}" \
    "$pct" "$cur" "$total" "${C_DIM}" "$label" "${C_RESET}"
}

die() { fail "$*"; exit 1; }

cmd_stop() {
  if [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
    kill "$(cat "$PIDFILE")" && echo "stopped postgres (pid $(cat "$PIDFILE"))"
    rm -f "$PIDFILE"
  else
    local pids
    pids=$(pgrep -f "$INST/bin/postgres" || true)
    [ -n "$pids" ] && { kill $pids 2>/dev/null; echo "stopped postgres ($pids)"; } || echo "no standalone postgres running"
    rm -f "$PIDFILE"
  fi
}

# ---------- arg parsing ----------
SERVICES_ARG=""
SHOW_TOP_ERRORS=0
for arg in "$@"; do
  case "$arg" in
    --stop|stop) cmd_stop; exit 0 ;;
    --service=*) SERVICES_ARG="${arg#--service=}" ;;
    --top-errors) SHOW_TOP_ERRORS=1 ;;
    -h|--help) sed -n '2,/^set -uo/p' "$0" | sed 's/^# \{0,1\}//; /^set /d'; exit 0 ;;
    *) die "unknown arg: $arg" ;;
  esac
done

# ---------- locate nix paths ----------
PG_PREFIX=$(ls -d /nix/store/*postgresql-14* 2>/dev/null | grep -vE -- '-lib|and-plugins|-doc|-dev$' | head -1)
PG_LIB_PREFIX=$(ls -d /nix/store/*postgresql-14*-lib 2>/dev/null | head -1)
POSTGIS=$(ls -d /nix/store/*postgis-3* 2>/dev/null | head -1)

[ -z "$PG_PREFIX" ]     && die "postgresql-14* not found in /nix/store. Run \`, run-mobility-stack-dev\` once first."
[ -z "$PG_LIB_PREFIX" ] && die "postgresql-14*-lib not found in /nix/store."
[ -z "$POSTGIS" ]       && die "postgis-3* not found in /nix/store. Run \`, run-mobility-stack-dev\` once first."
[ -d "$DATA_DIR" ]      || die "data dir $DATA_DIR not found. Run \`, run-mobility-stack-dev\` once to initialize the cluster."

# ---------- build relocated install with postgis (if not built) ----------
need_build=0
[ -x "$INST/bin/postgres" ] || need_build=1
[ -f "$INST/share/postgresql/extension/postgis.control" ] || need_build=1
[ -f "$INST/lib/postgis-3.so" ] || need_build=1

phase "Setting up standalone postgres+postgis at $INST"
if [ "$need_build" = "1" ]; then
  info "building (one-time): copying postgres binaries + postgis files"
  rm -rf "$INST"
  mkdir -p "$INST/bin" "$INST/share/postgresql/extension" "$INST/lib"
  for b in postgres psql pg_ctl pg_isready createdb dropdb; do
    cp "$PG_PREFIX/bin/$b" "$INST/bin/$b"
  done
  for d in "$PG_PREFIX"/share/postgresql/*; do
    name=$(basename "$d")
    [ "$name" = "extension" ] && continue
    ln -s "$d" "$INST/share/postgresql/$name"
  done
  cp -P "$PG_PREFIX"/share/postgresql/extension/* "$INST/share/postgresql/extension"/
  cp -P "$POSTGIS"/share/postgresql/extension/* "$INST/share/postgresql/extension"/
  cp -P "$PG_LIB_PREFIX"/lib/* "$INST/lib"/ 2>/dev/null || true
  cp -P "$POSTGIS"/lib/* "$INST/lib"/

  # Postgres only consults dynamic_library_path when the module path does not
  # start with $libdir/. Postgis ships .control + .sql files referencing
  # $libdir/postgis-3 etc. — strip that prefix so our $INST/lib dir is searched.
  ( cd "$INST/share/postgresql/extension"
    for f in postgis*.sql postgis*.control; do
      [ -f "$f" ] || continue
      sed -i.bak \
        -e 's|$libdir/postgis_topology-3|postgis_topology-3|g' \
        -e 's|$libdir/postgis_raster-3|postgis_raster-3|g' \
        -e 's|$libdir/postgis-3|postgis-3|g' "$f"
    done
    rm -f ./*.bak )
  ok "install ready at $INST"
else
  ok "install reused (already built)"
fi

PSQL=$INST/bin/psql

# ---------- pg_hba ----------
cat > "$HBA" <<EOF
local all all trust
host all all 127.0.0.1/32 trust
host all all ::1/128 trust
EOF

# ---------- start postgres (idempotent) ----------
phase "Starting postgres on port $PORT"
mkdir -p "$SOCKET_DIR"
if "$INST/bin/pg_isready" -h localhost -p "$PORT" >/dev/null 2>&1; then
  ok "postgres already running (pid $(pgrep -f "$INST/bin/postgres" | head -1))"
else
  ( nohup "$INST/bin/postgres" \
      -D "$DATA_DIR" \
      -p "$PORT" \
      -k "$SOCKET_DIR" \
      -c hba_file="$HBA" \
      -c dynamic_library_path="\$libdir:$INST/lib" \
      > "$SERVER_LOG" 2>&1 &
    echo $! > "$PIDFILE"
  ) &
  ( for _ in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
      sleep 1
      "$INST/bin/pg_isready" -h localhost -p "$PORT" >/dev/null 2>&1 && exit 0
    done
    exit 1
  ) &
  spin $! "waiting for postgres..."
  if "$INST/bin/pg_isready" -h localhost -p "$PORT" >/dev/null 2>&1; then
    ok "postgres ready (pid $(cat "$PIDFILE" 2>/dev/null || pgrep -f "$INST/bin/postgres" | head -1))"
  else
    tail -20 "$SERVER_LOG"
    die "postgres failed to start; see $SERVER_LOG"
  fi
fi

# ---------- ensure atlas_superuser, then drop+recreate atlas_dev ----------
phase "Resetting atlas_dev"
"$INST/bin/psql" -h "$SOCKET_DIR" -p "$PORT" -U "$(whoami)" -d postgres <<'SQL' >/dev/null 2>&1 || true
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname='atlas_superuser') THEN
    CREATE ROLE atlas_superuser WITH LOGIN SUPERUSER PASSWORD 'atlas';
  END IF;
END $$;
SQL

$PSQL -h localhost -p "$PORT" -U atlas_superuser -d postgres -c "DROP DATABASE IF EXISTS atlas_dev;" >/dev/null
$PSQL -h localhost -p "$PORT" -U atlas_superuser -d postgres -c "CREATE DATABASE atlas_dev OWNER atlas_superuser;" >/dev/null
ok "atlas_dev recreated"

# ---------- run seeds in dev-stack order (Backend/nix/services/nammayatri.nix initialDatabases.schemas) ----------
SEEDS=(
  dev/sql-seed/pre-init.sql
  dev/sql-seed/rider-app-seed.sql
  dev/sql-seed/public-transport-rider-platform-seed.sql
  dev/local-testing-data/public-transport-rider-platform.sql
  dev/sql-seed/mock-registry-seed.sql
  dev/local-testing-data/mock-registry.sql
  dev/sql-seed/dynamic-offer-driver-app-seed.sql
  dev/sql-seed/rider-dashboard-seed.sql
  dev/local-testing-data/rider-dashboard.sql
  dev/sql-seed/provider-dashboard-seed.sql
  dev/local-testing-data/provider-dashboard.sql
  dev/sql-seed/unified-dashboard-seed.sql
  dev/sql-seed/safety-dashboard-seed.sql
  dev/local-testing-data/safety-dashboard.sql
  dev/sql-seed/special-zone-seed.sql
  dev/local-testing-data/special-zone.sql
  dev/sql-seed/kaal-chakra-seed.sql
)
: > "$SEED_LOG"
phase "Seeding atlas_dev (${#SEEDS[@]} files, in dev-stack order)"
total=${#SEEDS[@]}
i=0
for s in "${SEEDS[@]}"; do
  i=$((i+1))
  progress "$i" "$total" "$(basename "$s")"
  echo "--- $s ---" >> "$SEED_LOG"
  $PSQL -h localhost -p "$PORT" -U atlas_superuser -d atlas_dev -v ON_ERROR_STOP=0 -q -f "$ROOT/$s" >> "$SEED_LOG" 2>&1 || true
done
[ "$USE_TTY" = "1" ] && printf '\r\033[K'
seed_errs=$(grep -cE '^psql:[^:]*:[0-9]+: ERROR:|^ERROR:' "$SEED_LOG" || true)
if [ "$seed_errs" = "0" ]; then
  ok "seeds: 0 errors   ${C_DIM}log=$SEED_LOG${C_RESET}"
else
  warn "seeds: ${C_YELLOW}$seed_errs${C_RESET} errors (likely cosmetic role-already-exists)   ${C_DIM}log=$SEED_LOG${C_RESET}"
fi

# ---------- run migrations in dhall-config startup order ----------
run_dir() {
  local dir=$1; local LOG=$2
  if [ ! -d "$ROOT/$dir" ]; then
    warn "${dir} ${C_DIM}— SKIP (missing)${C_RESET}"; return
  fi
  local files=()
  while IFS= read -r f; do files+=("$f"); done < <(ls "$ROOT/$dir"/*.sql 2>/dev/null | sort)
  local total=${#files[@]}
  local i=0
  for f in "${files[@]}"; do
    i=$((i+1))
    progress "$i" "$total" "$(basename "$f")"
    $PSQL -h localhost -p "$PORT" -U atlas_superuser -d atlas_dev -v ON_ERROR_STOP=0 -q -f "$f" >> "$LOG" 2>&1 || true
  done
  [ "$USE_TTY" = "1" ] && printf '\r\033[K'
  # Show last numerically-prefixed migration (highest number), if any.
  local last=""
  if [ "$total" -gt 0 ]; then
    last=$(printf '%s\n' "${files[@]##*/}" \
      | grep -E '^[0-9]+' \
      | sed -E 's/^([0-9]+).*/\1/' \
      | awk '{print +$0}' \
      | sort -n | tail -1)
  fi
  if [ -n "$last" ]; then
    printf '    %s•%s %-56s %sfiles=%s%d %s(last=%s)%s\n' \
      "${C_BLUE}" "${C_RESET}" "$dir" "${C_DIM}" "${C_RESET}" "$total" \
      "${C_DIM}" "$last" "${C_RESET}"
  else
    printf '    %s•%s %-56s %sfiles=%s%d\n' \
      "${C_BLUE}" "${C_RESET}" "$dir" "${C_DIM}" "${C_RESET}" "$total"
  fi
}

ERR_PATTERN='^psql:[^:]*:[0-9]+: ERROR:|^ERROR:'

run_service() {
  local svc=$1; shift
  local LOG=/tmp/migrate_${svc}.log
  : > "$LOG"
  phase "$svc"
  for d in "$@"; do run_dir "$d" "$LOG"; done
  local errs
  errs=$(grep -cE "$ERR_PATTERN" "$LOG" || true)
  if [ "$errs" = "0" ]; then
    ok "${C_BOLD}${svc}${C_RESET}: 0 errors   ${C_DIM}log=$LOG${C_RESET}"
  else
    fail "${C_BOLD}${svc}${C_RESET}: ${C_RED}$errs${C_RESET} errors   ${C_DIM}log=$LOG${C_RESET}"
  fi
}

DEFAULT_SERVICES="rider-app dynamic-offer-driver-app provider-dashboard rider-dashboard"
SERVICES="${SERVICES_ARG:-$DEFAULT_SERVICES}"
SERVICES="${SERVICES//,/ }"

for svc in $SERVICES; do
  case "$svc" in
    rider-app)
      run_service rider-app \
        dev/migrations-read-only/rider-app \
        dev/migrations/scheduler \
        dev/migrations/rider-app ;;
    dynamic-offer-driver-app)
      run_service dynamic-offer-driver-app \
        dev/migrations-read-only/dynamic-offer-driver-app \
        dev/migrations/dynamic-offer-driver-app \
        dev/migrations-after-release/dynamic-offer-driver-app ;;
    provider-dashboard)
      run_service provider-dashboard \
        dev/migrations/provider-dashboard \
        dev/migrations-read-only/provider-dashboard ;;
    rider-dashboard)
      run_service rider-dashboard \
        dev/migrations/rider-dashboard \
        dev/migrations-read-only/rider-dashboard ;;
    *) echo "unknown service: $svc (known: $DEFAULT_SERVICES)" >&2; exit 1 ;;
  esac
done

# ---------- summary ----------
echo
banner "Summary"
printf '  %-30s %s%-9s%s %s\n' "service" "${C_BOLD}" "errors" "${C_RESET}" "log"
hr
total_errs=0
for svc in $SERVICES; do
  LOG=/tmp/migrate_${svc}.log
  [ -f "$LOG" ] || continue
  errs=$(grep -cE "$ERR_PATTERN" "$LOG" || true)
  total_errs=$(( total_errs + errs ))
  if [ "$errs" = "0" ]; then
    badge="${C_GREEN}✔${C_RESET} ${C_GREEN}$(printf '%-7s' "$errs")${C_RESET}"
  else
    badge="${C_RED}✗${C_RESET} ${C_RED}$(printf '%-7s' "$errs")${C_RESET}"
  fi
  printf '  %-30s %b %s%s%s\n' "$svc" "$badge" "${C_DIM}" "$LOG" "${C_RESET}"
done
hr
if [ "$total_errs" = "0" ]; then
  ok "${C_BOLD}all clean${C_RESET} — every migration applied without errors"
else
  fail "${C_BOLD}$total_errs total errors${C_RESET} across services — investigate logs above"
fi

if [ "$SHOW_TOP_ERRORS" = "1" ]; then
  echo
  banner "Errors per service"
  ERR_PATTERN='^psql:[^:]*:[0-9]+: ERROR:|^ERROR:'
  NOTICE_PATTERN='^psql:[^:]*:[0-9]+: NOTICE:'
  WARN_PATTERN='^psql:[^:]*:[0-9]+: WARNING:'
  for svc in $SERVICES; do
    LOG=/tmp/migrate_${svc}.log
    [ -f "$LOG" ] || continue
    err_count=$(grep -cE "$ERR_PATTERN" "$LOG" || true)
    notice_count=$(grep -cE "$NOTICE_PATTERN" "$LOG" || true)
    warn_count=$(grep -cE "$WARN_PATTERN" "$LOG" || true)

    [ "$err_count" = "0" ] && [ "$notice_count" = "0" ] && [ "$warn_count" = "0" ] && continue

    printf '\n  %s%s%s   %serrors=%s%d  notices=%d  warnings=%d%s\n' \
      "${C_BOLD}${C_MAGENTA}" "$svc" "${C_RESET}" \
      "${C_DIM}" "${C_RESET}" "$err_count" "$notice_count" "$warn_count" "${C_DIM}${C_RESET}"

    if [ "$err_count" -gt 0 ]; then
      printf '    %sActual errors:%s\n' "${C_RED}${C_BOLD}" "${C_RESET}"
      # Show real ERROR lines: short file:line + message, up to 20 entries
      grep -hE "$ERR_PATTERN" "$LOG" \
        | sed -E 's|^psql:.*/([^/]+/[^:]+):([0-9]+): ERROR:[[:space:]]*|\1:\2  |' \
        | head -20 \
        | awk -v r="$C_RESET" -v red="$C_RED" \
              '{ printf "      %s%s%s\n", red, $0, r }'
      remaining=$(( err_count - 20 ))
      [ "$remaining" -gt 0 ] && printf '      %s… and %d more (see %s)%s\n' "${C_DIM}" "$remaining" "$LOG" "${C_RESET}"

      # Grouped categories (with names redacted) — useful for spotting patterns
      printf '\n    %sCategories:%s\n' "${C_DIM}" "${C_RESET}"
      grep -hE "$ERR_PATTERN" "$LOG" \
        | sed -E 's/^psql:[^:]+:[0-9]+: //; s/"[^"]*"/"X"/g' \
        | sort | uniq -c | sort -rn | head -10 \
        | awk -v dim="$C_DIM" -v r="$C_RESET" \
              '{ printf "      %s%4d%s  %s\n", dim, $1, r, substr($0, index($0,$2)) }'
    fi

    if [ "$notice_count" -gt 0 ]; then
      printf '\n    %sTop NOTICE categories (informational, not errors):%s\n' "${C_DIM}" "${C_RESET}"
      grep -hE "$NOTICE_PATTERN" "$LOG" \
        | sed -E 's/^psql:[^:]+:[0-9]+: //; s/"[^"]*"/"X"/g' \
        | sort | uniq -c | sort -rn | head -5 \
        | awk -v dim="$C_DIM" -v r="$C_RESET" \
              '{ printf "      %s%4d  %s%s\n", dim, $1, substr($0, index($0,$2)), r }'
    fi
  done
fi

echo
info "postgres still running on localhost:$PORT (db=atlas_dev) — stop with: $0 --stop"
