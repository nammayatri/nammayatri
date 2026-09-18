#!/usr/bin/env bash
set -euo pipefail

BACKEND_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REPO_DIR="$(cd "$BACKEND_DIR/.." && pwd)"
PERF_DIR="$BACKEND_DIR/dev/perf"
RUNS_DIR="${PROF_RUNS_DIR:-$BACKEND_DIR/perf-runs}"
TOOLS_LINK="$BACKEND_DIR/dist-perf-tools"
JOBS="${PROF_JOBS:-1}"
HEAP_INTERVAL="${PROF_HEAP_INTERVAL:-0.5}"
PYTHON="${PYTHON:-python3}"
RTS=()
CABAL=()

usage() {
  cat <<'EOF'
Usage: , prof <command> [args]

  build  <exe> [eventlog|profiling]      build <exe> with instrumentation (default: eventlog)
  run    <exe> <mode> [-- <RTS flags>]   build, run from Backend/, Ctrl-C once to stop, then report
  report <run-dir>                       regenerate the reports of a finished run
  list                                   recent runs
  modes                                  what each mode records
  tools                                  fetch the report tools, print their bin dir

Examples:
  , prof run rider-app-exe rts                     GC + scheduler view while you load-test
  , prof run rider-app-exe heap-info               memory growth by source location, near full speed
  , prof run dynamic-offer-driver-app-exe time     cost-centre profile (needs the profiling shell)
  , prof run rider-app-exe rts -- -A64m -qn2       A/B an RTS setting

Env: PROF_JOBS (cabal -j, default 1)   PROF_HEAP_INTERVAL (heap sample seconds, default 0.5)
     PROF_RUNS_DIR (default Backend/perf-runs)   PROF_BIN (run this binary instead of building)
EOF
}

modes() {
  cat <<'EOF'
mode           build      records                                 use it for
time           profiling  cost-centre time + alloc (.prof)        which functions burn CPU / allocate
heap-cc        profiling  live heap by cost-centre stack          which code produced the data that stays live
heap-type      profiling  live heap by type                       what kind of data is growing
heap-retainer  profiling  live heap by retainer set (slow)        who keeps that data alive
heap-info      eventlog   live heap by info table + source line   memory growth at near-normal speed
heap-closure   eventlog   live heap by closure type               cheapest first look at memory growth
rts            eventlog   GC + scheduler events                   slow under load: GC, idle cores, blocked threads

Every run also records per-GC statistics (gc.log) and the RTS summary.
The profiling build needs the profiling dev shell:  NY_PROFILING=1 nix develop --impure .#backend
EOF
}

die() {
  echo "prof: $*" >&2
  exit 1
}

flavour_of() {
  case "$1" in
    time | heap-cc | heap-type | heap-retainer) echo profiling ;;
    heap-info | heap-closure | rts) echo eventlog ;;
    *) die "unknown mode '$1' (see ', prof modes')" ;;
  esac
}

set_rts_flags() {
  local mode=$1 stem=$2
  case "$mode" in
    time) RTS=(-p "-po$stem" -l-agu "-ol$stem.eventlog") ;;
    heap-cc) RTS=(-hc "-i$HEAP_INTERVAL" "-po$stem" -l-agu "-ol$stem.eventlog") ;;
    heap-type) RTS=(-hy "-i$HEAP_INTERVAL" "-po$stem" -l-agu "-ol$stem.eventlog") ;;
    heap-retainer) RTS=(-hr "-i$HEAP_INTERVAL" "-po$stem" -l-agu "-ol$stem.eventlog") ;;
    heap-info) RTS=(-hi "-i$HEAP_INTERVAL" "-po$stem" -l-agu "-ol$stem.eventlog") ;;
    heap-closure) RTS=(-hT "-i$HEAP_INTERVAL" "-po$stem" -l-agu "-ol$stem.eventlog") ;;
    rts) RTS=(-l "-ol$stem.eventlog") ;;
  esac
}

set_cabal_flags() {
  CABAL=(--project-file="cabal.project.$1" --builddir="dist-$1")
}

tools_bin() {
  if [ ! -x "$TOOLS_LINK/bin/eventlog2html" ]; then
    echo "prof: fetching the report tools (one-time)" >&2
    nix build --accept-flake-config --option warn-dirty false --out-link "$TOOLS_LINK" "$REPO_DIR#profiling-tools" >&2
  fi
  echo "$TOOLS_LINK/bin"
}

check_profiling_libs() {
  local dir
  for dir in $(ghc-pkg field mobility-core library-dirs --simple-output 2>/dev/null || true); do
    if compgen -G "$dir/libHSmobility-core-*_p.a" >/dev/null; then
      return 0
    fi
  done
  cat >&2 <<'EOF'
prof: this dev shell has no profiling libraries for mobility-core and the other
      haskell-flake-built deps, so a profiling build cannot link.
      Enter the profiling shell (one-time nix build, cached afterwards) and retry:

        NY_PROFILING=1 nix develop --impure .#backend
EOF
  exit 1
}

link_local_config() {
  if [ -f "$BACKEND_DIR/cabal.project.local" ] && [ ! -e "$BACKEND_DIR/cabal.project.$1.local" ]; then
    ln -s cabal.project.local "$BACKEND_DIR/cabal.project.$1.local"
    echo "prof: linked cabal.project.$1.local -> cabal.project.local"
  fi
}

build() {
  [ $# -ge 1 ] || die "usage: , prof build <exe> [eventlog|profiling]"
  local exe=$1 flavour=${2:-eventlog}
  case "$flavour" in
    eventlog | profiling) ;;
    *) die "build flavour must be eventlog or profiling" ;;
  esac
  cd "$BACKEND_DIR"
  if [ "$flavour" = profiling ]; then
    check_profiling_libs
  fi
  link_local_config "$flavour"
  set_cabal_flags "$flavour"
  echo "prof: building exe:$exe ($flavour build in dist-$flavour, cabal -j$JOBS)"
  cabal build -j"$JOBS" "${CABAL[@]}" "exe:$exe"
}

binary_for() {
  local exe=$1 flavour=$2
  if [ -n "${PROF_BIN:-}" ]; then
    echo "$PROF_BIN"
    return
  fi
  build "$exe" "$flavour" >&2
  set_cabal_flags "$flavour"
  (cd "$BACKEND_DIR" && cabal list-bin -v0 "${CABAL[@]}" "exe:$exe")
}

run() {
  [ $# -ge 2 ] || die "usage: , prof run <exe> <mode> [-- <RTS flags>]"
  local exe=$1 mode=$2 flavour bin dir rc=0
  shift 2
  if [ "${1:-}" = "--" ]; then
    shift
  fi
  flavour=$(flavour_of "$mode")
  bin=$(binary_for "$exe" "$flavour")
  [ -x "$bin" ] || die "binary not found: $bin"
  dir="$RUNS_DIR/$exe/$(date +%Y%m%d-%H%M%S)-$mode"
  mkdir -p "$dir"
  set_rts_flags "$mode" "$dir/$exe"
  RTS+=("-S$dir/gc.log" "$@")
  echo "$exe" >"$dir/exe"
  echo "$mode" >"$dir/mode"
  printf '%q ' "$bin" +RTS "${RTS[@]}" -RTS >"$dir/command"
  echo >>"$dir/command"
  cat <<EOF
prof: $exe ($mode) starting from $BACKEND_DIR
prof: run dir  $dir
prof: app log  tail -f $dir/app.log
prof: once it is up, drive traffic (, perf-ramp / , perf-watch), then press Ctrl-C ONCE.
      The app shuts down gracefully (up to its graceTerminationPeriod) and the reports follow.
EOF
  cd "$BACKEND_DIR"
  trap ':' INT TERM
  "$bin" +RTS "${RTS[@]}" -RTS >"$dir/app.log" 2>&1 || rc=$?
  trap - INT TERM
  echo "prof: $exe exited with status $rc"
  report "$dir"
}

report() {
  [ $# -eq 1 ] || die "usage: , prof report <run-dir>"
  local dir exe mode bin summary ev
  dir=$(cd "$1" && pwd)
  exe=$(cat "$dir/exe")
  mode=$(cat "$dir/mode")
  bin=$(tools_bin)
  summary="$dir/SUMMARY.txt"
  ev="$dir/$exe.eventlog"
  {
    echo "run   $dir"
    echo "exe   $exe"
    echo "mode  $mode"
    echo "cmd   $(cat "$dir/command")"
    echo
  } >"$summary"

  if [ -s "$dir/$exe.prof" ]; then
    "$bin/ghc-prof-flamegraph" "$dir/$exe.prof" -o "$dir/flamegraph-time.svg" >/dev/null
    "$bin/ghc-prof-flamegraph" --alloc "$dir/$exe.prof" -o "$dir/flamegraph-alloc.svg" >/dev/null
    {
      echo "== top cost centres ($exe.prof)"
      awk '/^COST CENTRE/ && !seen {seen=1; print; next} seen && /^$/ {if (++blank == 2) exit; next} seen {print}' \
        "$dir/$exe.prof" | head -45
      echo
    } >>"$summary"
  fi

  case "$mode" in
    time)
      if [ -s "$ev" ]; then
        "$bin/hs-speedscope" "$ev" >/dev/null 2>&1 || echo "prof: hs-speedscope could not convert $ev" >&2
      fi
      ;;
    rts)
      if [ -s "$ev" ]; then
        "$bin/ghc-events" show "$ev" | "$PYTHON" "$PERF_DIR/eventlog_summary.py" - >>"$summary"
        echo >>"$summary"
      fi
      ;;
    heap-*)
      if ! { [ -s "$ev" ] && "$bin/eventlog2html" -o "$dir/heap.html" "$ev" >/dev/null 2>&1; }; then
        if [ -s "$dir/$exe.hp" ]; then
          "$bin/eventlog2html" -p -o "$dir/heap.html" "$dir/$exe.hp" >/dev/null
        fi
      fi
      ;;
  esac

  if [ -s "$dir/gc.log" ]; then
    "$PYTHON" "$PERF_DIR/gc_summary.py" "$dir/gc.log" >>"$summary"
  fi

  {
    echo
    echo "== artifacts in $dir"
    if [ -f "$dir/flamegraph-time.svg" ]; then
      echo "  flamegraph-time.svg, flamegraph-alloc.svg  open in a browser; click a frame to zoom"
    fi
    if [ -f "$ev.json" ]; then
      echo "  $exe.eventlog.json  drag into https://www.speedscope.app (Left Heavy / Sandwich views)"
    fi
    if [ -f "$dir/heap.html" ]; then
      echo "  heap.html  open in a browser; each band is a top heap consumer over time, Detailed tab lists all"
    fi
    if [ -f "$dir/$exe.prof" ]; then
      echo "  $exe.prof  full cost-centre tree with inherited %time / %alloc per call path"
    fi
    echo "  gc.log  one line per GC (alloc, copied, live, pause)"
    echo "  app.log  the app's own output"
  } >>"$summary"
  cat "$summary"
}

list() {
  if [ -d "$RUNS_DIR" ]; then
    ls -1dt "$RUNS_DIR"/*/*/ 2>/dev/null | head -20
  else
    echo "no runs yet in $RUNS_DIR"
  fi
}

cmd=${1:-help}
if [ $# -gt 0 ]; then
  shift
fi
case "$cmd" in
  build) build "$@" ;;
  run) run "$@" ;;
  report) report "$@" ;;
  list) list ;;
  modes) modes ;;
  tools) tools_bin ;;
  help | -h | --help) usage ;;
  *)
    usage
    exit 1
    ;;
esac
