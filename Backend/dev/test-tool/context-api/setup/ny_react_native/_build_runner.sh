#!/usr/bin/env bash
# Inner build runner — exec'd inside the nix shell from
# ny-react-native-setup.sh. Reads everything from env vars (no positional args).
set -euo pipefail
# Inputs (exported by the outer bash shim before exec):
#   NY_RN_DIR NY_RN_PLATFORM NY_RN_APP NY_RN_VARIANT REPO_ROOT NY_RN_APPS
: "${NY_RN_DIR:?}" "${NY_RN_PLATFORM:?}" "${NY_RN_APP:?}" "${NY_RN_VARIANT:?}" "${REPO_ROOT:?}"
IFS=" " read -r -a APPS <<< "${NY_RN_APPS:?}"

# Resolve and pin JAVA_HOME so gradle picks JDK 17 instead of any
# system-installed (newer) JDK. nix puts `java` on PATH; we walk up
# the symlink chain to find the JDK root. -- bash readlink -f is
# fine on macOS via the coreutils shipped with nix.
if command -v java >/dev/null; then
  JAVA_BIN="$(command -v java)"
  JAVA_REAL="$(readlink -f "$JAVA_BIN" 2>/dev/null || echo "$JAVA_BIN")"
  export JAVA_HOME="$(dirname "$(dirname "$JAVA_REAL")")"
  echo "ny-react-native: JAVA_HOME=$JAVA_HOME ($(java -version 2>&1 | head -n1))"
fi

echo "ny-react-native: node=$(node --version)  yarn=$(yarn --version)"

# Pin yarn1 to OUR nodejs_22 (workaround for nix-packaged yarn).
#
# `nixpkgs-unstable#yarn` is built on top of `nodejs-slim-24` and
# embeds an absolute path to that node binary. Every JS subprocess
# yarn spawns — including node-gyp for native addons like xxhash —
# uses Node 24, regardless of what's first in PATH. The provider's
# install.sh pulls in `xxhash@0.3.0` (a NAN addon from 2017) which
# fails to compile against Node 24's v8 headers (ExternalPointerTagRange
# template was added in Node 23+).
#
# Verify locally with: `yarn node --version` → prints v24.x even
# when `node --version` prints v22.x.
#
# Fix: install yarn1 via npm into a launcher-local prefix. The npm
# we use comes with nodejs_22, so the resulting yarn binary is
# wired to nodejs_22 — every subprocess it spawns (incl. node-gyp)
# now sees Node 22 v8 headers, which xxhash can compile against.
ensure_yarn_for_nodejs22() {
  local local_prefix="$NY_RN_DIR/.tmp-yarn-bin"
  if ! [ -x "$local_prefix/bin/yarn" ]; then
    echo "ny-react-native: installing yarn1 under nodejs_22 (workaround for nix-yarn's bundled nodejs-slim-24)"
    mkdir -p "$local_prefix"
    npm install -g --prefix "$local_prefix" --no-fund --no-audit yarn@1.22.22 2>&1 \
      | sed "s/^/  npm: /"
  fi
  export PATH="$local_prefix/bin:$PATH"
  hash -r
  local yarn_node_ver
  yarn_node_ver="$(yarn node --version 2>/dev/null || echo unknown)"
  echo "ny-react-native: yarn now spawns node $yarn_node_ver (was v24.x via nix-yarn's bundled nodejs-slim)"
}
ensure_yarn_for_nodejs22

# Stage-1 refactor: most data + helper logic lives under
# Backend/dev/test-tool/context-api/setup/ny_react_native/. The bash
# below shells out to these instead of carrying inline heredocs.
SETUP_PY_DIR="$REPO_ROOT/Backend/dev/test-tool/context-api/setup/ny_react_native"
SETUP_TEMPLATES="$SETUP_PY_DIR/templates"

write_consumer_local_properties() {
  local target="$1/android/local.properties"
  if [ -f "$target" ]; then
    echo "ny-react-native: keeping existing $target"
    return 0
  fi
  echo "ny-react-native: writing default $target"
  local sdk="${ANDROID_HOME:-$HOME/Library/Android/sdk}"
  sed "s|__ANDROID_HOME__|$sdk|" "$SETUP_TEMPLATES/consumer-local.properties.tmpl" > "$target"
}

# Drop a multi-client placeholder google-services.json under the app/
# directory if a real one is not already present. Lets the
# com.google.gms.google-services Gradle plugin resolve a client matching
# whichever flavor.applicationId is being built without us needing real
# Firebase credentials. Firebase calls at runtime will obviously fail —
# this is dev-build only, not for testing Firebase features.
# Stage-1 refactor: the bodies of these helpers used to be ~250 lines
# of inline Python heredoc + XML. They now live as standalone files
# under setup/ny_react_native/{common,templates}/ — see the README
# there for the full migration plan.

write_dummy_google_services_json() {
  python3 "$SETUP_PY_DIR/common/google_services_json.py" "$1"
}

write_dummy_google_services_plist() {
  # $1 = ios dir  $2 = optional bundle id to stamp into BUNDLE_ID.
  # Without the right BUNDLE_ID, Firebase aborts hard on launch with
  # SIGABRT in +[FIRApp configure] — the build still "succeeds" so the
  # crash is invisible from the build log alone.
  if [ -n "${2:-}" ]; then
    python3 "$SETUP_PY_DIR/common/google_services_plist.py" "$1" "$2"
  else
    python3 "$SETUP_PY_DIR/common/google_services_plist.py" "$1"
  fi
}

# Resolve the iOS PRODUCT_BUNDLE_IDENTIFIER for a given (consumer) variant
# scheme like "Lynx-Debug". We grep the pbxproj rather than calling
# `xcodebuild -showBuildSettings` because the latter requires Pods to be
# installed (we need the bundle id BEFORE pod install so the right plist
# is in place when xcodebuild's Firebase-copy phase runs).
ios_resolve_variant_bundle_id() {
  # Resolve PRODUCT_BUNDLE_IDENTIFIER for an iOS variant scheme like
  # `Lynx-Debug`. Uses xcodebuild's own -showBuildSettings rather than
  # parsing the pbxproj — the schemes here all map BlueprintName=<Brand>
  # + buildConfiguration=Debug, but PRODUCT_BUNDLE_IDENTIFIER lives on
  # the per-target XCBuildConfiguration entry referenced through the
  # target's XCConfigurationList. A naive pbxproj regex sees only
  # `name = Debug` (not the full `Lynx-Debug` scheme name) and so can't
  # tell different targets' Debug configs apart. xcodebuild walks the
  # whole graph correctly. Costs ~2s per call (worth it for correctness).
  local app_dir="$1" scheme="$2"
  # The two repos use different workspace names — consumer ships
  # Nammayatri.xcworkspace, provider ships provider.xcworkspace. Don't
  # hardcode either; pick whichever .xcworkspace lives in <app_dir>/ios/.
  # (Earlier the hardcoded "Nammayatri.xcworkspace" worked for consumer
  # but silently returned empty for provider, which made the post-launch
  # crash check fall through to a `simctl listapps | tail -n1` heuristic
  # — that picks the lexicographically-last installed user app on the
  # sim, frequently a stale `in.mobility.nammayatri.debug` from yesterday's
  # build, and reports a phantom crash.)
  local workspace
  workspace=$(ls -1d "$app_dir/ios/"*.xcworkspace 2>/dev/null | head -n1)
  if [ -z "$workspace" ]; then
    return 0
  fi
  ios_clean_env_run xcodebuild \
    -workspace "$workspace" \
    -scheme "$scheme" -configuration Debug \
    -showBuildSettings 2>/dev/null \
    | awk '/^[[:space:]]*PRODUCT_BUNDLE_IDENTIFIER[[:space:]]*=/ {
        sub(/^[[:space:]]*PRODUCT_BUNDLE_IDENTIFIER[[:space:]]*=[[:space:]]*/, "")
        print; exit
      }'
}

ensure_admob_debug_manifest() {
  local app_dir="$1"
  local debug_dir="$app_dir/android/app/src/debug"
  local manifest="$debug_dir/AndroidManifest.xml"
  mkdir -p "$debug_dir"
  echo "ny-react-native: writing debug AndroidManifest.xml with AdMob + Firebase opt-outs at $manifest"
  cp "$SETUP_TEMPLATES/AndroidManifest.debug.xml" "$manifest"
}

ensure_admob_app_ids() {
  python3 "$SETUP_PY_DIR/common/admob_app_ids.py" "$1"
}

patch_consumer_base_url_defaults() {
  python3 "$SETUP_PY_DIR/common/patch_base_url.py" "$1" "$2"
}

write_local_base_url() {
  python3 "$SETUP_PY_DIR/common/write_local_base_url.py" "$1" "$2"
}

write_provider_local_properties() {
  local target="$1/android/local.properties"
  if [ -f "$target" ]; then
    echo "ny-react-native: keeping existing $target"
    return 0
  fi
  echo "ny-react-native: writing default $target"
  cat > "$target" <<EOF
sdk.dir=${ANDROID_HOME:-$HOME/Library/Android/sdk}
MAPS_API_KEY=""
CONFIG_URL_DRIVER="http://localhost:8016"
CONFIG_URL_USER="http://localhost:8013"
MERCHANT_ID_USER="NAMMA_YATRI"
MERCHANT_ID_DRIVER="NAMMA_YATRI_PARTNER"
FACEBOOK_APP_ID=""
FACEBOOK_CLIENT_TOKEN=""
FB_LOGIN_PROTOCOL_SCHEMA=""
CLEVERTAP_ACCOUNT_ID=""
CLEVERTAP_ACCOUNT_TOKEN=""
RS_ENC_KEY=""
RS_ALGO=""
EOF
}

# ── Emulator preflight ──────────────────────────────────────────────
if [ "$NY_RN_PLATFORM" = "android" ]; then
  if ! command -v adb >/dev/null; then
    echo "ny-react-native: ERROR adb not on PATH (set ANDROID_HOME / install platform-tools)"
    exit 3
  fi
  # Reset adb daemon to clear any stale connection state from a prior
  # run (e.g. zombie emulator entry, version mismatch with system adb).
  # This is the most common cause of ddmlib ShellCommandUnresponsive /
  # TimeoutException during gradle :installXxxDebug.
  echo "ny-react-native: restarting adb daemon"
  adb kill-server >/dev/null 2>&1 || true
  adb start-server >/dev/null 2>&1 || true

  if ! adb devices | grep -q "emulator-"; then
    if ! command -v emulator >/dev/null; then
      echo "ny-react-native: ERROR no emulator running and emulator binary not on PATH"
      exit 3
    fi
    AVD="$(emulator -list-avds | head -n1 || true)"
    if [ -z "$AVD" ]; then
      echo "ny-react-native: ERROR no AVD configured (run: avdmanager create avd ...)"
      exit 3
    fi
    echo "ny-react-native: booting emulator $AVD (using snapshot if available)"
    # Note: NO -no-snapshot-load. Letting the emulator restore its
    # snapshot drops boot time from ~90s to ~10s on Apple Silicon and
    # leaves CPU headroom for gradle, which avoids the
    # ShellCommandUnresponsiveException tower during :installXxxDebug.
    nohup emulator -avd "$AVD" >/tmp/ny-rn-emulator.log 2>&1 &
    adb wait-for-device
    # First wait for sys.boot_completed (init.rc finished), then for
    # adb shell to actually answer real commands — system_server can
    # still be busy after boot_completed, which is exactly when
    # gradle ddmlib starts to time out.
    BOOT_DEADLINE=$(( $(date +%s) + 180 ))
    while [ "$(adb shell getprop sys.boot_completed 2>/dev/null | tr -d "\\r")" != "1" ]; do
      if [ "$(date +%s)" -gt "$BOOT_DEADLINE" ]; then
        echo "ny-react-native: ERROR emulator boot_completed timeout (180s)"
        exit 3
      fi
      sleep 2
    done
    SHELL_DEADLINE=$(( $(date +%s) + 90 ))
    until adb shell pm list packages -s >/dev/null 2>&1; do
      if [ "$(date +%s)" -gt "$SHELL_DEADLINE" ]; then
        echo "ny-react-native: ERROR adb shell unresponsive after boot (90s)"
        exit 3
      fi
      sleep 2
    done
    echo "ny-react-native: emulator $AVD ready (adb shell is responsive)"
  else
    echo "ny-react-native: existing emulator detected"
  fi
  # In an Android emulator, `localhost` points to the emulator itself,
  # NOT the host machine. Two ways to reach the host backend:
  #   (a) `adb reverse tcp:N tcp:N`  — emulator localhost:N → host localhost:N
  #   (b) special alias 10.0.2.2     — always points to host loopback
  # We use (a) so the same BASE_URL ("http://localhost:N/...") works on
  # both Android emulators AND iOS Simulators (which share network with
  # the host natively). Reverses are lost on adb daemon restart / device
  # reconnect, so this helper is also called again right before launch.
  apply_adb_reverses() {
    local label="$1"
    # Metro ports: 8088 (customer) / 8089 (driver). Dodges every
    # reservation in Backend/nix/services/ports.nix — most importantly
    # 8081 (location-tracking-service); pointing the emulator's
    # localhost:8081 at host:8081 would route Metro requests INTO LTS.
    adb reverse tcp:8088  tcp:8088  >/dev/null 2>&1 || true   # Metro (customer)
    adb reverse tcp:8089  tcp:8089  >/dev/null 2>&1 || true   # Metro (driver)
    adb reverse tcp:8013  tcp:8013  >/dev/null 2>&1 || true   # rider-app HTTP (BAP)
    adb reverse tcp:8016  tcp:8016  >/dev/null 2>&1 || true   # driver-app HTTP (BPP)
    adb reverse tcp:50051 tcp:50051 >/dev/null 2>&1 || true   # notification-service gRPC
    echo "ny-react-native: adb reverse wired ($label) — current forwards:"
    adb reverse --list 2>/dev/null | sed "s/^/  /" || true
  }
  apply_adb_reverses "post-emulator-boot"
else
  if ! command -v xcrun >/dev/null; then
    echo "ny-react-native: ERROR xcrun not on PATH (Xcode required for iOS)"
    exit 3
  fi
  if ! xcrun simctl list devices booted 2>/dev/null | grep -q "Booted"; then
    UDID="$(xcrun simctl list devices available -j 2>/dev/null \
      | python3 -c "import sys,json; d=json.load(sys.stdin); print(next((x[\"udid\"] for runtime in d[\"devices\"].values() for x in runtime if x.get(\"isAvailable\")), \"\"))")"
    if [ -z "$UDID" ]; then
      echo "ny-react-native: ERROR no iOS Simulator devices available"
      exit 3
    fi
    echo "ny-react-native: booting iOS Simulator $UDID"
    xcrun simctl boot "$UDID" || true
    open -a Simulator
  else
    echo "ny-react-native: existing iOS Simulator detected"
  fi
  # Capture the booted Simulator UDID so we pass --udid to
  # `react-native run-ios` later. Without this, RN CLI auto-picks the
  # first connected device — and if the dev has a real iPhone plugged
  # in (or paired wirelessly), it will try to build/sign against that
  # device. That requires a configured DEVELOPMENT_TEAM + provisioning
  # profile, which the launcher does not set up; the result is
  # `xcodebuild exited with error code 65` deep in code-signing with
  # no useful log because RN CLI hides the build output.
  IOS_SIM_UDID="$(xcrun simctl list devices booted -j 2>/dev/null \
    | python3 -c "import sys,json; d=json.load(sys.stdin); print(next((x[\"udid\"] for r in d[\"devices\"].values() for x in r if x.get(\"state\")==\"Booted\"), \"\"))" || true)"
  if [ -n "$IOS_SIM_UDID" ]; then
    echo "ny-react-native: targeting iOS Simulator $IOS_SIM_UDID (skipping any connected real device)"
    export IOS_SIM_UDID
  else
    echo "ny-react-native: WARN could not resolve booted Simulator UDID — RN CLI may pick a real device and fail at code-signing"
  fi
fi

# When the launcher runs inside the nammayatri nix dev shell (the common
# case — test-context-api itself is started from there), CC/CXX point at
# nix's clang-11 wrapper and PATH puts it ahead of /usr/bin/clang.
# CocoaPods + xcodebuild Pod targets inherit `CC=clang` and resolve to
# nix-clang-11, which doesn't accept `-index-store-path` — every Pod
# target then fails with "unknown argument: '-index-store-path'" and
# xcodebuild aborts with exit 65. Run every iOS-touching command (bundle,
# pod, react-native run-ios) through this wrapper so Apple's toolchain
# inside DEVELOPER_DIR wins.
ios_clean_env_run() {
  local apple_dev
  apple_dev="$(/usr/bin/xcode-select -p 2>/dev/null || echo /Applications/Xcode.app/Contents/Developer)"
  local extra_path="/usr/bin:/bin"
  # The Gemfile.lock on this branch pins bundler 2.6.9 — system Ruby 2.6
  # cannot satisfy that. If brew Ruby is present (the README's recommended
  # install), prepend it so `bundle` resolves to a Ruby that has 2.6.9.
  if [ -x /opt/homebrew/opt/ruby/bin/ruby ]; then
    extra_path="/opt/homebrew/opt/ruby/bin:$extra_path"
  fi
  env -u CC -u CXX -u CPATH -u LIBRARY_PATH -u LDFLAGS \
      -u NIX_CFLAGS_COMPILE -u NIX_CFLAGS_LINK -u NIX_LDFLAGS \
      DEVELOPER_DIR="$apple_dev" \
      PATH="${extra_path}:${PATH}" \
      "$@"
}
export -f ios_clean_env_run 2>/dev/null || true

# Compact xcodebuild stream → human-readable progress.
#
# Without this, `react-native run-ios` (with CI=true to disable the
# animated spinner) dumps the raw xcodebuild firehose: every Pod
# compilation flushes a multi-K-char clang invocation line to stdout,
# which in turn drowns out the actual signal — target banners, file-
# level Compile*/Ld/CodeSign events, and errors. The dashboard's Build
# tab then becomes thousands of identical "- Building the app..." lines
# (RN CLI's pre-CI-flag fallback) or thousands of identical clang flag
# blobs — neither of which tell you whether the build is progressing or
# stuck.
#
# This filter:
#   - DROPs the noisy compiler-invocation lines (any line containing
#     -isysroot or -fmodule-name=, plus shell-prelude lines like `cd /…`
#     and `export FOO=...`),
#   - DROPs RN CLI's animated `- Building the app …` spinner frames,
#   - KEEPs the high-signal events: `=== BUILD TARGET …`, `CompileC`,
#     `CompileSwift`, `Ld`, `Libtool`, `CodeSign`, `PhaseScriptExecution`,
#     `Touch`, `ProcessPCH`, etc. + every error/warning/note + the final
#     `** BUILD SUCCEEDED **` / `** BUILD FAILED **` banner + RN CLI's
#     own `info` / `success` / `error` lines (Xcode workspace found,
#     Installing on simulator, Launching, etc.),
#   - PREFIXes every kept line with elapsed seconds since this filter
#     started — so a 30-second gap between events is immediately visible
#     instead of looking like a stall.
ios_pretty_log_filter() {
  perl -ne '
    BEGIN { $| = 1; $start = time; }
    next if /-isysroot|-fmodule-name=|--serialize-diagnostics/;
    next if m{^\s*/.*?/(?:clang|clang\+\+|swift|swiftc|swift-frontend|ld|libtool)\s};
    next if /^\s*export\s+\w+=/;
    next if m{^\s*cd\s+/};
    next if /Building the app/;
    next if /^\s*$/;
    if (/^(?:=== BUILD|CompileC|CompileSwift|MergeSwiftModule|EmitSwiftModule|Ld\s|Libtool\s|Touch\s|CodeSign|PhaseScriptExecution|ProcessPCH|ProcessInfoPlistFile|WriteAuxiliaryFile|GenerateDSYMFile|ScanDependencies|RegisterExecutionPolicyException|ExtractAppIntentsMetadata|ProcessProductPackaging|Check dependencies)/ ||
        /^(?:info|warn|warning|error|success|debug)\s/ ||
        /\b(?:error|warning|note):/ ||
        /\*\* (?:BUILD|TEST|CLEAN|ARCHIVE)/ ||
        /^Failed/ ||
        /^The following build commands failed/) {
      my $line = $_;
      $line =~ s/\r?\n?$//;
      printf "[%4ds] %s\n", time - $start, $line;
    }
  '
}

# Wrap an iOS build invocation so:
#   - --verbose disables RN CLI's animated `ora` progress spinner so we
#     get real per-target xcodebuild lines instead of a wall of
#     `- Building the app......` dots. (CI=true alone does NOT disable
#     the spinner for `run-ios`: in a 30-min build the raw log was
#     ~5,200 spinner dots vs ~94 informative lines, starving the filter
#     and making the dashboard look frozen even while xcodebuild was
#     happily compiling 200+ Pods. --verbose forces RN CLI to forward
#     xcodebuild's stdout directly.)
#   - the scrubbed iOS env (Apple clang) is applied,
#   - raw output is tee'd to /tmp/ny-rn-xcodebuild-<app>.log for forensics,
#   - filtered/pretty output streams to the dashboard Build tab.
# Returns the exit status of the wrapped command (not the filter).
ios_run_with_logs() {
  local raw_log="/tmp/ny-rn-xcodebuild-${app:-unknown}.log"
  echo "ny-react-native: streaming filtered xcodebuild log → dashboard"
  echo "ny-react-native: full raw log at $raw_log"
  # Inject --verbose immediately after the `run-ios` subcommand so it
  # isn't parsed as a positional arg. We splice into the arg list rather
  # than appending to "$@".
  local cmd=()
  local injected=0
  for tok in "$@"; do
    cmd+=("$tok")
    if [ "$injected" -eq 0 ] && [ "$tok" = "run-ios" ]; then
      cmd+=("--verbose")
      injected=1
    fi
  done
  set -o pipefail
  CI=true ios_clean_env_run "${cmd[@]}" 2>&1 \
    | tee "$raw_log" \
    | ios_pretty_log_filter
  local rc=${PIPESTATUS[0]}
  set +o pipefail
  return "$rc"
}
export -f ios_pretty_log_filter ios_run_with_logs 2>/dev/null || true

# ── Per-app prebuild ────────────────────────────────────────────────
for app in "${APPS[@]}"; do
  APP_DIR="$NY_RN_DIR/$app"
  echo ""
  echo "ny-react-native: ── prebuild $app ──"
  cd "$APP_DIR"

  echo "ny-react-native: $app · yarn install"
  yarn install --frozen-lockfile

  if [ "$app" = "consumer" ]; then
    echo "ny-react-native: consumer · ReScript build"
    yarn re:build || yarn run rescript build
    # Compile the workspace TS packages the consumer imports. The
    # repo s `re:start` does this on every dev launch; we have to
    # do it explicitly because we run `re:build` (rescript only).
    # Without this, Metro 500s on `cannot resolve module config-types`
    # because libs/config-types/dist/ does not exist yet.
    echo "ny-react-native: consumer · tsc libs/config-types + libs/custom-linters"
    ( cd "$NY_RN_DIR/consumer" \
      && npx tsc -p ../libs/config-types 2>&1 | sed "s/^/  config-types: /" || true ) || true
    ( cd "$NY_RN_DIR/consumer" \
      && npx tsc -p ../libs/custom-linters 2>&1 | sed "s/^/  custom-linters: /" || true ) || true
    write_consumer_local_properties "$APP_DIR"
    write_dummy_google_services_json "$APP_DIR/android/app"
    # README step 4.2.5: GoogleService-Info.plist must exist in
    # consumer/ios/ AND consumer/ios/<Variant>/Plist/. Missing on a
    # fresh checkout — without it xcodebuild dies with code 65 in the
    # Firebase copy-plist phase before any compile output is shown.
    #
    # On iOS, also resolve the chosen variant's actual
    # PRODUCT_BUNDLE_IDENTIFIER (e.g. Lynx-Debug → in.mobility.international)
    # and stamp it into BUNDLE_ID so Firebase's auto-init does not
    # SIGABRT on a bundle-id mismatch the moment the app launches. This
    # is the difference between "build succeeded but app crashes < 2s
    # after splash" and a real launch.
    if [ "$NY_RN_PLATFORM" = "ios" ]; then
      variant_pascal_for_plist="$(printf %s "$NY_RN_VARIANT" | python3 -c "import sys; s=sys.stdin.read().strip(); print((s[:1].upper()+s[1:]) if s else s)")"
      consumer_bundle_id="$(ios_resolve_variant_bundle_id "$APP_DIR" "${variant_pascal_for_plist}-Debug")"
      if [ -n "$consumer_bundle_id" ]; then
        echo "ny-react-native: consumer iOS variant ${variant_pascal_for_plist}-Debug → bundle id $consumer_bundle_id"
      else
        echo "ny-react-native: WARN could not resolve PRODUCT_BUNDLE_IDENTIFIER for ${variant_pascal_for_plist}-Debug — Firebase may SIGABRT on launch (bundle-id mismatch)"
      fi
      write_dummy_google_services_plist "$APP_DIR/ios" "${consumer_bundle_id:-}"
    else
      write_dummy_google_services_plist "$APP_DIR/ios"
    fi
    ensure_admob_app_ids "$APP_DIR"
    ensure_admob_debug_manifest "$APP_DIR"
    write_local_base_url "$APP_DIR" "http://localhost:8013/v2"
    # Override the hardcoded `consumerBaseUrl` defaults too — App.tsx
    # writes this into MMKV on every launch, otherwise the .env BASE_URL
    # only takes effect for one boot before being overwritten.
    patch_consumer_base_url_defaults "$APP_DIR" "http://localhost:8013/v2"
  else
    write_provider_local_properties "$APP_DIR"
    write_dummy_google_services_json "$APP_DIR/android/app"
    # Same Firebase BUNDLE_ID-match concern as consumer above. The
    # provider's Lynx-Debug variant launches as in.mobility.lynxdriver
    # (NOT in.juspay.nammayatri.dev like the placeholder), so
    # +[FIRApp configure] SIGABRTs <2s after launch unless the
    # GoogleService-Info.plist's BUNDLE_ID matches the variant's
    # PRODUCT_BUNDLE_IDENTIFIER. Resolve it the same way consumer does.
    if [ "$NY_RN_PLATFORM" = "ios" ]; then
      variant_pascal_for_plist="$(printf %s "$NY_RN_VARIANT" | python3 -c "import sys; s=sys.stdin.read().strip(); print((s[:1].upper()+s[1:]) if s else s)")"
      provider_bundle_id="$(ios_resolve_variant_bundle_id "$APP_DIR" "${variant_pascal_for_plist}-Debug")"
      if [ -n "$provider_bundle_id" ]; then
        echo "ny-react-native: provider iOS variant ${variant_pascal_for_plist}-Debug → bundle id $provider_bundle_id"
      else
        echo "ny-react-native: WARN could not resolve PRODUCT_BUNDLE_IDENTIFIER for ${variant_pascal_for_plist}-Debug — Firebase may SIGABRT on launch (bundle-id mismatch)"
      fi
      write_dummy_google_services_plist "$APP_DIR/ios" "${provider_bundle_id:-}"
    else
      write_dummy_google_services_plist "$APP_DIR/ios"
    fi
    ensure_admob_app_ids "$APP_DIR"
    ensure_admob_debug_manifest "$APP_DIR"
    if [ -f "$APP_DIR/setup_config.sh" ]; then
      # NY_RN_VARIANT is the brand for which to generate provider config.
      # Provider expects camelCase (nammaYatri, jatriSaathi). Default to
      # nammaYatri if the user picked a consumer-style PascalCase brand
      # that the provider doesnt have.
      provider_app="$NY_RN_VARIANT"
      # PascalCase -> camelCase fallback for cross-app convenience
      provider_app="$(printf %s "$provider_app" | sed -E "s/^([A-Z])/\\L\\1/")"
      echo "ny-react-native: provider · setup_config.sh --app $provider_app"
      bash setup_config.sh --ci --app "$provider_app" --os "$NY_RN_PLATFORM" --mode master \
        || echo "ny-react-native: provider · setup_config.sh exited non-zero (continuing)"
    fi
    # Override env keys AFTER setup_config.sh (which regenerates .env).
    # Stamp every key the provider source reads in one pass:
    #   BASE_URL       — kept for symmetry with consumer (some shared
    #                    hooks read it).
    #   UI_BASE_URL    — what provider's getBaseUrlfromConfig() actually
    #                    reads (provider/src/api/utils.ts:97). Without
    #                    this, every API call has baseURL='' and iOS
    #                    rejects with `TypeError: Network request failed`.
    #   PLASMA_API_KEY — AppDelegate.initAppMonitor() calls
    #                    `PlasmaConfig.init(apiKey: RNCConfig.env(for:
    #                    "PLASMA_API_KEY") ?? "", baseURL: …)`. Plasma's
    #                    Swift init asserts (SIGTRAP) on empty values
    #                    → app crashes <2s after launch with an
    #                    EXC_BREAKPOINT and no NSException.
    #   PLASMA_URL     — same assertion. URL is a placeholder; Plasma
    #                    network calls 404 against the local driver-app
    #                    but that's acceptable noise for dev.
    python3 "$SETUP_PY_DIR/common/write_env_keys.py" "$APP_DIR" \
      'BASE_URL=http://localhost:8016/ui' \
      'UI_BASE_URL=http://localhost:8016/ui' \
      'PLASMA_API_KEY=dev-placeholder' \
      'PLASMA_URL=http://localhost:8016/plasma'
    # Point the Android gRPC client at the locally-run notification-
    # service (process-compose entry `notification-service`, gRPC :50051)
    # instead of the upstream prod default `grpc.moving.tech`. Patches
    # both the Firebase Remote Config defaults XML and the Kotlin
    # GRPCService.kt fallback constants. The launcher's adb-reverse
    # heartbeat in server.py forwards :50051 from the emulator's
    # localhost back to the host. iOS is a no-op (provider has no
    # native gRPC client in the iOS source).
    python3 "$SETUP_PY_DIR/common/grpc_local_url.py" "$APP_DIR"
  fi

  if [ "$NY_RN_PLATFORM" = "ios" ]; then
    # Sanity check on the nammayatri-ios submodule. Asymmetry between
    # consumer and provider:
    #   - consumer/ios/Podfile DOES reference
    #     nammayatri-ios/mobility-customer/MobilityCustomer.podspec, so
    #     that subdir MUST exist or pod install dies in CocoaPods with
    #     "No podspec found for MobilityCustomer".
    #   - provider/ios/Podfile pulls all pods from the
    #     `git@github.com:nammayatri/ny-cocoapods-specs.git` private
    #     spec repo (line 2 of the Podfile) — it does NOT reference
    #     any sibling subdir under nammayatri-ios/. The submodule only
    #     needs to be populated; no per-app subdir is required.
    #
    # The consumer-only check below mirrors the early Python check in
    # __main__.py — both layers gate iOS consumer builds on the
    # presence of mobility-customer/ since pod install would fail
    # otherwise. For provider we just ensure the submodule populated.
    if [ ! -d "$NY_RN_DIR/nammayatri-ios" ] || [ -z "$(ls -A "$NY_RN_DIR/nammayatri-ios" 2>/dev/null || true)" ]; then
      echo ""
      echo "ny-react-native: ERROR nammayatri-ios/ submodule did not populate under $NY_RN_DIR"
      echo "  iOS builds need this submodule (consumer for podspecs, provider for spec source repo)."
      echo "  Re-clone the parent repo over SSH so submodule auth carries through:"
      echo "    rm -rf $NY_RN_DIR && git clone --recurse-submodules git@github.com:nammayatri/ny-react-native $NY_RN_DIR"
      echo "  Or set NY_RN_PATH=/Users/khuzemakhomosi/Documents/ny-react-native if you have a working checkout."
      exit 5
    fi
    if [ "$app" = "consumer" ]; then
      sub_dir="$NY_RN_DIR/nammayatri-ios/mobility-customer"
      if [ ! -d "$sub_dir" ] || [ -z "$(ls -A "$sub_dir" 2>/dev/null || true)" ]; then
        echo ""
        echo "ny-react-native: ERROR nammayatri-ios/mobility-customer is missing or empty under $NY_RN_DIR"
        echo "  The consumer iOS Podfile expects podspecs under nammayatri-ios/mobility-customer/."
        echo "  Re-clone the parent repo over SSH so submodule auth carries through:"
        echo "    rm -rf $NY_RN_DIR && git clone --recurse-submodules git@github.com:nammayatri/ny-react-native $NY_RN_DIR"
        echo "  Or set NY_RN_PATH=/Users/khuzemakhomosi/Documents/ny-react-native if you have a working checkout."
        echo "  If the submodule populated but this dir is genuinely absent,"
        echo "  the branch ny-react-native is on may not ship mobility-customer yet."
        exit 5
      fi
    fi

    echo "ny-react-native: $app · pod install"
    # First run: --repo-update so any newly-added podspec sources (incl.
    # ones served from the nammayatri-ios submodule) resolve.
    # Subsequent runs: a plain `pod install` is enough; we always pass
    # --repo-update to be safe and avoid the "Unable to find a specification"
    # class of errors on a fresh checkout.
    # Per-pod progress is high-signal (200+ Pods take 2–3 min on a fresh
    # checkout); prefix with `pod:` so the dashboard Build tab can tell
    # pod output apart from xcodebuild output.
    ( cd "$APP_DIR/ios" \
      && ios_clean_env_run bundle install 2>&1 | sed "s/^/  bundle: /" \
      && ( ios_clean_env_run bundle exec pod install --repo-update 2>&1 | sed "s/^/  pod: /" \
           || ( echo "ny-react-native: pod install --repo-update failed; retrying plain pod install" \
                && ios_clean_env_run bundle exec pod install 2>&1 | sed "s/^/  pod: /" ) ) )
  fi
done

# ── Build, install, launch ──────────────────────────────────────────
METRO_PIDS=()
cleanup() {
  echo "ny-react-native: tearing down (Metros: ${METRO_PIDS[*]:-none})"
  # Dump the tail of every Metro log so the user can see WHY Metro
  # died if it was not torn down explicitly. Common causes: port
  # conflict, missing patch-package, react-native CLI keypress EOF.
  for app in "${APPS[@]:-}"; do
    log="/tmp/ny-rn-metro-$app.log"
    if [ -f "$log" ]; then
      echo "── tail of $log ──"
      tail -n 40 "$log" 2>/dev/null | sed "s/^/  metro($app): /" || true
    fi
  done
  for pid in "${METRO_PIDS[@]:-}"; do
    kill "$pid" 2>/dev/null || true
  done
}
trap cleanup EXIT INT TERM

# Find the next free TCP port starting from $1. Walks one port at a
# time. Falls through to the start value if no probing tool is
# available (defensive — both lsof and nc ship with the nix shell).
find_free_port() {
  local p=$1
  local hops=0
  while [ "$hops" -lt 50 ]; do
    if lsof -ti ":$p" >/dev/null 2>&1; then
      p=$(( p + 1 ))
    elif nc -z localhost "$p" >/dev/null 2>&1; then
      p=$(( p + 1 ))
    else
      echo "$p"
      return 0
    fi
    hops=$(( hops + 1 ))
  done
  echo "$1"
  return 0
}

# Kill any stale Metro instances for the given app subdir so each new
# launch lands on the canonical METRO_PORT_BASE instead of walking past
# busy ports. Without this, sequential launches accumulate (8088, 8089,
# 8090, …) and the simulator app may end up connected to a stale Metro
# whose bundle doesn't match the just-installed binary — observed
# symptoms include "Invalid hook call" (two React copies via two Metros)
# and "Module has not been registered as callable" (bundle from a Metro
# that's about to die). Match by the unique substring
# `data/ny-react-native/<app>/` which only appears in this app's CLI
# arg list, regardless of port.
kill_stale_metros_for_app() {
  local app="$1"
  local marker="data/ny-react-native/$app/"
  # `pgrep -f` matches against the full command line. Find every Metro
  # for this app, then walk up to its parent shell (npm exec wrapper)
  # so the whole tree dies — leaving the npm parent alive sometimes
  # respawns Metro a second later.
  local pids
  pids=$(pgrep -f "$marker.*react-native.*start" 2>/dev/null || true)
  if [ -n "$pids" ]; then
    local parents=""
    for pid in $pids; do
      local ppid
      ppid=$(ps -o ppid= -p "$pid" 2>/dev/null | tr -d ' ' || true)
      if [ -n "$ppid" ] && [ "$ppid" != "1" ]; then
        parents="$parents $ppid"
      fi
    done
    echo "ny-react-native: killing stale Metros for $app (pids: $pids$parents)"
    # shellcheck disable=SC2086
    kill $pids $parents 2>/dev/null || true
    # Anything still alive after 2s gets SIGKILL — Metro sometimes
    # ignores SIGTERM mid-bundle.
    sleep 2
    local still
    still=$(pgrep -f "$marker.*react-native.*start" 2>/dev/null || true)
    if [ -n "$still" ]; then
      # shellcheck disable=SC2086
      kill -KILL $still 2>/dev/null || true
      sleep 1
    fi
  fi
  # NOTE: do NOT also sweep the METRO_PORT_BASE..+5 range here — when
  # apps launch sequentially in the same run (provider then customer),
  # a port sweep on the second iteration would kill the just-launched
  # provider Metro that's correctly bound on 8088. The marker-based
  # pgrep above already handles every stale Metro for THIS app without
  # touching siblings.
}

METRO_PORT_BASE=8088   # Metro default. 8081 is location-tracking-service;
                       # 8082 is kept as buffer. Both customer (8088) and
                       # driver (8088→find_free_port→8089) walk up from
                       # this base — dodges every reservation in
                       # Backend/nix/services/ports.nix.
LAUNCHED=()
for app in "${APPS[@]}"; do
  APP_DIR="$NY_RN_DIR/$app"
  cd "$APP_DIR"

  # Reap any stale Metros for this app first so the new launch lands on
  # the canonical port. Without this, find_free_port walks past the
  # squatter and we end up with two Metros for one app — see "Invalid
  # hook call" and "Module not registered as callable" symptoms.
  kill_stale_metros_for_app "$app"

  # Pick a free port — old Metro instances from previous launch attempts
  # may still be holding 8088 (or 8089). Without this, the new Metro
  # exits with EADDRINUSE before the build even starts.
  METRO_PORT=$(find_free_port "$METRO_PORT_BASE")
  if [ "$METRO_PORT" != "$METRO_PORT_BASE" ]; then
    echo "ny-react-native: port $METRO_PORT_BASE busy — using $METRO_PORT instead"
  fi

  echo ""
  echo "ny-react-native: ── build/install/launch $app on $NY_RN_PLATFORM (metro :$METRO_PORT) ──"

  # Background Metro on its own port so two apps can co-run.
  # Launch Metro as a DIRECT child of this shell so the trailing `wait`
  # blocks on it.
  #
  # We invoke `npx react-native start` directly (instead of `yarn start`)
  # so there is no yarn wrapper between us and Metro — yarn 1.x kept
  # exiting under our Popen environment despite </dev/null + CI=true.
  # Direct npx + nohup + </dev/null + CI=true + no parent TTY together
  # eliminate every TTY/keypress dependency RN CLI tries to attach.
  cd "$APP_DIR"
  # --reset-cache so Metro re-transforms with the just-written .env
  # values instead of serving a cached bundle from a previous launch
  # (react-native-config inlines BASE_URL at transform time).
  CI=true nohup npx react-native start --port "$METRO_PORT" --reset-cache \
    </dev/null >"/tmp/ny-rn-metro-$app.log" 2>&1 &
  # Capture into a scalar before pushing — bash with `set -u` aborts
  # on `${arr[-1]}` if any inherited bashopts disallow neg-index, and
  # we already have the pid in $!.
  metro_pid=$!
  METRO_PIDS+=("$metro_pid")
  echo "ny-react-native: started Metro for $app (pid $metro_pid, port $METRO_PORT, log /tmp/ny-rn-metro-$app.log)"

  # Wait for Metro to actually be ready to serve the bundle.
  # Without this, the just-installed app boots, requests the bundle,
  # gets an error response (Metro still cold-starting), and shows a
  # red box with "Module has not been registered as callable.
  # Registered callable JavaScript modules (n = 0)" — no modules
  # because the bundle never executed cleanly.
  #
  # Metro responds with the literal text "packager-status:running" on
  # GET /status once it is ready. We poll up to 60s.
  echo "ny-react-native: waiting for Metro on :$METRO_PORT to be ready…"
  # Loop body is at script scope (inside the inner bash -c), not in a
  # function — so plain assignments instead of `local`.
  METRO_DEADLINE=$(( $(date +%s) + 60 ))
  while true; do
    metro_status_body=$(curl -s -m 2 "http://localhost:$METRO_PORT/status" 2>/dev/null || true)
    if echo "$metro_status_body" | grep -q "packager-status:running"; then
      echo "ny-react-native: Metro :$METRO_PORT ready"
      break
    fi
    if [ "$(date +%s)" -gt "$METRO_DEADLINE" ]; then
      echo "ny-react-native: WARN Metro :$METRO_PORT did not become ready in 60s — continuing anyway"
      echo "── tail of /tmp/ny-rn-metro-$app.log ──"
      tail -n 30 "/tmp/ny-rn-metro-$app.log" 2>/dev/null | sed "s/^/  metro: /" || true
      break
    fi
    sleep 2
  done

  # Build + install + launch (foreground so we capture build logs).
  # Variant maps to a productFlavor (Android) / scheme (iOS):
  #   consumer Android: <Brand>DevDebug   (e.g. LynxDevDebug)
  #   consumer iOS:     <Brand>-Debug     (e.g. Lynx-Debug)
  #   provider:         setup_config.sh already wrote the right .env /
  #                     theme files using --app <variant>; the default
  #                     `yarn android` / `yarn ios` then builds it.
  #
  # The Google Services Gradle plugin needs google-services.json to
  # contain a client whose package_name matches the variant ApplicationId.
  # We seed the JSON with a fat list of known IDs in
  # write_dummy_google_services_json — but flavors can resolve to IDs we
  # do not know about (e.g. Lynx -> in.mobility.international.debug).
  # build_with_install_recovery runs the build and recovers from two
  # well-known classes of install failure:
  #   1. "No matching client found for package name X" — append X to
  #      google-services.json and retry.
  #   2. INSTALL_FAILED_INSUFFICIENT_STORAGE / "not enough space" — the
  #      AVD has filled up after multiple flavor installs. Uninstall
  #      all known dev package names + their .debug variants, then retry.
  build_with_install_recovery() {
    local cmd_log="/tmp/ny-rn-build-$app.log"
    local attempt rc
    for attempt in 1 2 3; do
      rm -f "$cmd_log"
      # Defensive: temporarily disable `set -e` and run the pipeline
      # explicitly so we always reach PIPESTATUS — irrespective of
      # whether `pipefail` is honored in this shell context.
      set +e
      "$@" 2>&1 | tee "$cmd_log"
      rc=${PIPESTATUS[0]:-1}
      set -e
      if [ "$rc" -eq 0 ]; then
        return 0
      fi
      echo "ny-react-native: build attempt $attempt failed with rc=$rc; checking for recoverable errors"

      # All recovery checks below grep over the build log. grep returning
      # 1 (no match) is normal here, but with `set -e` it can propagate
      # through `$(...)` substitutions and abort the function before we
      # reach the storage-full branch. Disable errexit for the whole
      # recovery body — we already check return codes explicitly.
      set +e

      # Recovery 1: missing google-services client for variant applicationId.
      # The error line looks like:
      #   No matching client found for package name in.mobility.X.debug
      # (the quotes around the package name vary by gradle version; use
      # a quote-free strategy: grab the next non-whitespace token, then
      # strip everything that is not [A-Za-z0-9._-] — i.e. drop quotes).
      local missing_pkg
      missing_pkg="$(grep -oE "No matching client found for package name [^ ]+" "$cmd_log" 2>/dev/null \
        | head -n1 \
        | sed -E "s/.*name +//" \
        | tr -d -c "A-Za-z0-9._-")"
      if [ -n "$missing_pkg" ] && [ "$attempt" -lt 3 ]; then
        echo ""
        echo "ny-react-native: detected missing google-services client for package $missing_pkg"
        echo "ny-react-native: appending placeholder client and retrying"
        python3 - "$APP_DIR/android/app/google-services.json" "$missing_pkg" <<PY
import json, sys, hashlib
target, pkg = sys.argv[1], sys.argv[2]
with open(target) as f:
data = json.load(f)
existing = {c["client_info"]["android_client_info"]["package_name"] for c in data["client"]}
if pkg not in existing:
h = hashlib.sha1(pkg.encode()).hexdigest()[:16]
data["client"].append({
    "client_info": {
        "mobilesdk_app_id": f"1:000000000000:android:{h}",
        "android_client_info": {"package_name": pkg},
    },
    "oauth_client": [],
    "api_key": [{"current_key": "AIzaSyDUMMY-not-functional"}],
    "services": {"appinvite_service": {"other_platform_oauth_client": []}},
})
with open(target, "w") as f:
    json.dump(data, f, indent=2)
print(f"  added client for {pkg}")
PY
        set -e
        continue
      fi

      # Recovery 2: AVD out of space. Surgical uninstall is unreliable
      # (system_server caches, dexopt artifacts, accumulated logs all
      # eat /data faster than uninstall frees it). Just wipe the AVD
      # and cold-boot it, then retry the install.
      local storage_match
      storage_match="$(grep -oE "INSTALL_FAILED_INSUFFICIENT_STORAGE|not enough space|Requested internal only" "$cmd_log" | head -n1 || true)"
      echo "ny-react-native: storage-error pattern match in build log: ${storage_match:-NONE}"
      if [ -n "$storage_match" ] && [ "$attempt" -lt 3 ]; then
        echo ""
        echo "ny-react-native: emulator internal storage full — wiping AVD data and cold-booting"

        # Discover the AVD name. Prefer the running emulator
        # ro.kernel.qemu.avd_name property (works on modern API levels);
        # fall back to the first listed AVD on the system.
        local AVD_NAME
        AVD_NAME="$(adb -e shell getprop ro.kernel.qemu.avd_name 2>/dev/null | tr -d "\\r" || true)"
        if [ -z "$AVD_NAME" ]; then
          AVD_NAME="$(adb -e shell getprop ro.boot.qemu.avd_name 2>/dev/null | tr -d "\\r" || true)"
        fi
        if [ -z "$AVD_NAME" ]; then
          AVD_NAME="$(emulator -list-avds 2>/dev/null | head -n1 || true)"
        fi
        if [ -z "$AVD_NAME" ]; then
          echo "ny-react-native: ERROR could not determine AVD name to wipe"
          return 1
        fi
        echo "ny-react-native: wiping AVD: $AVD_NAME"

        # Tell the running emulator to shut down, then WAIT until it
        # actually disappears from `adb devices` — racing the next
        # `emulator -avd` against the dying instance produces an
        # "AVD already in use" error or a half-attached ghost device.
        echo "ny-react-native: shutting down current emulator…"
        adb -e emu kill >/dev/null 2>&1 || true
        local KILL_DEADLINE
        KILL_DEADLINE=$(( $(date +%s) + 30 ))
        while adb devices 2>/dev/null | grep -q "emulator-"; do
          if [ "$(date +%s)" -gt "$KILL_DEADLINE" ]; then
            echo "ny-react-native: WARN emulator did not disappear after 30s; force-killing qemu processes"
            pkill -9 -f "qemu-system" 2>/dev/null || true
            pkill -9 -f "emulator64" 2>/dev/null || true
            sleep 2
            break
          fi
          sleep 1
        done
        adb kill-server >/dev/null 2>&1 || true
        sleep 1
        adb start-server >/dev/null 2>&1 || true

        local EMU_LOG="/tmp/ny-rn-emulator.log"
        : > "$EMU_LOG"
        echo "ny-react-native: launching emulator $AVD_NAME with -wipe-data (logs: $EMU_LOG)"
        ( emulator -avd "$AVD_NAME" -wipe-data -no-snapshot-load >>"$EMU_LOG" 2>&1 & )
        disown 2>/dev/null || true

        # Wait for the new emulator to register with adb, with periodic
        # diagnostics so the user can see we are not hung.
        echo "ny-react-native: waiting for emulator to register with adb…"
        local DEV_DEADLINE
        DEV_DEADLINE=$(( $(date +%s) + 120 ))
        until adb devices 2>/dev/null | grep -q "emulator-"; do
          if [ "$(date +%s)" -gt "$DEV_DEADLINE" ]; then
            echo "ny-react-native: ERROR emulator did not appear in adb after 120s"
            echo "── tail of $EMU_LOG ──"
            tail -n 40 "$EMU_LOG" 2>/dev/null || true
            return 1
          fi
          sleep 3
        done
        adb wait-for-device

        # Cold boot from wipe takes longer than snapshot restore.
        echo "ny-react-native: waiting for sys.boot_completed (cold boot)…"
        local BOOT_DEADLINE BOOT_START
        BOOT_START=$(date +%s)
        BOOT_DEADLINE=$(( BOOT_START + 420 ))
        while [ "$(adb shell getprop sys.boot_completed 2>/dev/null | tr -d "\\r")" != "1" ]; do
          if [ "$(date +%s)" -gt "$BOOT_DEADLINE" ]; then
            echo "ny-react-native: ERROR cold-boot timeout (420s) after wipe"
            echo "── tail of $EMU_LOG ──"
            tail -n 40 "$EMU_LOG" 2>/dev/null || true
            return 1
          fi
          # Heartbeat every ~30s so the modal does not look frozen.
          local elapsed=$(( $(date +%s) - BOOT_START ))
          if [ $(( elapsed % 30 )) -lt 5 ] && [ "$elapsed" -gt 5 ]; then
            echo "ny-react-native: still waiting for boot ($elapsed/420s)…"
          fi
          sleep 5
        done
        echo "ny-react-native: sys.boot_completed=1 after $(( $(date +%s) - BOOT_START ))s"

        local SHELL_DEADLINE
        SHELL_DEADLINE=$(( $(date +%s) + 120 ))
        until adb shell pm list packages -s >/dev/null 2>&1; do
          if [ "$(date +%s)" -gt "$SHELL_DEADLINE" ]; then
            echo "ny-react-native: ERROR adb shell unresponsive after cold boot"
            return 1
          fi
          sleep 3
        done
        echo "ny-react-native: adb shell ready"
        # Re-wire adb reverse — wipe drops everything including these.
        apply_adb_reverses "post-wipe"
        echo "ny-react-native: AVD $AVD_NAME wiped + cold-booted, retrying install"
        set -e
        continue
      fi

      # Recovery 3: APK installed cleanly but RN CLI guessed the wrong
      # launcher activity (it uses defaultConfig.applicationId rather
      # than the active flavor s applicationId, which is wrong for
      # multi-flavor consumer builds like lynx -> in.mobility.international.debug).
      # Extract the real applicationId from the just-built APK via aapt
      # and launch with adb shell monkey.
      if grep -qE "Installed on 1 device|installRelease|installDebug" "$cmd_log" \
         && grep -qE "Activity class.*does not exist|am start.*Error" "$cmd_log"; then
        echo "ny-react-native: APK installed but RN CLI guessed wrong launcher activity"
        local apk_path aapt_bin app_pkg
        # Filter to APKs newer than $cmd_log (which is recreated at the
        # start of every build attempt). Without this filter we pick the
        # newest APK across ALL flavors — e.g. an earlier Lynx APK
        # whose mtime beats the just-built NammaYatri APK.
        apk_path=$(find "$APP_DIR/android/app/build/outputs/apk" -type f -name "*.apk" -newer "$cmd_log" 2>/dev/null \
          | head -n1)
        if [ -z "$apk_path" ]; then
          # Fallback: no APK newer than $cmd_log (should not happen on
          # a successful install, but be defensive). Take freshest one.
          apk_path=$(find "$APP_DIR/android/app/build/outputs/apk" -type f -name "*.apk" 2>/dev/null \
            | xargs ls -t 2>/dev/null | head -n1)
        fi
        aapt_bin=$(ls -1 "${ANDROID_HOME:-$HOME/Library/Android/sdk}"/build-tools/*/aapt 2>/dev/null \
          | sort -V | tail -n1)
        [ -z "$aapt_bin" ] && aapt_bin=$(command -v aapt 2>/dev/null || true)
        app_pkg=""
        if [ -n "$aapt_bin" ] && [ -n "$apk_path" ]; then
          # aapt prints: `package: name=<q>com.foo<q> versionCode=...`
          # The regex .// strips the leading quote/apostrophe; . .* strips
          # the trailing quote + space + rest. Avoids embedding literal
          # single quotes (which would break our outer bash -c wrapper).
          app_pkg=$("$aapt_bin" dump badging "$apk_path" 2>/dev/null \
            | head -n1 \
            | sed -E "s/^package: name=.//; s/. .*//")
          echo "ny-react-native: extracted applicationId from APK: ${app_pkg:-<none>}"
        else
          echo "ny-react-native: WARN aapt not found at \$ANDROID_HOME/build-tools/*/aapt"
        fi
        if [ -n "$app_pkg" ]; then
          # Confirm the package is actually on-device.
          if ! adb shell pm list packages 2>/dev/null | grep -q "package:$app_pkg$"; then
            echo "ny-react-native: WARN $app_pkg not in pm list packages — install may have failed silently"
          fi

          # Wipe app data BEFORE launch so MMKV / shared-prefs / accounts
          # do not stash a previously-cached BASE_URL (the consumer reads
          # the URL from MMKV on every boot, falling back to .env only
          # when MMKV is empty). Without this clear, the .env BASE_URL
          # we just rewrote is ignored on every re-launch.
          echo "ny-react-native: clearing $app_pkg data so BASE_URL from .env is picked up"
          adb shell pm clear "$app_pkg" 2>&1 | sed "s/^/  pm clear: /" || true

          # Try monkey first — it picks the LAUNCHER activity automatically.
          echo "ny-react-native: launching $app_pkg via adb shell monkey"
          local monkey_out
          monkey_out=$(adb shell monkey -p "$app_pkg" -c android.intent.category.LAUNCHER 1 2>&1 || true)
          echo "$monkey_out" | sed "s/^/  monkey: /"

          # Verify the activity is actually on top. If monkey reported
          # no launcher activity, or the foreground stayed on home screen,
          # fall back to resolving the explicit component name and using
          # `am start -n`.
          sleep 2
          local top
          top=$(adb shell dumpsys activity activities 2>/dev/null \
            | grep -E "topResumedActivity|mResumedActivity" \
            | head -n1 \
            | tr -d "\\r" || true)
          echo "ny-react-native: top activity after monkey: ${top:-<unknown>}"

          if ! echo "$top" | grep -q "$app_pkg"; then
            echo "ny-react-native: monkey did not bring $app_pkg to foreground — falling back to am start"
            local resolved
            resolved=$(adb shell cmd package resolve-activity --brief \
              -c android.intent.category.LAUNCHER -a android.intent.action.MAIN "$app_pkg" 2>/dev/null \
              | tail -n1 | tr -d "\\r[:space:]" || true)
            if [ -n "$resolved" ] && echo "$resolved" | grep -q "/"; then
              echo "ny-react-native: am start -n $resolved"
              adb shell am start -n "$resolved" 2>&1 | sed "s/^/  am start: /" || true
            else
              echo "ny-react-native: WARN could not resolve launcher activity for $app_pkg"
              echo "ny-react-native: try manually: adb shell monkey -p $app_pkg -c android.intent.category.LAUNCHER 1"
            fi
          fi

          # Give the app 4s to either survive or crash, then dump logcat
          # so the user can see WHY it died if it crashed on init. Common
          # causes: Firebase init failing with our placeholder
          # google-services.json, missing CONFIG_URL response from local
          # backend, or cleartext HTTP blocked.
          sleep 4
          echo "ny-react-native: post-launch crash check…"
          local app_pid
          app_pid=$(adb shell pidof "$app_pkg" 2>/dev/null | tr -d "\\r" || true)
          if [ -z "$app_pid" ]; then
            echo "ny-react-native: WARN $app_pkg is NOT running — it crashed on startup."
          else
            echo "ny-react-native: $app_pkg is running (pid $app_pid)"
          fi
          echo "ny-react-native: last AndroidRuntime / FATAL log lines (most recent 60):"
          adb logcat -d -t 800 AndroidRuntime:E "*:F" 2>/dev/null \
            | tail -n 60 \
            | sed "s/^/  logcat: /" \
            || true
          if [ -z "$app_pid" ]; then
            echo "ny-react-native: HINT crash on init is usually one of:"
            echo "  - Firebase failing to init with our dummy google-services.json"
            echo "    (mitigation: drop a real google-services.json in"
            echo "     consumer/android/app/google-services.json before re-launching)"
            echo "  - The app cannot reach BASE_URL — verify with:"
            echo "      adb shell curl -v http://localhost:8013/v2  (Android 14+)"
            echo "      or check that adb reverse is wired (above output)"
            echo "  - Cleartext HTTP blocked on this Android API level —"
            echo "    needs network_security_config.xml allowing localhost."
          fi
          set -e
          return 0
        else
          echo "ny-react-native: WARN could not derive applicationId; skipping manual launch"
          set -e
          return 0
        fi
      fi

      set -e
      return 1
    done
  }

  # Re-apply adb reverses + verify a device is actually connected
  # before kicking gradle. Without the device-presence check, gradle
  # exits with "No connected devices!" if adb dropped the emulator
  # between our preflight and now.
  if [ "$NY_RN_PLATFORM" = "android" ]; then
    # Make sure the metro port we just picked is forwarded too
    # (apply_adb_reverses only does the standard 8088/8089; if Metro
    # ended up on 8090+ via find_free_port spillover, add it).
    apply_adb_reverses "pre-launch"
    if [ "$METRO_PORT" -ne 8088 ] && [ "$METRO_PORT" -ne 8089 ]; then
      adb reverse "tcp:$METRO_PORT" "tcp:$METRO_PORT" >/dev/null 2>&1 || true
      echo "ny-react-native: also reversed tcp:$METRO_PORT for Metro"
    fi

    # Device-presence sanity check. If adb sees no device, attempt a
    # daemon restart (most common cause of transient device dropout).
    if ! adb devices 2>/dev/null | grep -qE "^emulator-[0-9]+\s+device"; then
      echo "ny-react-native: WARN no adb device visible — restarting adb daemon"
      adb kill-server >/dev/null 2>&1 || true
      adb start-server >/dev/null 2>&1 || true
      adb wait-for-device
      # Give system_server a beat to reattach
      for _ in 1 2 3 4 5; do
        if adb devices 2>/dev/null | grep -qE "^emulator-[0-9]+\s+device"; then
          break
        fi
        sleep 2
      done
    fi
    # Re-apply reverses after the potential daemon restart
    apply_adb_reverses "pre-launch-post-adb-reset"
    if [ "$METRO_PORT" -ne 8088 ] && [ "$METRO_PORT" -ne 8089 ]; then
      adb reverse "tcp:$METRO_PORT" "tcp:$METRO_PORT" >/dev/null 2>&1 || true
    fi
  else
    # iOS Simulator shares the host network stack natively — `localhost`
    # in the simulator IS the host loopback. No port forwarding needed.
    echo "ny-react-native: iOS Simulator uses host loopback natively (no reverse needed)"
  fi

  # Build the iOS run-ios flag list once. --udid pins the booted
  # Simulator (avoids RN CLI grabbing a connected real device).
  # --verbose is NOT passed by default (bloats the log to ~50k lines);
  # we add it only on the retry path after a quiet build has failed.
  ios_flags=(--port "$METRO_PORT")
  if [ -n "${IOS_SIM_UDID:-}" ]; then
    ios_flags+=(--udid "$IOS_SIM_UDID")
  fi

  # README troubleshooting (consumer/README.md): "if you face any errors
  # in IOS then clean the project and try reinstalling cocoapods again
  # cd ios && rm -rf build && rm -rf Pods && pod install". We codify
  # that as a one-shot recovery step, then re-run the build with
  # CI=true so RN CLI streams the raw xcodebuild log line-by-line
  # (default mode hides everything behind animated spinner dots).
  ios_clean_and_pod_reinstall() {
    local app_dir="$1"
    echo "ny-react-native: iOS recovery — rm -rf $app_dir/ios/build $app_dir/ios/Pods then pod install"
    rm -rf "$app_dir/ios/build" "$app_dir/ios/Pods" || true
    ( cd "$app_dir/ios" && ios_clean_env_run bundle install \
      && ( ios_clean_env_run bundle exec pod install --repo-update \
           || ios_clean_env_run bundle exec pod install ) ) \
      || echo "ny-react-native: WARN pod install during recovery exited non-zero (continuing)"
  }
  retry_ios_build() {
    local label="$1"; shift
    echo "ny-react-native: $label run-ios failed — retrying with --verbose for raw xcodebuild log"
    # Don't filter the verbose retry — when both quiet and clean retries
    # have already failed, the user wants the unfiltered failure cause
    # in the Build tab. Still tee to disk for grep-ability.
    local raw_log="/tmp/ny-rn-xcodebuild-${app:-unknown}-verbose.log"
    set -o pipefail
    CI=true ios_clean_env_run npx react-native run-ios --verbose "$@" 2>&1 \
      | tee "$raw_log" \
      | sed "s/^/  xcodebuild: /" || true
    set +o pipefail
  }

  if [ "$app" = "consumer" ]; then
    if [ "$NY_RN_PLATFORM" = "android" ]; then
      variant="${NY_RN_VARIANT}DevDebug"
      echo "ny-react-native: consumer · gradle variant $variant on metro :$METRO_PORT"
      # Pass --port so RN CLI tells the app which Metro port to talk to.
      build_with_install_recovery npx react-native run-android --mode "$variant" --port "$METRO_PORT"
    else
      # iOS schemes are PascalCase (Lynx-Debug, Bridge-Debug, BharatTaxi-Debug…).
      # The dashboard / manual invocations sometimes pass lowercase
      # (`lynx`); without normalisation xcodebuild aborts with
      # "workspace does not contain a scheme named lynx-Debug" — which
      # RN CLI then reports as a generic code-65 with no log. Uppercase
      # the first character to recover the canonical scheme name.
      variant_pascal="$(printf %s "$NY_RN_VARIANT" | python3 -c "import sys; s=sys.stdin.read().strip(); print((s[:1].upper()+s[1:]) if s else s)")"
      variant="${variant_pascal}-Debug"
      echo "ny-react-native: consumer · iOS scheme $variant on metro :$METRO_PORT (sim UDID: ${IOS_SIM_UDID:-auto})"
      if ! ios_run_with_logs npx react-native run-ios --scheme "$variant" "${ios_flags[@]}"; then
        ios_clean_and_pod_reinstall "$APP_DIR"
        if ! ios_run_with_logs npx react-native run-ios --scheme "$variant" "${ios_flags[@]}"; then
          retry_ios_build "consumer" --scheme "$variant" "${ios_flags[@]}"
          echo "ny-react-native: ERROR consumer iOS build failed (xcodebuild exit != 0). Inspect the xcodebuild: lines above for the real error."
          exit 7
        fi
      fi
    fi
  else
    # provider — setup_config.sh already configured the brand; just build.
    if [ "$NY_RN_PLATFORM" = "android" ]; then
      build_with_install_recovery npx react-native run-android --port "$METRO_PORT"
    else
      # Provider iOS schemes are PascalCase like consumer (Bridge-Debug,
      # KeralaSavaari-Debug, Lynx-Debug, NammaYatri-Debug, OdishaYatri-
      # Debug, YatriSathi-Debug — see the run-ios error message). Without
      # an explicit --scheme RN CLI looks for one literally named
      # "provider" and fails with "Could not load the shared scheme".
      provider_variant_pascal="$(printf %s "$NY_RN_VARIANT" | python3 -c "import sys; s=sys.stdin.read().strip(); print((s[:1].upper()+s[1:]) if s else s)")"
      provider_variant="${provider_variant_pascal}-Debug"
      echo "ny-react-native: provider · iOS scheme $provider_variant on metro :$METRO_PORT (sim UDID: ${IOS_SIM_UDID:-auto})"
      if ! ios_run_with_logs npx react-native run-ios --scheme "$provider_variant" "${ios_flags[@]}"; then
        ios_clean_and_pod_reinstall "$APP_DIR"
        if ! ios_run_with_logs npx react-native run-ios --scheme "$provider_variant" "${ios_flags[@]}"; then
          retry_ios_build "provider" --scheme "$provider_variant" "${ios_flags[@]}"
          echo "ny-react-native: ERROR provider iOS build failed (xcodebuild exit != 0). Inspect the xcodebuild: lines above for the real error."
          exit 7
        fi
      fi
    fi
  fi

  # Mirror Android's post-launch crash check on iOS too. RN CLI reports
  # "Successfully launched" the moment `simctl launch` returns a PID —
  # but the app commonly aborts <2 seconds later (Firebase bundle-id
  # mismatch, missing Info.plist key, dyld dylib not found, etc.). Without
  # this check the dashboard says "ready" while the simulator is back on
  # the home screen, leaving the user to wonder why nothing happened.
  if [ "$NY_RN_PLATFORM" = "ios" ]; then
    sleep 3
    echo "ny-react-native: iOS post-launch crash check…"
    bundle_to_check=""
    # Both consumer and provider use the `<Brand>-Debug` scheme convention,
    # and ios_resolve_variant_bundle_id works against any workspace under
    # $APP_DIR/ios/. The earlier code only resolved for consumer, then fell
    # through to a `simctl listapps | tail -n1` heuristic for provider — but
    # that heuristic picks the LAST installed user app on the simulator,
    # which is non-deterministic (e.g. yesterday's `in.mobility.nammayatri.debug`
    # would shadow today's `in.mobility.lynxdriver` and report a false crash).
    variant_pascal_check="$(printf %s "$NY_RN_VARIANT" | python3 -c "import sys; s=sys.stdin.read().strip(); print((s[:1].upper()+s[1:]) if s else s)")"
    bundle_to_check="$(ios_resolve_variant_bundle_id "$APP_DIR" "${variant_pascal_check}-Debug" || true)"
    if [ -z "$bundle_to_check" ] && [ -n "${IOS_SIM_UDID:-}" ]; then
      # Last-resort fallback when the resolver fails. The simctl listapps
      # heuristic is unreliable (see above); only use it as final resort
      # so we at least try SOMETHING. We prefer recently-installed apps
      # (mtime-sorted) over the lexicographic last entry the old code used.
      bundle_to_check="$(xcrun simctl listapps "${IOS_SIM_UDID}" 2>/dev/null \
        | grep -E 'CFBundleIdentifier' | tail -n1 | sed -E 's/.*= +"?([^";]+)"?;.*/\1/' || true)"
    fi
    if [ -n "$bundle_to_check" ] && [ -n "${IOS_SIM_UDID:-}" ]; then
      echo "ny-react-native: checking $bundle_to_check on $IOS_SIM_UDID"
      # Earlier we used `simctl spawn launchctl list | grep UIKitApplication:`
      # — but simulator apps don't always register under launchctl, so that
      # reported false negatives ("NOT running" while the app was perfectly
      # fine on screen). Use `simctl get_app_container` to get the .app's
      # absolute path on the host, then pgrep host processes whose command
      # line contains that path. The simulator launches each app as a host
      # process whose argv[0] is .../<DeviceUDID>/.../<UUID>/<Bundle>.app/<Exec>,
      # so this is a reliable single-call check.
      app_container=$(xcrun simctl get_app_container "$IOS_SIM_UDID" "$bundle_to_check" 2>/dev/null || true)
      app_pid=""
      if [ -n "$app_container" ]; then
        app_pid=$(pgrep -f "$app_container" 2>/dev/null | head -n1 || true)
      fi
      if [ -z "$app_pid" ]; then
        echo "ny-react-native: WARN $bundle_to_check is NOT running — it crashed on launch."
        echo "ny-react-native: most recent diagnostic report:"
        # Find newest .ips for *this* bundle/exec, written in the last
        # 120s. Older reports are from a previous run and are misleading.
        # Match on bundle id (most reliable) or exec name as a fallback.
        ips=$(ls -t "$HOME/Library/Logs/DiagnosticReports/"*.ips 2>/dev/null \
              | head -n5 \
              | while read -r f; do
                  # mtime within last 120s
                  if [ "$(find "$f" -mmin -2 -print 2>/dev/null)" = "" ]; then continue; fi
                  if grep -lF "\"bundleID\":\"$bundle_to_check\"" "$f" 2>/dev/null; then break; fi
                done)
        if [ -n "$ips" ]; then
          echo "  → $ips"
          # Top of the .ips: exception type + termination reason + first
          # 12 frames of the crashed thread. That is almost always
          # enough to identify the crash class (Firebase, dyld, NSE,
          # missing Info.plist key, etc.).
          python3 - "$ips" <<'PY' 2>/dev/null | sed "s/^/  ips: /" || true
import json, sys
p = sys.argv[1]
text = open(p).read().split("\n", 1)
hdr = json.loads(text[0])
body = json.loads(text[1])
print(f"bundle: {hdr.get('bundleID')}  pid: {body.get('pid')}")
print(f"launch: {body.get('procLaunch')} → exit: {body.get('captureTime')}")
exc = body.get('exception') or {}
print(f"exception: type={exc.get('type')} signal={exc.get('signal')} codes={exc.get('codes')}")
last_ex = body.get('lastExceptionBacktrace') or []
if last_ex:
    print("lastExceptionBacktrace top frames:")
    for fr in last_ex[:10]:
        print(f"  {fr.get('imageName','?')}  {fr.get('symbol') or fr.get('symbolLocation','')}")
crashed = next((t for t in body.get('threads', []) if t.get('triggered')), None)
if crashed:
    print("crashed thread top frames:")
    for fr in (crashed.get('frames') or [])[:10]:
        print(f"  #{fr.get('imageIndex','?')}  {fr.get('symbol') or fr.get('symbolLocation','')}")
PY
        else
          echo "  (no fresh .ips for $bundle_to_check in last 120s — the app may"
          echo "   have been killed by simctl/Spring Board rather than crashing,"
          echo "   or pgrep missed a slow-spawning process. Tail unified log below.)"
        fi
        echo "ny-react-native: tail of unified log for $bundle_to_check:"
        xcrun simctl spawn "$IOS_SIM_UDID" log show --last 60s --style compact \
          --predicate "process == \"${bundle_to_check##*.}\" OR senderImagePath CONTAINS \"${bundle_to_check##*.}\"" 2>/dev/null \
          | tail -n 30 \
          | sed "s/^/  log: /" || true
        echo "ny-react-native: HINT iOS launch-crash is usually one of:"
        echo "  - Firebase bundle-id mismatch — GoogleService-Info.plist BUNDLE_ID"
        echo "    must equal the variant's PRODUCT_BUNDLE_IDENTIFIER. The runner now"
        echo "    auto-resolves this; if you see it, the resolver missed the variant."
        echo "  - Missing Info.plist usage description (NSLocationWhenInUseUsageDescription,"
        echo "    NSCameraUsageDescription, NSContactsUsageDescription, …)."
        echo "  - dyld dylib not found — usually means a Pod was added on another"
        echo "    branch but not re-installed: rm -rf ios/Pods ios/build && pod install."
      else
        echo "ny-react-native: $bundle_to_check is running (pid $app_pid) ✓"
        echo "ny-react-native:   .app path: $app_container"
      fi
    else
      echo "ny-react-native: WARN could not resolve bundle id or sim UDID — skipping iOS crash check"
    fi
  fi

  LAUNCHED+=("$app")
  # Bump base for the next app so find_free_port starts above this run
  # (handles the "both" case: consumer→8088, provider→8089+).
  METRO_PORT_BASE=$(( METRO_PORT + 1 ))
done

echo ""
echo "ny-react-native: launched ${LAUNCHED[*]}"
echo "ny-react-native: Metros are running in the background. Closing the test-context-api modal will keep them alive; tearing down test-context-api kills them."

# Stay alive so Metros keep serving until the parent kills us. Try
# `wait` first (blocks on direct children); if that returns while any
# Metro PID is still alive, fall back to a poll loop. Belt-and-
# suspenders against bash job-control quirks where wait can return 0
# without a child actually exiting.
if [ ${#METRO_PIDS[@]} -gt 0 ]; then
  echo "ny-react-native: blocking on Metro PIDs: ${METRO_PIDS[*]}"
  wait "${METRO_PIDS[@]}" 2>/dev/null || true
  while true; do
    any_alive=0
    for pid in "${METRO_PIDS[@]}"; do
      if kill -0 "$pid" 2>/dev/null; then
        any_alive=1
        break
      fi
    done
    if [ "$any_alive" -eq 0 ]; then
      echo "ny-react-native: all Metros have exited"
      break
    fi
    sleep 5
  done
else
  wait
fi
