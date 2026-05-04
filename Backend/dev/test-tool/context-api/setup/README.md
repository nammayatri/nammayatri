# Test-tool launcher setup package

Python helpers + static templates that drive the dashboard "Launch Native
App" / "Launch Control Center" buttons. These split out of the original
monolithic `Backend/dev/test-tool/ny-react-native-setup.sh` in three stages:

## Layout

```
setup/
├── ny_react_native/
│   ├── __main__.py            # (Stage 2b) orchestrator entrypoint
│   ├── common/                # shared, side-effect-free helpers
│   │   # ── data writers (Stage 1) ──
│   │   ├── google_services_json.py    # Android Firebase placeholder
│   │   ├── google_services_plist.py   # iOS Firebase placeholder
│   │   ├── admob_app_ids.py           # patches app.json
│   │   ├── patch_base_url.py          # rewrites consumerBaseUrl in src-v2
│   │   ├── write_local_base_url.py    # rewrites BASE_URL across .env files
│   │   ├── local_properties.py        # consumer + provider local.properties
│   │   # ── support library (Stage 2a) ──
│   │   ├── log.py                     # info / warn / err / section
│   │   ├── sh.py                      # run / run_ok / capture
│   │   ├── paths.py                   # REPO_ROOT, ny_rn_dir, app_dir
│   │   ├── ssh.py                     # GIT_SSH_COMMAND + diagnostics
│   │   ├── git_repo.py                # clone-or-pull, sync_submodules
│   │   ├── adb.py                     # resolve_bin, serials, apply_reverses
│   │   ├── simulator.py               # iOS sim boot + UDID capture
│   │   ├── emulator.py                # Android emulator boot
│   │   └── metro.py                   # find_free_port, start, wait_ready
│   ├── consumer/{ios,android}.py      # (Stage 2b) per-app build/install/launch
│   ├── provider/{ios,android}.py      # (Stage 2b)
│   └── templates/                     # static data — no logic
│       ├── AndroidManifest.debug.xml          # AdMob + Firebase opt-outs
│       └── consumer-local.properties.tmpl     # __ANDROID_HOME__ placeholder
└── control_center/frontend/setup.py   # (Stage 2b) control-center launcher
```

## Migration stages

- **Stage 1 (done)**. Every embedded data blob and Python heredoc moved
  out of `ny-react-native-setup.sh` into separate files. Bash continues
  to orchestrate but shells out to helpers. ~300 line reduction. No
  behavior change.
- **Stage 2a (done)**. Support library — `log`, `sh`, `paths`, `ssh`,
  `git_repo`, `adb`, `simulator`, `emulator`, `metro`,
  `local_properties`. These are dormant until Stage 2b wires them up;
  they import cleanly and resolve paths correctly today (verified by
  smoke test). Bash unchanged.
- **Stage 2b (next)**. Build the orchestrator + per-app modules. Two
  remaining hard parts: Android `build_with_install_recovery` (~120
  lines: AdMob package mismatch retry, INSUFFICIENT_STORAGE wipe-and-
  retry, APK extract + monkey + am-start fallback when RN CLI guesses
  the wrong launcher activity), and iOS clean-and-pod-reinstall
  recovery. Both stay in bash for now via small extracted scripts
  Python subprocesses.
- **Stage 3**. Drop the bash entirely; nix-shell invocation moves into
  `server.py`. The Popen call switches from `bash setup.sh` to
  `python3 -m setup.ny_react_native`.

## Invoking individual helpers from bash

Each helper is `python3 -m`-importable and also runnable as a script:

```bash
# Drop dummy google-services.json into consumer/android/app/
python3 setup/ny_react_native/common/google_services_json.py \
        data/ny-react-native/consumer/android/app

# Drop GoogleService-Info.plist throughout consumer/ios/
python3 setup/ny_react_native/common/google_services_plist.py \
        data/ny-react-native/consumer/ios

# Patch consumerBaseUrl in every appDefaultConfig/*.ts
python3 setup/ny_react_native/common/patch_base_url.py \
        data/ny-react-native/consumer http://localhost:8013/v2
```

## Invariants

- Helpers MUST be idempotent and safe to re-run.
- Existing real config files (real Firebase plists, real
  google-services.json) are kept untouched — placeholders only fill in
  when the file is absent.
- No helper imports anything outside the Python stdlib so it can run
  inside or outside the nix shell.
