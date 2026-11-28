# Namma Yatri Frontend — Setup Guide (Windows / WSL2)

This guide documents a verified, working process to set up the frontend development
environment, build the PureScript/webpack frontend, and produce an installable Android
debug APK, on a Windows machine using WSL2.

It supersedes the existing `Frontend/README.md` instructions for Windows users, since
`nix develop .#frontend` currently does not work (see "Known issues" below).

## Prerequisites

- Windows 10/11 with WSL2 support
- Android Studio installed on Windows (used for its bundled Android SDK; not required
  to run the actual build)
- At least 12 GB of RAM available to allocate to WSL2

## 1. Install WSL2 + Ubuntu

In PowerShell (as Administrator):

```powershell
wsl --install
```

Reboot if prompted, then complete Ubuntu's first-launch setup.

## 2. Increase WSL2 memory allocation

The PureScript compiler (`purs`) is memory-intensive on this codebase (1189+ modules)
and will be OOM-killed on WSL2's default memory allocation (~50% of host RAM, often
under 8 GB).

Create/edit `C:\Users\<YourUsername>\.wslconfig`:

```ini
[wsl2]
memory=12GB
swap=8GB
processors=4
```

Adjust `memory` to your machine's specs (leave headroom for Windows). From PowerShell:

```powershell
wsl --shutdown
```

Reopen your Ubuntu terminal to apply.

## 3. Install Node.js and PureScript (do NOT rely on Nix — see Known Issues)

Install Node via apt (avoids WSL2 picking up Windows' `node`/`npm` from the PATH,
which happens by default and causes native-binary incompatibilities):

```bash
sudo apt update
sudo apt install -y nodejs npm perl
```

Verify:

```bash
which node   # should print /usr/bin/node, not a /mnt/c/... path
node -v
```

Set up a user-owned global npm prefix and install PureScript:

```bash
mkdir -p ~/.npm-global
npm config set prefix ~/.npm-global
echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
npm install -g purescript
purs --version   # 0.15.16 as of writing
```

## 4. Clone the repo and install frontend dependencies

```bash
git clone https://github.com/nammayatri-algeria/nammayatri.git
cd nammayatri
```

```bash
cd Frontend/ui-customer
npm i
```

## 5. Run the dev server (optional, for local iteration)

```bash
npm start
```

This runs `spago build --watch` + `webpack-dev-server` in parallel. First build compiles
all ~1189 PureScript modules and can take 10-20+ minutes. Dev server is reachable at
`http://localhost:8084` from the Windows browser (WSL2 forwards localhost automatically).

## 6. Production build (for Android)

```bash
cd Frontend/ui-customer
spago build --purs-args '--censor-warnings'
NODE_OPTIONS="--max-old-space-size=10240" npm run bundle:android
```

The default Node heap limit (~2 GB) is insufficient for Terser minification on this
bundle; 10 GB has been confirmed sufficient with 12 GB of WSL2 RAM available.

## 7. Bundle into the native Android project

Target merchant used here: `nammaYatri` (customer app).

```bash
cd ~/nammayatri/Frontend/android-native
mkdir -p app/src/user/nammaYatri/assets/juspay
cp ../ui-customer/dist/android/index_bundle.js app/src/user/nammaYatri/assets/juspay/
cd ~/nammayatri/Frontend
bash userJuspayAssets.sh nammaYatri
```

## 8. Android build prerequisites

### 8a. JDK inside WSL2

The Gradle build needs a JDK installed **inside WSL2** (not Android Studio's bundled
Windows JBR), since the project lives on the WSL2 filesystem:

```bash
sudo apt install -y openjdk-17-jdk
```

### 8b. Reference the existing Android SDK (from Android Studio on Windows)

Rather than installing a second SDK copy, reuse the one Android Studio already has.
Find your SDK path (Android Studio → Settings → Android SDK), then create
`Frontend/android-native/local.properties`:

sdk.dir=/mnt/c/Users/<YourWindowsUsername>/AppData/Local/Android/Sdk


### 8c. Required BuildConfig fields (see Known Issues)

Several `buildConfigField` entries in `app/build.gradle` have unquoted empty-string
defaults that produce invalid generated Java when unset. Add placeholder (or real,
if available) quoted values to `local.properties`:

MERCHANT_ID_USER="namma_yatri_user"
MERCHANT_ID_DRIVER="namma_yatri_driver"
CONFIG_URL_USER="https://api.sandbox.nammayatri.in"
CONFIG_URL_DRIVER="https://api.sandbox.nammayatri.in"
RS_ENC_KEY="placeholder_key"
RS_ALGO="AES"
RS_INSTANCE_TYPE="placeholder_instance"
RS_ALGO_PADDING="PKCS5Padding"


Note: with placeholder values, the resulting APK will install and launch without
crashing, but will remain stuck on its loading screen — these encryption/config
fields appear to be tied to a real Juspay backend. Real values are required for a
fully functional app (see Known Issues).

### 8d. Firebase configuration

The repo ships a placeholder `google-services-temp.json` (empty credentials, covers
all app package name variants) sufficient to satisfy the Gradle build, but will crash
the app at runtime (`FirebaseInitProvider: Please set your Project ID`).

For a non-crashing build, register your own free Firebase project at
https://console.firebase.google.com with Android package name matching your target
flavor (e.g. `in.juspay.nammayatri.debug` for nammaYatri customer dev), download the
real `google-services.json`, and place it at `Frontend/android-native/app/google-services.json`
(this file is gitignored — do not commit real credentials).

## 9. Build the APK

The project contains ~30 merchant/flavor combinations. `assembleDebug` attempts to
build all of them and will fail on flavors your placeholder config doesn't cover.
Build only the target flavor instead — for nammaYatri customer (Ny = Namma Yatri,
User = customer app):

```bash
cd ~/nammayatri/Frontend/android-native
./gradlew assembleNyUserDevDebug
```

Output APK: `app/build/outputs/apk/nyUserDev/debug/app-nyUser-dev-debug.apk` (~121 MB
in debug, unstripped).

## 10. Install on a physical device

```bash
cp app/build/outputs/apk/nyUserDev/debug/app-nyUser-dev-debug.apk "/mnt/c/Users/<YourWindowsUsername>/apk-transfer/"
```

From Windows PowerShell, with USB debugging enabled on the device:

```powershell
& "C:\Users\<YourWindowsUsername>\AppData\Local\Android\Sdk\platform-tools\adb.exe" install -r "C:\Users\<YourWindowsUsername>\apk-transfer\app-nyUser-dev-debug.apk"
```

---

## Known issues in the current codebase (as of this writing)

1. **`nix develop .#frontend` does not work.** `Frontend/default.nix` does not define
   any `devShells` output (verified via `nix eval .#devShells.x86_64-linux --apply
   builtins.attrNames`, which only lists `backend`). The README and `.envrc.frontend`
   both reference `.#frontend`, but it isn't wired up. This guide bypasses Nix
   entirely for the frontend as a result.

2. **A commit on `main` (`872a9d0a`) deletes the entire `Frontend/` directory**, with
   a commit message admitting the cleanup was incomplete (references to `Frontend/`
   remain in `flake.nix`, `.envrc.frontend`, CI workflows, etc.). This appears to be a
   stray test/experimental commit. Recommend reverting it upstream. This guide's
   steps assume the `Frontend/` directory has been restored (as done in this PR).

3. **`app/build.gradle` has unquoted empty-string defaults for several
   `buildConfigField` entries** (`MERCHANT_ID_USER`, `MERCHANT_ID_DRIVER`,
   `CONFIG_URL_USER`, `CONFIG_URL_DRIVER`, `RS_ENC_KEY`, `RS_ALGO`,
   `RS_INSTANCE_TYPE`, `RS_ALGO_PADDING`), which generate invalid Java
   (`= ;`) unless overridden via `local.properties`. Compare to `CUSTOMER_REG_TOKEN`,
   whose default is correctly quoted (`"\"NA\""`).

4. **README property names for `local.properties` are stale**: it references
   `USER_MERCHANT_ID`/`DRIVER_MERCHANT_ID`, but the actual fields read by
   `build.gradle` are `MERCHANT_ID_USER`/`MERCHANT_ID_DRIVER`.

5. **App remains stuck on its loading screen with placeholder `RS_ENC_KEY`/
   `RS_ALGO`/`RS_INSTANCE_TYPE` values.** No network call to `CONFIG_URL_USER` is
   observed in logs before the hang (verified via `adb logcat` and Chrome remote
   WebView inspection — the WebView never leaves `about:blank`/`never-attached`).
   These fields appear to be encryption/signing parameters used internally by the
   Juspay HyperServices SDK before any network activity begins. Getting a fully
   functional app likely requires either real sandbox credentials from the Namma
   Yatri/Juspay team, or self-hosting the `Backend/` service (which has its own,
   working Nix devShell) to generate real values.
