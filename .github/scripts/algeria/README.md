# Building the backend for Algeria on GitHub Actions

`.github/workflows/algeria-backend-build.yml` compiles the Namma Yatri Haskell
backend on a free GitHub-hosted runner and hands back four Linux x86-64 ELF
binaries as a build artifact.

Run it from **Actions → "algeria: build backend" → Run workflow**. It never
runs on its own — `workflow_dispatch` only.

---

## What comes out

| Binary | Why we need it |
|---|---|
| `rider-app-exe` | patched to accept `+213` |
| `dynamic-offer-driver-app-exe` | patched to accept `+213` |
| `beckn-gateway-exe` | not in the published Docker image |
| `mock-registry-exe` | not in the published Docker image |
| `driver-offer-allocator-exe` | only if you tick `build_allocator` |

Plus `MANIFEST.txt` with `file` output, sha256 and sizes for each.

The two gaps this closes are the same gap. The rider side (BAP) and the driver
side (BPP) do not talk directly — the gateway sits between them. It isn't in
the image we downloaded, so a ride search comes back with a route and no price
and the driver never sees it. Compiling it is the same job as compiling the
`+213` patch. One successful run unblocks both.

---

## The five patches

All hardcode `+91`. Applied by `apply-patches.py`, which fails loudly rather
than skipping a site — a missed patch produces a binary that looks fine and
still rejects `+213` at runtime.

| File | Line | Change |
|---|---|---|
| rider `Domain/Action/UI/Registration.hs` | 81 | `P.mobileIndianCode` → `("+213" :: Regex)` |
| driver `Domain/Action/UI/Registration.hs` | 76 | `P.mobileIndianCode` → `("+213" :: P.Regex)` |
| driver `Domain/Action/Dashboard/Driver.hs` | 301 | `mobileIndianCode = "+91"` → `"+213"` |
| driver `Domain/Action/UI/Call.hs` | 61 | `findByMobileNumber "+91"` → `"+213"` |
| driver `Domain/Action/UI/DriverOnboarding/Image.hs` | 189 | `findByMobileNumber "+91"` → `"+213"` |

**The two `Registration.hs` sites are not the same edit.** `Regex` is a type
synonym exported by `Kernel.Types.Predicate`, and the two files import that
module differently:

```haskell
-- rider  Registration.hs:50
import Kernel.Types.Predicate              -- unqualified -> `Regex`
-- driver Registration.hs:45
import qualified Kernel.Types.Predicate as P   -- qualified  -> `P.Regex`
```

Writing bare `Regex` in the driver file is a compile error you would find out
about hours into the build. Neither site needs a new import.

We hardcode `+213` rather than switching to the permissive
`P.mobileCountryCode`, which accepts any country code: that would let anyone in
the world trigger an OTP SMS, which is how SMS-pumping fraud works. Widening it
later is a one-line change.

### Still `+91`, deliberately not touched

`rider-app` `Domain/Action/UI/Call.hs:131,134` hardcodes `+91` for exophone
lookup and reverse phone lookup in the Exotel call flow. It is not on the patch
list and not on the registration path, so it does not block anything today. It
will matter if and when inbound calls get wired up.

---

## Things that are not arbitrary

Changing any of these will cost you a day. They were paid for already.

**Base image `fpco/stack-build:lts-16.31`.** GHC 8.8.4, Ubuntu 18.04,
glibc 2.27 — the same glibc as the runtime image. A newer base produces
binaries that do not start on the target. We have already hit exactly that
class of failure (`libpthread.so.0: symbol __libc_vfork ... not defined`). The
codebase is pinned to LTS 16.31 and will not compile on a newer one anyway.

**`STACK_ROOT=/home/stackage/.stack`, with nothing mounted over it.** That
image ships a prepopulated stack root — 3.7 GB, including a 599 MB package
index and precompiled snapshot packages. Mounting an empty volume there throws
all of it away and forces stack to re-bootstrap from FP Complete servers that
have rotted since 2020 (their Hackage mirror now returns 403). This was the
single biggest time sink in the whole exercise. The cache is merged *into*
`snapshots/` with `rsync`, never mounted over the root.

**`casa.fpcomplete.com` must be reachable.** stack 2.3.3 reads the snapshot
definition from it and treats failure as fatal — verified by blocking it, which
killed the build with everything else already cached. It is Cloudflare-fronted
with both A and AAAA records, and GitHub runners have no IPv6 route, so it can
hang on the AAAA address until timeout instead of failing fast. The workflow
pins it to IPv4 in `/etc/hosts` and checks it up front rather than an hour in.

**`--allow-different-user` and `chown -R root:root`.** The bind mount carries
the runner's uid; stack runs as root inside the container and refuses to build
a tree it does not own.

**`--fast` (`-O0`).** Roughly halves build time and peak memory for the app
packages. This backend answers API calls in a demo; runtime speed is
irrelevant.

**Both apps in one `stack build`.** They share their entire dependency tree, so
the second costs almost nothing on top of the first.

**Not `jobs.<id>.container:`.** Running the steps *inside* the build image
would be tidier, but Ubuntu 18.04 has glibc 2.27 and the Node 20 actions
(`actions/cache`, `actions/upload-artifact`) need 2.28+. So the workflow drives
the container with `docker run` / `docker exec` from the host, and the actions
run on the host where they work.

---

## beckn-gateway: extra-dep → project package

`Backend/stack.yaml` lists beckn-gateway under `extra-deps:`. stack builds
dependency packages **library-only** — executables are built only for project
packages. As a plain extra-dep we would get `libbeckn-gateway` and no
`beckn-gateway-exe`, which is the one thing we need.

`prepare-stack-yaml.py` moves the two subdirs from `extra-deps:` to
`packages:`, pointing at a sibling clone at the same pinned commit
(`7094d2af`). Upstream clearly did this at some point too — the lines are still
sitting there commented out:

```yaml
extra-deps:
  # - ../beckn-gateway/app/gateway
  # - ../beckn-gateway/app/mock-registry
```

beckn-gateway's own `stack.yaml` pins shared-kernel at `296681fc` while
nammayatri pins `28bae0f3`; building it inside the nammayatri project means it
compiles against `28bae0f3`. That is the combination nammayatri's own
`stack.yaml` already declares, and it is the cheap one — one snapshot, one
dependency build for all four executables. Building beckn-gateway as its own
stack project would pull a second `mobility-core` and, because stack keys its
snapshot database on the whole extra-deps set, risk recompiling the entire
dependency tree a second time.

If the gateway ever fails to compile against `28bae0f3`, that separate-project
build is the fallback — expect it to be slow.

---

## Runtime, cost, cache

Free. Public fork, GitHub-hosted runner, no card. 4 vCPU / 16 GB, x86-64
Linux — native, no emulation, unlike a local Apple Silicon build.

Expect **2–5 hours** for a cold build of ~600 packages. GitHub caps a job at
6 h; the workflow stops at 350 minutes so the cache save and artifact upload
still run.

The cache is what makes a timeout survivable. It stores the compiled dependency
tree (`$STACK_ROOT/snapshots`) and every `.stack-work`, and is saved with
`if: always()` — so a run that runs out of time still leaves the next one less
to do. Just run the workflow again; stack resumes and never starts from zero.

Two caveats:

- A repo gets 10 GB of Actions cache, evicted LRU. The dependency tree is a
  large fraction of that. If cache saves start failing, either accept the
  slower cold builds or untick `use_cache`.
- Disk is tight (~30 GB needed). The workflow removes the preinstalled Android
  SDK, .NET, Swift and friends first, and prints `df -h` before and after.

---

## Which source gets built

Not this branch. The workflow fetches `nammayatri/nammayatri` at
`03a753113af1fdcddf3378d9dc2fc31170e385e4` — the 2023 baseline our deployment
actually runs, and the last point at which `Backend/stack.yaml` exists. This
branch is ~10 800 commits past that and has moved to cabal + nix, so it has no
`stack.yaml` to build with and its binaries would not match the running image.

Both the repo and the ref are workflow inputs, so pointing this at our own fork
later is one field, not an edit.

---

## Running the scripts locally

Both take the directory that contains `Backend/`:

```bash
git clone --depth 1 https://github.com/nammayatri/nammayatri.git src \
  && git -C src fetch --depth 1 origin 03a753113af1fdcddf3378d9dc2fc31170e385e4 \
  && git -C src checkout FETCH_HEAD

python3 .github/scripts/algeria/apply-patches.py       src
python3 .github/scripts/algeria/prepare-stack-yaml.py  src
```

Both are idempotent. There is also a self-contained Docker build script that
predates this workflow and encodes the same constraints, in
`ny-build-for-friend/` outside the repo.
