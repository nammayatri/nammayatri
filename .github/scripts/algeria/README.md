# Building the backend for Algeria on GitHub Actions

`.github/workflows/algeria-backend-build.yml` compiles the Namma Yatri Haskell
backend on a free GitHub-hosted runner and hands back four Linux x86-64 ELF
binaries as a build artifact.

## Starting a build

Push anything to the branch **`algeria/build-backend`**. That branch is the
trigger and exists for no other reason:

```bash
git push --force origin algeria/osrm-routing:algeria/build-backend
```

There is a concurrency group, so pushing again supersedes the run in flight
rather than racing it for a runner.

**Why not the "Run workflow" button?** GitHub only registers a
`workflow_dispatch` trigger when the workflow file is on the repo's *default*
branch. Ours is not, and putting it on `main` would fire upstream's
`cabal-main-push.yaml` — which, unlike most of the upstream workflows, has no
`github.repository` guard, so every push to `main` on this fork would start a
full failing cabal build. If you ever point this fork's default branch at a
branch carrying this file, the button appears and the inputs become editable.

Nothing runs on a schedule and nothing runs on a normal push to a work branch.

---

## What comes out

The run has two jobs. `build` compiles and publishes the executables as an
artifact; `publish` turns them into a container image you can deploy with a
single pull:

```
ghcr.io/<owner>/ny-backend:<source-ref>-<run>
ghcr.io/<owner>/ny-backend:latest
```

The image is namespaced by whoever owns the repo, so moving this to the company
org publishes to the company org with no edit. Authentication is `GITHUB_TOKEN`
— there is no secret to configure.

It is built from `Backend/dev/local-stack/Dockerfile.rider`, the same file the
local stack uses, so the published image and a locally built one cannot drift.
That Dockerfile also installs a modern `librdkafka` over the published base
image, which ships 0.11 — too old for these binaries, which call
`rd_kafka_destroy_flags()`.

To deploy from it instead of building locally:

```bash
export NY_IMAGE=ghcr.io/nammayatri-algeria/ny-backend:latest
docker compose pull rider-app
docker compose up -d --no-build
```



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
`snapshots/` with `cp -a`, never mounted over the root. Package directories
under `snapshots/` are content-addressed, so ours sit alongside the image's and
nothing prepopulated is lost.

**The C libraries are probably already there.** That image exists to build
Stackage, so the step checks for `librdkafka`, `libpq`, `libssl`,
`libmysqlclient` and `libz` and does nothing if they are present. Reaching for
apt in this image is a trap in two ways, both of which cost us a run:

- `apt-get update` exits non-zero because the image carries third-party repos
  (Confluent, NVIDIA CUDA) whose signing keys have since expired. The Ubuntu
  archive itself is fine. Confluent is worth keeping — it is what supplies a
  `librdkafka` new enough for `hw-kafka-client` — so it is marked
  `[trusted=yes]` rather than removed.
- **Do not repoint bionic at `old-releases.ubuntu.com`.** 18.04 is past its
  normal EOL but is still on `archive.ubuntu.com` under ESM. Rewriting the
  sources turns a working archive into `does not have a Release file`. An
  earlier version of this workflow did exactly that "as a fallback" and broke
  the step it was meant to rescue.

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

**Never write `inputs.x != false` for a flag that defaults to true.** This one
cost a run and very nearly cost five hours. Because the build is triggered by a
push, there is no `inputs` context at all — every `inputs.x` is null. GitHub
casts operands to numbers when their types differ, and both `null` and `false`
become `0`, so `inputs.x != false` evaluates to **false** and the step silently
skips. Run 30993922925 skipped the `+213` patches, the gateway preparation and
the cache restore, and reported every one of them as a normal green "skipped".
Had the apt step not failed straight afterwards, it would have spent five hours
building unpatched binaries with no gateway and finished successfully.

Two spellings that do work:

```yaml
if: ${{ github.event_name != 'workflow_dispatch' || inputs.use_cache }}   # default true
SOURCE_REF: ${{ inputs.source_ref || '03a7531...' }}                      # default string
```

`||` is fine because null is falsy, so the fallback wins. It is only the
comparisons that lie. And mind the precedence: `always() && a || b` parses as
`(always() && a) || b`, so parenthesise when you mean otherwise.

Because reading a condition is evidently not enough, run
`validate-workflow.py` before pushing a change to the workflow. It *evaluates*
every `if:` as a push event, asserts that the steps which must run do, checks
that a failed build still banks its cache and uploads its logs, runs `bash -n`
over every `run:` block, and fails on both traps above by name:

```bash
python3 .github/scripts/algeria/validate-workflow.py
```

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

## The forked dependencies

The build pulls from ten outside repositories, all pinned to exact commits in
`Backend/stack.yaml`. Every one of them is now mirrored in the company org:

```
nammayatri-algeria/beckn-gateway        nammayatri-algeria/beam
nammayatri-algeria/shared-kernel        nammayatri-algeria/beam-mysql
nammayatri-algeria/euler-hs             nammayatri-algeria/mysql-haskell
nammayatri-algeria/hedis                nammayatri-algeria/bytestring-lexing
nammayatri-algeria/passetto             nammayatri-algeria/haskell-sequelize
```

Each fork was checked to contain the exact commit the build pins — a fork that
does not is no insurance at all. Nothing points at them yet: `stack.yaml` still
names the upstream repos, and it should, because the forks are a fallback rather
than a change of dependency. Switching over is a search-and-replace in
`stack.yaml` if any upstream ever disappears.

**Forking does not cover everything.** Two build-time dependencies cannot be
forked:

- `fpco/stack-build:lts-16.31`, a third-party image on Docker Hub
- `casa.fpcomplete.com`, a live service stack 2.3.3 contacts and treats failure
  as fatal — and FP Complete's infrastructure is already partly rotted (their
  Hackage mirror returns 403)

The published container image is what actually protects against those, because
a finished image no longer needs any of them.

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
