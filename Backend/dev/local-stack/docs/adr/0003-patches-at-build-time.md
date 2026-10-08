# 0003 — Patches at build time

**Status:** in force since August 2026. **Where it shows:**
`.github/workflows/algeria-backend-build.yml`,
`.github/scripts/algeria/apply-patches.py`,
`ghcr.io/nammayatri-algeria/ny-backend:latest`.

## Context

The running backend is upstream Namma Yatri at **`03a7531` (2023-03-02)** —
the last baseline that is self-contained, seeds a real merchant, and still
builds with `stack` ([README](../../README.md), *Why this exists*). This branch
is upstream's later tree (cabal + nix, ~10 800 commits on) and cannot produce
those binaries. Upstream also hard-codes India: `+91`, ten-digit numbers, and
more that only surfaced in our countries (a BECKN parser that could not read a
negative longitude — Nouakchott is at −15.9).

## Decision

Do not edit the Haskell in this tree. The CI workflow checks out upstream at
`03a7531`, applies our patches with `apply-patches.py`, builds, and publishes
an image. On 2026-10-08 the script holds **54 patches across 29 files**: both
countries' dial codes and lengths, the gps parser, the per-merchant search
lock, the dispatch filter's Redis key, the car on each offer, and the
passenger's choice of driver. The script is idempotent and **fails loudly** if
a site is missing — a skipped patch builds a binary that looks fine and still
rejects a number at runtime.

## Consequences

- The Haskell under `Backend/app` and `Backend/lib` here is **upstream's and
  not what runs**. To know what the server does, ask it (`/openapi`, `strings`
  on the binary) — [driver-api.md](../driver-api.md).
- A backend change is: add a patch, push to `algeria/build-backend`, wait for
  the image, deploy it, re-prove the ride flow. Batch changes into one build.
- The build depends on outside services that cannot be forked (a Docker Hub
  image, FP Complete's casa); the published image is the insurance. The
  dependencies themselves are mirrored in the company org
  (`.github/scripts/algeria/README.md`, *The forked dependencies*).
