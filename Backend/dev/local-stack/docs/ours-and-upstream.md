# What is ours, and what is upstream's

This repository is a fork of [Namma Yatri](https://github.com/nammayatri/nammayatri).
Of its ~15 400 files, about 200 are ours. This page says which, so nobody has to
work it out again — this report had to, before the restructuring could start.
Measured 2026-10-08 on `algeria/osrm-routing`.

## Ours

| Path | What |
|---|---|
| `Backend/dev/local-stack/` | **The whole deployment**: what the server runs (`stack/`), how it is released (`ops/`), its tests, its docs. |
| `.github/workflows/algeria-*.yml` | Our three workflows: node tests, ride regression, backend build ([testing.md](testing.md)) |
| `.github/scripts/algeria/` | The backend build's scripts — `apply-patches.py` holds every change we make to the Haskell ([adr/0003](adr/0003-patches-at-build-time.md)) |
| `CLAUDE.md`, from *This fork: Movin* down | The fork's rules for an assistant; the part above it is upstream's |
| `docs/Movin-backend-architecture.pdf` | The architecture report |
| `.github/workflows/hlint.yaml` | One edit: upstream's HLint skips our `algeria/*` branches |

Not in this repository at all: the passenger and driver app (React Native,
`ny-algeria-passenger`) and the website and console (`Movin_DZ_Website`).

## Upstream's — and what that means

| Path | Status |
|---|---|
| `Backend/app/`, `Backend/lib/` | Upstream's Haskell, **not what runs**. The server runs upstream `03a7531` (2023) plus 54 patches applied in CI; this tree is ~10 800 commits newer and diverges on whole subsystems. Ask the server (`/openapi`, `strings` on the binary), not the tree ([driver-api.md](driver-api.md)). |
| `Backend/dhall-configs/`, `Backend/dev/` (outside `local-stack/`) | Upstream's. The deployment's configuration is in `local-stack/stack/` and in its database. |
| `Frontend/` | Upstream's PureScript app. **We do not build or ship it.** Identical to upstream since phase 2 (`117ad97b03` reverted four early edits). |
| `.github/workflows/` (other than ours) | Upstream's CI. It does not run usefully here. |
| Everything else | Upstream's, untouched. |

## How to check it again

Our commits are the non-merge commits by the owner's account; everything they
touch outside the paths above was reverted in phase 2.

    git log --no-merges --author=MohaGNPro --name-only --format= \
      | sort -u | grep -v '^Backend/dev/local-stack/\|^\.github/scripts/algeria/\|^\.github/workflows/algeria-'

The answer on 2026-10-08: `CLAUDE.md`, `docs/Movin-backend-architecture.pdf`,
`.github/workflows/hlint.yaml`, and `Frontend/` files whose changes were
reverted.

## Why it is still one repository

Moving `local-stack/` to a repository of its own was the plan's first open
decision ("move it, after launch"). Until then the rule that keeps the two
apart is the layout: the server is released from `stack/` and nothing else, and
no Haskell is edited in this tree.
