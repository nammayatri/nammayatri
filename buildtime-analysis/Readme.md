# Build Time Analysis

This directory contains tools for analyzing Nix build logs and visualizing build performance.

## Files

- `analyze_build_log.py` - Parses build logs and generates timing analysis
- `visualize_builds.py` - Creates charts and visualizations from build data
- `nix-push.yaml` - GitHub Actions workflow that runs analysis after builds

## Local Usage

```bash
# Analyze a build log
python3 analyze_build_log.py all-apps.log

# Generate visualizations
python3 visualize_builds.py
```

## GitHub Actions Integration

The `nix-push.yaml` workflow automatically runs build analysis after each CI build.

### Setup Required

#### 1. Workflow Permissions

Ensure your GitHub repository has the following permissions configured:

1. Go to **Settings** → **Actions** → **General**
2. Under **Workflow permissions**, select:
   - ✅ **Read and write permissions**
   - ✅ **Allow GitHub Actions to create and approve pull requests** (optional)

#### 2. Artifacts Access

Analysis results are uploaded as GitHub Artifacts. No additional configuration needed - artifacts are automatically available from the GitHub Actions workflow run page.

#### 3. Optional: GitHub Releases

To push analysis results to GitHub Releases (configured in workflow):

1. Go to **Settings** → **Secrets and variables** → **Actions**
2. Ensure `GITHUB_TOKEN` has permissions to create releases
3. The workflow will create/update a release named "Latest Build Analysis"

### Viewing Results

After each build:

1. Go to the **Actions** tab in your repository
2. Click on the workflow run
3. Scroll down to **Artifacts** section
4. Download `build-analysis-results` which contains:
   - `build_analysis.json` - Raw build data
   - `build_analysis_report.md` - Summary report
   - `*.png` - Visualization charts
   - `*/parallelization_analysis.png` - Per-app analysis

### Configuration

The analysis runs automatically with these defaults:
- Retention: 30 days for artifacts
- Python version: 3.11
- Dependencies: matplotlib, pandas, numpy

To customize, edit the `analyze-build-times` job in `nix-push.yaml`.

### Build Time Improvement

  Your Actual Build Timeline

  04:08:38                                      04:33:47    04:39:43
     |                                             |           |
     |  rider-app (18.1m)          |               |           |
     |████████████████████████████ |               |           |
     |  04:08:38 → 04:26:47       |               |           |
     |                             |               |           |
     |  dynamic-offer-driver-app (25.1m)           |           |
     |█████████████████████████████████████████████|           |
     |  04:08:38 → 04:33:47                       |           |
     |                                             |           |
     |          (everything below BLOCKED until 04:33:47)      |
     |                                             |           |
     |                                  allocator  (53s)       |
     |                                  producer   (41s)       |
     |                                  kafka-cons (47s)       |
     |                                  unified-db (1.4m)      |
     |                                  lib-dashboard (3.0m)───|──┐
     |                                             |           |  |
     |                                             | provider-db (1.8m)
     |                                             | rider-db    (1.5m)
     |                                             | safety-db   (1.8m)
     |                                             |           |
     |                                             |    hunit-tests (34s)

  Key findings

  ┌─────────────────────────────────────┬────────────────────────────────────────────────────┐
  │               Metric                │                       Value                        │
  ├─────────────────────────────────────┼────────────────────────────────────────────────────┤
  │ Wall clock total                    │ 31.1 min                                           │
  ├─────────────────────────────────────┼────────────────────────────────────────────────────┤
  │ dynamic-offer-driver-app            │ 25.1 min (81% of total!)                           │
  ├─────────────────────────────────────┼────────────────────────────────────────────────────┤
  │ rider-app                           │ 18.1 min (runs in parallel, finishes earlier)      │
  ├─────────────────────────────────────┼────────────────────────────────────────────────────┤
  │ Post-driver-app chain               │ 5.9 min (allocator+lib-dashboard+dashboards+hunit) │
  ├─────────────────────────────────────┼────────────────────────────────────────────────────┤
  │ Idle cores after rider-app finishes │ 7 min of 24 cores doing nothing                    │
  ├─────────────────────────────────────┼────────────────────────────────────────────────────┤
  │ Current parallelization factor      │ 4.2x (of theoretical 24x)                          │
  └─────────────────────────────────────┴────────────────────────────────────────────────────┘

  The problem is clear

  dynamic-offer-driver-app at 25.1 minutes is the critical path. During the first 25 minutes, only 2-3 Nix derivations are building (rider-app, dynamic-offer-driver-app,
  and pre-commit-run). After rider-app finishes at 18.1m, most of your 24 cores sit idle for 7 minutes waiting for dynamic-offer-driver-app.

  After it finishes, 5 dependents burst in parallel but take only ~3 min for the longest chain (lib-dashboard → dashboards → hunit-tests).

  Recommended configuration for your machine

  Since only 2-3 heavy packages build concurrently, you want maximum cores per package:

  **cabal.project**
  jobs: 4

  **package.yaml ghc-options (for dynamic-offer-driver-app and rider-app)**
  - -j12

  **Rationale:**
  - Phase 1 (0-18m): dynamic-offer-driver-app + rider-app building in parallel = 2 × 12 = 24 cores, fully saturated
  - Phase 2 (18-25m): only dynamic-offer-driver-app left as heavy build — gets 12 cores. Rider-app dependents (scheduler, search-result-aggregator) are tiny (<1 min), the
  extra jobs slots let those trickle through on remaining cores
  - Phase 3 (25-31m): 5+ small dependents burst in parallel — jobs: 4 lets 4 build at once with -j12 each but they're small so they won't actually use all 12

  If you want to be more aggressive for Phase 2 (avoid idle cores when only driver-app is left):

  jobs: 3
  - -j12

  This is slightly less parallel for the burst phase but 3 × 12 = 36 means Nix won't over-subscribe too badly on your 24 cores (GHC's -j is a hint, not a hard limit — it
  won't spawn 12 threads if there aren't 12 compilable modules at that moment).

  TL;DR

  **cabal.project**
  jobs: 3

  **package.yaml ghc-options**
  - -j12

  Expected impact: dynamic-offer-driver-app goes from 25.1m (with -j4) to ~12-15m (with -j12), since 3x more module-level parallelism. Total wall clock drops from 31m →
  ~18-20m.

  The bottleneck is dynamic-offer-driver-app compilation speed, not inter-package parallelism. Throwing more cores at that single package is where your 24 cores pay off.