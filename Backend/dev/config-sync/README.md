# Config Sync

Three-phase, file-based config transfer between Nammayatri environments
(`prod`, `prod_international`, `master`, `env` → `local`/`master`).

The whole workflow is driven by `config_transfer.py`:

1. **export** — pull raw rows from the source DB, write one JSON file per table.
2. **patch**  — apply per-direction overrides (URL rewrites, re-encryption,
   `merge_json`, etc.). Optionally zip and upload the patched bundle to S3.
3. **import** — read the patched JSON, generate SQL, execute against the
   target DB. Optionally fetch the bundle from CloudFront first.

## Directory layout

```
config-sync/
├── config_transfer.py            # the only entrypoint
├── requirements.txt
├── assets/
│   ├── config.json               # registry of schemas/tables to sync (+ dim cols)
│   ├── environments.json         # DB connections per environment (gitignored)
│   ├── environments.json.example # template — copy to environments.json
│   ├── patches.json              # per-direction overrides (active)
│   ├── patches.json.example      # template — copy to patches.json
│   └── data/
│       ├── <env>/<schema>/<table>.json           # raw export
│       ├── <from>_to_<to>/<schema>/<table>.json  # patched output
│       └── tmp/                                  # staging, promoted on success
├── master_to_local_sql/                          # generated SQL (dry-run)
└── prod_international_to_local_sql/              # generated SQL (dry-run)
```

## One-time setup

```bash
cd Backend/dev/config-sync

# Python deps
python3 -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt

# Config files (these are gitignored — copy from the .example versions)
cp assets/environments.json.example assets/environments.json
cp assets/patches.json.example       assets/patches.json

# Fill in real DB hosts / passwords in assets/environments.json
```

### Config files

| File                       | Purpose                                                                                        |
| -------------------------- | ---------------------------------------------------------------------------------------------- |
| `assets/config.json`       | Registry of `{schema: {table: {dim: [...]}}}` controlling what gets synced and the upsert key. |
| `assets/environments.json` | Per-env DB connections (`default` + per-schema overrides). Supports `execPod` for `kubectl exec`. |
| `assets/patches.json`      | Direction-specific transforms (`<from>_to_<to>` keys: URL rewrites, re-encryption, JSON merges). |

### Optional environment variables

```
CONFIG_SYNC_S3_BUCKET=...        # default bucket for `patch --s3`
CONFIG_SYNC_S3_PREFIX=config-sync # default S3 key prefix
CONFIG_SYNC_CLOUDFRONT_URL=...   # public base URL for `import --fetch`
```

These can also be passed as CLI flags (`--s3-bucket`, `--s3-prefix`, `--fetch-url`).

## The three phases

### 1. Export

Reads from the source DB, writes `assets/data/<env>/<schema>/<table>.json`.

```bash
python config_transfer.py export --from master
python config_transfer.py export --from master --parallel 10
python config_transfer.py export --from prod   --schema atlas_app
python config_transfer.py export --from prod   --table fare_policy --table fare_product
```

For `prod` / `prod_international`, the export is routed through the configured
`execPod` (`kubectl exec <pod> -- psql ...`) when set in `environments.json`.

### 2. Patch

Applies the `<from>_to_<to>` block from `patches.json` to the raw export and
writes the result to `assets/data/<from>_to_<to>/`.

Local-only:
```bash
python config_transfer.py patch --from master              --to local
python config_transfer.py patch --from prod_international  --to local
```

With S3 upload (zips `<from>_to_<to>/` and pushes to
`s3://<bucket>/<prefix>/<from>_to_<to>.zip`):
```bash
python config_transfer.py patch --from master --to local --s3
python config_transfer.py patch --from master --to local --s3 \
    --s3-bucket my-bucket --s3-prefix config-sync
```

Inspect the patch set without running it:
```bash
python config_transfer.py show-patches --from master --to local
```

### 3. Import

Reads `assets/data/<from>_to_<to>/`, generates SQL, executes it against the
target DB inside a `BEGIN/COMMIT`.

Local data, dry run (writes SQL files into `<from>_to_<to>_sql/` for review):
```bash
python config_transfer.py import --from master --to local --dry-run
```

Local data, execute:
```bash
python config_transfer.py import --from master --to local
```

Fetch from CloudFront if the patched directory is missing locally:
```bash
python config_transfer.py import --from master --to local --fetch
python config_transfer.py import --from master --to local --fetch \
    --fetch-url https://dxxxx.cloudfront.net/config-sync
```

Other useful flags:
```bash
--schema atlas_app             # restrict to one schema
--local-dir /path/to/dir       # import from a custom directory
```

## Typical workflows

**Producer (has prod access, publishes a bundle):**
```bash
python config_transfer.py export --from prod_international --parallel 10
python config_transfer.py patch  --from prod_international --to local --s3
```

**Consumer (no prod access, just wants to seed a local DB):**
```bash
python config_transfer.py import --from prod_international --to local --fetch
```

**Master → local (most common dev flow):**
```bash
python config_transfer.py export --from master
python config_transfer.py patch  --from master --to local
python config_transfer.py import --from master --to local --dry-run   # review
python config_transfer.py import --from master --to local             # apply
```

## Utilities

```bash
python config_transfer.py list                        # list all configured tables
python config_transfer.py list --schema atlas_app     # filter by schema
python config_transfer.py list --env master           # show row counts in an env
```

## Allowed transfer directions

`prod → local`, `prod → master`, `prod_international → local`,
`prod_international → master`, `master → local`, `env → local`.
