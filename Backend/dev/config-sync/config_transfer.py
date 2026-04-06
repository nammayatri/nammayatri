#!/usr/bin/env python3
"""
NammayYatri Config Transfer

Two-step workflow:
  1. export — read from source DB table-by-table, validate dims, write each
     table as a separate JSON file to tmp, then promote to assets/data/<env>/
  2. import — read table files one-by-one from assets/data/<env>/, apply patches,
     validate against target, generate SQL, execute

Usage:
  python config_transfer.py export --from master
  python config_transfer.py import --from master --to local --dry-run
  python config_transfer.py import --from master --to local

Storage:
  DEV=true  -> local files under assets/data/<env>/<schema>/<table>.json
  Otherwise -> S3 at <prefix>/<env>/<schema>/<table>.json
  Tmp during export: assets/data/tmp/<schema>/<table>.json (promoted on success)
"""

import argparse
import json
import os
import re
import shutil
import sys
import time as _time
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import date, datetime, time, timezone
from decimal import Decimal
from pathlib import Path
from uuid import UUID

import csv as _csv_module
import psycopg2
import psycopg2.extras
from dotenv import load_dotenv

# Config tables can have huge JSON blobs (e.g., rider_config, system_configs)
_csv_module.field_size_limit(100 * 1024 * 1024)  # 100MB

load_dotenv()

SCRIPT_DIR = Path(__file__).resolve().parent
ASSETS_DIR = SCRIPT_DIR / "assets"
DATA_DIR = ASSETS_DIR / "data"
TMP_DIR = DATA_DIR / "tmp"

_RE_ENCRYPT = re.compile(r'0\.1\.0\|[0-9]+\|[A-Za-z0-9+/=]{16,}')

VALID_ENVS = {"prod", "master", "env", "local"}
ALLOWED_TRANSFERS = {
    ("prod", "local"), ("prod", "master"),
    ("master", "local"), ("env", "local"),
}

IS_DEV = os.getenv("DEV", "").lower() in ("true", "1", "yes")


# ── Loaders ──────────────────────────────────────────────────────────────────


def load_json_file(path, label=""):
    if not path.exists():
        example = path.with_suffix(".json.example")
        sys.exit(f"Missing {path}\n  cp {example} {path}")
    with open(path) as f:
        return json.load(f)


def load_config_tables():
    data = load_json_file(ASSETS_DIR / "config.json")
    return {k: v for k, v in data.items() if not k.startswith("_")}


def table_names_for_schema(schema_config):
    return list(schema_config.keys())


def get_dim_columns(schema_config, table_name):
    return schema_config.get(table_name, {}).get("dim", [])


def load_environments():
    return load_json_file(ASSETS_DIR / "environments.json")


def load_patches():
    path = ASSETS_DIR / "patches.json"
    return json.load(open(path)) if path.exists() else {}


# ── DB helpers ───────────────────────────────────────────────────────────────


def get_db_config(env_config, environment, schema):
    env = env_config.get(environment)
    if not env:
        sys.exit(f"Environment '{environment}' not in environments.json")
    base = dict(env.get("default", {}))
    base.update(env.get("schemas", {}).get(schema, {}))
    if "db_schema" not in base:
        base["db_schema"] = schema
    return base


def get_connection(db_config):
    return psycopg2.connect(
        host=db_config["host"], port=db_config.get("port", 5432),
        database=db_config.get("database", "postgres"),
        user=db_config["user"], password=db_config.get("password", ""),
    )


def table_exists(cursor, schema, table):
    cursor.execute(
        "SELECT EXISTS (SELECT 1 FROM information_schema.tables "
        "WHERE table_schema = %s AND table_name = %s)", (schema, table))
    row = cursor.fetchone()
    return list(row.values())[0] if isinstance(row, dict) else row[0]


# ── Serialization ────────────────────────────────────────────────────────────


def json_serializer(obj):
    if isinstance(obj, (datetime, date, time)):
        return obj.isoformat()
    if isinstance(obj, Decimal):
        return str(obj)
    if isinstance(obj, UUID):
        return str(obj)
    if isinstance(obj, (bytes, memoryview)):
        b = obj.tobytes() if isinstance(obj, memoryview) else obj
        return "\\x" + b.hex()
    raise TypeError(f"Type {type(obj).__name__} not serializable")


def escape_sql_value(val):
    if val is None:
        return "NULL"
    if isinstance(val, bool):
        return "TRUE" if val else "FALSE"
    if isinstance(val, (int, float, Decimal)):
        return str(val)
    if isinstance(val, str):
        if val.startswith("\\x"):
            return f"'{val}'"
        return "'" + val.replace("'", "''") + "'"
    if isinstance(val, (dict, list)):
        return "'" + json.dumps(val).replace("'", "''") + "'::jsonb"
    return "'" + str(val).replace("'", "''") + "'"


# ── File I/O (table-level, batched for large tables) ─────────────────────────

BATCH_SIZE = 10000  # rows per batch file


def table_dir_path(base_dir, env_name, schema):
    d = base_dir / env_name / schema
    d.mkdir(parents=True, exist_ok=True)
    return d


def export_table(base_dir, env_name, schema, table, meta, conn, db_schema, columns):
    """Export table using COPY TO CSV — streams raw data from Postgres.

    Writes CSV to a temp file on disk, then parses in batches of BATCH_SIZE
    to write JSON. Never holds more than one batch in memory.

    Small tables (<=BATCH_SIZE) -> single table.json
    Large tables -> table/_meta.json + table/batch_NNNN.json

    Returns (path_or_dir, total_rows).
    """
    import csv

    d = table_dir_path(base_dir, env_name, schema)

    # Stream COPY to temp CSV on disk
    csv_tmp = d / f"_{table}.csv"
    copy_cur = conn.cursor()
    with open(csv_tmp, "w", newline="") as f:
        copy_cur.copy_expert(
            f'COPY "{db_schema}"."{table}" TO STDOUT WITH CSV HEADER', f
        )
    copy_cur.close()

    # Parse CSV and write batches to disk — never hold more than one batch in memory.
    # First pass: write batch files to a temp dir.
    table_d = d / table
    table_d.mkdir(parents=True, exist_ok=True)

    batch = []
    batch_num = 0
    total = 0

    with open(csv_tmp, "r", newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            batch.append(dict(row))
            total += 1
            if len(batch) >= BATCH_SIZE:
                (table_d / f"batch_{batch_num:04d}.json").write_text(
                    json.dumps(batch, default=json_serializer))
                batch_num += 1
                batch = []
    if batch:
        (table_d / f"batch_{batch_num:04d}.json").write_text(
            json.dumps(batch, default=json_serializer))
        batch_num += 1

    csv_tmp.unlink()

    if total == 0:
        # Empty table — clean up dir, write single empty file
        shutil.rmtree(table_d)
        data = {**meta, "columns": columns, "rows": []}
        path = d / f"{table}.json"
        path.write_text(json.dumps(data, default=json_serializer, indent=2))
        return path, 0

    if batch_num == 1:
        # Single batch — convert dir to single file
        only_batch = json.loads((table_d / "batch_0000.json").read_text())
        shutil.rmtree(table_d)
        data = {**meta, "columns": columns, "rows": only_batch}
        path = d / f"{table}.json"
        path.write_text(json.dumps(data, default=json_serializer, indent=2))
        return path, total

    # Multiple batches — write meta
    meta_data = {**meta, "columns": columns, "total_rows": total, "batched": True}
    (table_d / "_meta.json").write_text(json.dumps(meta_data, default=json_serializer, indent=2))
    return table_d, total


def iter_table_rows(base_dir, env_name, schema, table):
    """Yield (meta, rows_batch) for a table. Handles both single JSON and batched.
    Each yield is at most BATCH_SIZE rows.
    """
    d = table_dir_path(base_dir, env_name, schema)

    # Check if batched (directory) or single file
    table_d = d / table
    single_f = d / f"{table}.json"

    if table_d.exists() and (table_d / "_meta.json").exists():
        # Batched
        meta = json.loads((table_d / "_meta.json").read_text())
        for batch_file in sorted(table_d.glob("batch_*.json")):
            rows = json.loads(batch_file.read_text())
            yield meta, rows
    elif single_f.exists():
        data = json.loads(single_f.read_text())
        yield data, data.get("rows", [])
    else:
        return


def read_table_meta(base_dir, env_name, schema, table):
    """Read just the metadata (columns, etc.) without loading all rows."""
    d = table_dir_path(base_dir, env_name, schema)
    table_d = d / table
    single_f = d / f"{table}.json"

    if table_d.exists() and (table_d / "_meta.json").exists():
        return json.loads((table_d / "_meta.json").read_text())
    elif single_f.exists():
        data = json.loads(single_f.read_text())
        # Don't return rows, just meta
        return {k: v for k, v in data.items() if k != "rows"}
    return None


def list_table_files(base_dir, env_name, schema):
    d = base_dir / env_name / schema
    if not d.exists():
        return []
    names = set()
    for f in d.iterdir():
        if f.is_file() and f.suffix == ".json":
            names.add(f.stem)
        elif f.is_dir() and (f / "_meta.json").exists():
            names.add(f.name)
    return sorted(names)


# ── S3 ───────────────────────────────────────────────────────────────────────


def s3_client():
    import boto3
    return boto3.client("s3", region_name=os.getenv("AWS_REGION", "ap-south-1"))


def push_table_s3(data, bucket, prefix, env_name, schema, table):
    key = f"{prefix}/{env_name}/{schema}/{table}.json"
    payload = json.dumps(data, default=json_serializer, indent=2)
    s3_client().put_object(Bucket=bucket, Key=key, Body=payload.encode(), ContentType="application/json")
    return key


def pull_table_s3(bucket, prefix, env_name, schema, table):
    key = f"{prefix}/{env_name}/{schema}/{table}.json"
    resp = s3_client().get_object(Bucket=bucket, Key=key)
    return json.loads(resp["Body"].read().decode("utf-8"))


# ── Patches ──────────────────────────────────────────────────────────────────


def apply_patches_to_table(table_name, table_data, schema, patch_config):
    if not patch_config:
        return table_data, 0

    global_reps = patch_config.get("global_replacements", [])
    schema_reps = patch_config.get("schema_replacements", {}).get(schema, [])
    table_ovr = patch_config.get("table_overrides", {}).get(schema, {}).get(table_name, [])
    dim_ovr = patch_config.get("dimension_overrides", {}).get(schema, {}).get(table_name, [])
    all_reps = [r for r in global_reps + schema_reps if "find" in r]
    count = 0

    reencrypt = patch_config.get("reencrypt_foreign_values", True)

    for row in table_data["rows"]:
        for col in list(row.keys()):
            if not isinstance(row[col], str):
                continue
            orig = row[col]
            # Global string replacements (URLs etc)
            for r in all_reps:
                row[col] = row[col].replace(r["find"], r["replace"])
            # Re-encrypt foreign Passetto values with local keys
            if reencrypt and _RE_ENCRYPT.search(row[col]):
                row[col] = _reencrypt_foreign_values(row[col])
            if row[col] != orig:
                count += 1

        for ovr in table_ovr:
            if ovr["field"] in row:
                row[ovr["field"]] = ovr["value"]
                count += 1

        for rule in dim_ovr:
            # Empty where = match all rows
            where = rule.get("where", {})
            if not all(str(row.get(k)) == str(v) for k, v in where.items()):
                continue
            # "set" — full field replacement, supports eval:{field} templates
            # "set" — full field replacement
            for f, v in rule.get("set", {}).items():
                if f in row:
                    row[f] = _resolve_value(v, row)
                    count += 1
            # "merge_json" — merge fields into existing JSON value
            for f, fields in rule.get("merge_json", {}).items():
                if f in row and row[f]:
                    try:
                        existing = json.loads(row[f]) if isinstance(row[f], str) else row[f]
                        if isinstance(existing, dict) and isinstance(fields, dict):
                            _deep_merge_encrypt(existing, fields)
                            row[f] = json.dumps(existing)
                            count += 1
                    except (json.JSONDecodeError, TypeError):
                        pass

    return table_data, count


def _deep_merge_encrypt(target, patch):
    """Recursively merge *patch* into *target* dict, encrypting ENCRYPT: values."""
    for k, v in patch.items():
        if isinstance(v, str) and v.startswith("ENCRYPT:"):
            target[k] = _encrypt_via_obj(v[8:])
        elif isinstance(v, dict):
            if k not in target or not isinstance(target[k], dict):
                target[k] = {}
            _deep_merge_encrypt(target[k], v)
        else:
            target[k] = v


def _resolve_value(val, row):
    """Resolve a patch value. Supports:
    - Plain string: used as-is
    - "eval:prefix-{field_name}-suffix": interpolates row field values
    - "ENCRYPT:plaintext": encrypts via passetto /encrypt/obj
    """
    if not isinstance(val, str):
        return val
    if val.startswith("eval:"):
        template = val[5:]
        # Replace {field_name} with row values
        def _sub(m):
            field = m.group(1)
            return str(row.get(field, ""))
        return re.sub(r'\{(\w+)\}', _sub, template)
    if val.startswith("ENCRYPT:"):
        return _encrypt_via_obj(val[8:])
    return val


_reencrypt_cache = {}

def _reencrypt_foreign_values(text):
    """Replace all 0.1.0|N|... encrypted tokens with locally-encrypted dummy values.

    Master and local Passetto use different keys, so master-encrypted values
    can't be decrypted locally. For local testing, re-encrypt a dummy placeholder
    so the app can decrypt without errors. Mock services don't check real credentials.
    """
    def _replace(m):
        orig = m.group(0)
        if orig in _reencrypt_cache:
            return _reencrypt_cache[orig]
        replacement = _encrypt_via_obj("test-dummy-key")
        _reencrypt_cache[orig] = replacement
        return replacement
    return _RE_ENCRYPT.sub(_replace, text)


_encrypt_cache = {}

def _encrypt_via_obj(plaintext):
    """Encrypt using passetto /encrypt with S"value" format.

    Passetto expects S-prefix for string values: S"actual-value"
    This matches how the seed geninis tool and the app encrypt data.
    """
    if plaintext in _encrypt_cache:
        return _encrypt_cache[plaintext]
    import requests
    try:
        s_val = f'S"{plaintext}"'
        resp = requests.post("http://localhost:8079/encrypt", json={"value": s_val}, timeout=5)
        val = resp.json()["value"]
        _encrypt_cache[plaintext] = val
        return val
    except Exception:
        return f"UNENCRYPTED:{plaintext}"


# ── SQL generation (per table) ───────────────────────────────────────────────


# ── Target DB helpers (per table, no bulk load) ──────────────────────────────


def fetch_target_table_columns(cursor, db_schema, table):
    """Returns ([column_names], {col: data_type}, set_of_not_null_cols)."""
    cursor.execute("""SELECT column_name, data_type, is_nullable FROM information_schema.columns
        WHERE table_schema = %s AND table_name = %s ORDER BY ordinal_position""",
        (db_schema, table))
    rows = cursor.fetchall()
    if rows and isinstance(rows[0], dict):
        cols = [r["column_name"] for r in rows]
        types = {r["column_name"]: r["data_type"] for r in rows}
        not_null = {r["column_name"] for r in rows if r["is_nullable"] == "NO"}
    else:
        cols = [r[0] for r in rows]
        types = {r[0]: r[1] for r in rows}
        not_null = {r[0] for r in rows if r[2] == "NO"}
    return cols, types, not_null


def fetch_target_table_head(cursor, db_schema, table):
    if not table_exists(cursor, db_schema, table):
        return {}
    cursor.execute(f'SELECT * FROM "{db_schema}"."{table}" LIMIT 1')
    row = cursor.fetchone()
    return dict(row) if row else {}


def check_target_coverage_from_dims(cursor, db_schema, table, source_dims, dim_cols):
    """Returns list of warning strings for dim values in target not in source.
    Takes pre-collected source_dims set instead of full rows."""
    if not dim_cols:
        return []
    if not table_exists(cursor, db_schema, table):
        return []

    source_dims_str = {tuple(str(v) for v in dv) for dv in source_dims}
    dim_select = ", ".join(f'"{c}"' for c in dim_cols)
    cursor.execute(f'SELECT {dim_select}, COUNT(*) FROM "{db_schema}"."{table}" GROUP BY {dim_select}')

    warnings = []
    for group_row in cursor.fetchall():
        dim_val = tuple(str(v) for v in group_row[:-1])
        if dim_val not in source_dims_str:
            dim_display = ", ".join(f"{c}={v}" for c, v in zip(dim_cols, dim_val))
            warnings.append(f"({dim_display}): {group_row[-1]} rows")
    return warnings


def generate_delete_sql(schema, table, dim_cols, dim_values, total_rows):
    """Generate DELETE statements for a table."""
    lines = []
    if not dim_cols:
        lines.append(f"-- {schema}.{table}: {total_rows} rows (DELETE all + INSERT)")
        lines.append(f'DELETE FROM "{schema}"."{table}";')
    else:
        dim_str = ", ".join(dim_cols)
        lines.append(f"-- {schema}.{table}: {total_rows} rows (DELETE by {dim_str}: {len(dim_values)} values)")
        for dv in sorted(dim_values, key=str):
            where = " AND ".join(f'"{c}" = {escape_sql_value(v)}' for c, v in zip(dim_cols, dv))
            lines.append(f'DELETE FROM "{schema}"."{table}" WHERE {where};')
    return "\n".join(lines) + "\n"


def generate_insert_sql(schema, table, rows, src_columns, target_cols, head_row, not_null_cols=None, col_types=None):
    """Generate INSERT statements for a batch of rows.

    CSV COPY exports NULL as empty string. Convert empty strings back to NULL,
    except for NOT NULL text columns where empty string is kept as-is.
    For non-text NOT NULL columns (integer, etc.), empty strings become DEFAULT.
    """
    if not rows:
        return ""

    not_null_cols = not_null_cols or set()
    col_types = col_types or {}
    _text_types = {"text", "character", "character varying", "json", "jsonb"}
    tgt_cols = target_cols if target_cols is not None else src_columns
    insert_cols = [c for c in tgt_cols if c in src_columns or c in head_row]
    col_list = ", ".join(f'"{c}"' for c in insert_cols)

    lines = []
    for row in rows:
        vals = []
        for c in insert_cols:
            if c in row:
                val = row[c]
                if val == "":
                    if c not in not_null_cols:
                        vals.append("NULL")
                    elif col_types.get(c) not in _text_types:
                        # NOT NULL non-text column (e.g., integer): empty string is invalid, use DEFAULT
                        vals.append("DEFAULT")
                    else:
                        vals.append(escape_sql_value(val))
                else:
                    vals.append(escape_sql_value(val))
            elif c in head_row:
                vals.append(escape_sql_value(head_row[c]))
            else:
                vals.append("NULL")
        lines.append(f'INSERT INTO "{schema}"."{table}" ({col_list}) VALUES ({", ".join(vals)});')
    return "\n".join(lines) + "\n"


# ── Commands ─────────────────────────────────────────────────────────────────


def cmd_export(args):
    """Export: read source DB table-by-table -> tmp -> promote to data/<env>/."""
    from_env = args.source_env
    print(f"Exporting from {from_env} {'(DEV)' if IS_DEV else '(S3)'}\n")

    env_config = load_environments()
    config_tables = load_config_tables()

    if args.schemas:
        config_tables = {s: config_tables[s] for s in args.schemas if s in config_tables}

    table_filter = set(args.tables) if hasattr(args, 'tables') and args.tables else None

    # Clean tmp (only for filtered tables if specified)
    for schema in config_tables:
        tmp_schema_dir = TMP_DIR / from_env / schema
        if table_filter:
            for t in table_filter:
                tf = tmp_schema_dir / f"{t}.json"
                td = tmp_schema_dir / t
                if tf.exists(): tf.unlink()
                if td.exists(): shutil.rmtree(td)
        elif tmp_schema_dir.exists():
            shutil.rmtree(tmp_schema_dir)

    max_workers = args.parallel if hasattr(args, 'parallel') and args.parallel else 4
    results = {}

    for schema, schema_config in sorted(config_tables.items()):
        tables = table_names_for_schema(schema_config)
        if table_filter:
            tables = [t for t in tables if t in table_filter]
            if not tables:
                continue
        print(f"{'=' * 60}")
        print(f"{schema} ({len(tables)} tables, {max_workers} parallel)")
        print(f"{'=' * 60}")

        source_db = get_db_config(env_config, from_env, schema)
        db_schema = source_db.get("db_schema", schema)

        # Pre-check: get columns and validate dims using one connection
        try:
            check_conn = get_connection(source_db)
            check_cur = check_conn.cursor()
        except Exception as e:
            print(f"  CONNECTION FAILED: {e}", file=sys.stderr)
            results[schema] = {"status": "failed", "error": str(e)}
            continue

        export_tasks = []  # (table, meta, columns)
        dim_warnings = []

        for table in tables:
            if not table_exists(check_cur, db_schema, table):
                print(f"    [skip] {db_schema}.{table} — does not exist")
                continue

            check_cur.execute(f'SELECT * FROM "{db_schema}"."{table}" LIMIT 0')
            columns = [desc[0] for desc in check_cur.description]

            dim_cols = get_dim_columns(schema_config, table)
            missing = [c for c in dim_cols if c not in columns]
            if missing:
                print(f"    [DIM WARN] {table}: dim columns {missing} not in data — skipping table")
                dim_warnings.append(table)
                continue

            meta = {
                "schema": schema, "db_schema": db_schema, "table": table,
                "source_env": from_env,
                "exported_at": datetime.now(timezone.utc).isoformat(),
            }
            export_tasks.append((table, meta, columns))

        check_cur.close()
        check_conn.close()

        # Export tables in parallel — each gets its own DB connection
        table_count = 0
        row_count = 0

        def _export_one(task):
            tbl, meta, cols = task
            t0 = _time.time()
            conn = get_connection(source_db)
            try:
                result_path, total = export_table(
                    TMP_DIR, from_env, schema, tbl, meta, conn, db_schema, cols
                )
            finally:
                conn.close()
            elapsed = _time.time() - t0
            label = result_path.name if result_path.is_file() else f"{tbl}/ ({total // BATCH_SIZE + 1} batches)"
            return tbl, total, label, elapsed

        export_errors = []
        with ThreadPoolExecutor(max_workers=max_workers) as pool:
            futures = {pool.submit(_export_one, t): t for t in export_tasks}
            for future in as_completed(futures):
                try:
                    tbl, total, label, elapsed = future.result()
                    print(f"    [ok] {db_schema}.{tbl}: {total} rows -> {label} ({elapsed:.1f}s)")
                    table_count += 1
                    row_count += total
                except Exception as e:
                    tbl = futures[future][0]
                    print(f"    [FAIL] {db_schema}.{tbl}: {e}", file=sys.stderr)
                    export_errors.append(tbl)

        # Promote tmp -> data (or push to S3)
        tmp_schema_dir = TMP_DIR / from_env / schema
        if IS_DEV:
            dest = DATA_DIR / from_env / schema
            if table_filter and dest.exists():
                # Merge: only replace exported tables, keep others
                for item in tmp_schema_dir.iterdir():
                    target = dest / item.name
                    if target.exists():
                        if target.is_dir():
                            shutil.rmtree(target)
                        else:
                            target.unlink()
                    shutil.move(str(item), str(target))
                shutil.rmtree(tmp_schema_dir)
            else:
                if dest.exists():
                    shutil.rmtree(dest)
                dest.parent.mkdir(parents=True, exist_ok=True)
                shutil.move(str(tmp_schema_dir), str(dest))
            print(f"  Promoted to {dest}")
        else:
            if not args.s3_bucket:
                sys.exit("--s3-bucket required when DEV is not set")
            # Push all files (single JSON + batched dirs)
            for item in tmp_schema_dir.rglob("*.json"):
                rel = item.relative_to(tmp_schema_dir)
                key = f"{args.s3_prefix}/{from_env}/{schema}/{rel}"
                s3_client().put_object(
                    Bucket=args.s3_bucket, Key=key,
                    Body=item.read_bytes(), ContentType="application/json",
                )
            print(f"    Pushed {schema} to S3")
            shutil.rmtree(tmp_schema_dir)

        warns = dim_warnings + export_errors
        if warns:
            results[schema] = {"status": "ok", "tables": table_count, "rows": row_count,
                               "warnings": f"skipped: {', '.join(warns)}"}
        else:
            results[schema] = {"status": "ok", "tables": table_count, "rows": row_count}
        print()

    # Clean empty tmp dirs
    if TMP_DIR.exists():
        shutil.rmtree(TMP_DIR, ignore_errors=True)

    print(f"\n{'=' * 60}")
    print(f"EXPORT SUMMARY ({from_env})")
    print(f"{'=' * 60}")
    for s, r in sorted(results.items()):
        if r["status"] == "ok":
            warn = f"  ({r['warnings']})" if r.get('warnings') else ""
            print(f"  {s}: {r['tables']} tables, {r['rows']} rows{warn}")
        else:
            print(f"  {s}: FAILED — {r.get('error', r['status'])}")

    if any(r["status"] != "ok" for r in results.values()):
        sys.exit(1)

def cmd_patch(args):
    """Patch: read raw export, apply all overrides, write patched dump.

    Reads from: assets/data/<from_env>/
    Writes to:  assets/data/<from>_to_<to>/
    Applies: global replacements, re-encryption via passetto /encrypt/obj, merge_json
    Starts passetto automatically if not running (uses seed DB on port 5422).
    """
    from_env = args.source_env
    to_env = args.target_env
    direction = f"{from_env}_to_{to_env}"

    print(f"Patching: {from_env} -> {to_env}\n")

    config_tables = load_config_tables()
    patch_config = load_patches().get(direction, {})

    if not patch_config:
        print(f"WARNING: No patches for '{direction}'\n")

    schemas = args.schemas or list(config_tables.keys())
    src_base = DATA_DIR / from_env
    dst_base = DATA_DIR / direction

    if dst_base.exists():
        shutil.rmtree(dst_base)

    for schema in schemas:
        if schema not in config_tables:
            continue
        available = list_table_files(src_base, "", schema)
        if not available:
            print(f"[skip] No data for {schema}")
            continue

        print(f"{'=' * 60}")
        print(f"{schema} ({len(available)} tables)")
        print(f"{'=' * 60}")

        # If there are any global or schema-level find/replace rules, every table
        # needs patching — the 10-row sample can miss rows that contain the target string.
        has_global_reps = bool([r for r in patch_config.get("global_replacements", [])
                                if "find" in r]
                               + [r for r in patch_config.get("schema_replacements", {}).get(schema, [])
                                  if "find" in r])

        for table in available:
            # First pass: check if this table needs patching at all
            needs_patch = has_global_reps
            total_rows = 0
            for meta, batch_rows in iter_table_rows(src_base, "", schema, table):
                total_rows += len(batch_rows)
                if not needs_patch:
                    test_data = {"columns": meta.get("columns", []), "rows": batch_rows[:10]}
                    _, pc = apply_patches_to_table(table, test_data, schema, patch_config)
                    if pc > 0:
                        needs_patch = True

            if total_rows == 0:
                print(f"    [skip] {table}: 0 rows")
                continue

            if not needs_patch:
                # No patches needed — copy raw export as-is
                src_table_dir = src_base / schema / table
                src_table_file = src_base / schema / f"{table}.json"
                dst_schema_dir = table_dir_path(dst_base, "", schema)

                if src_table_dir.exists():
                    shutil.copytree(str(src_table_dir), str(dst_schema_dir / table))
                elif src_table_file.exists():
                    shutil.copy2(str(src_table_file), str(dst_schema_dir / f"{table}.json"))

                print(f"    [copy] {table}: {total_rows} rows (no patches)")
                continue

            # Needs patching — process batch by batch
            patch_count = 0
            batch_idx = 0

            for meta, batch_rows in iter_table_rows(src_base, "", schema, table):
                batch_data = {"columns": meta.get("columns", []), "rows": batch_rows}
                batch_data, pc = apply_patches_to_table(table, batch_data, schema, patch_config)
                patch_count += pc

                out_meta = {k: v for k, v in meta.items() if k != "rows"}
                out_meta["patched"] = True
                out_meta["direction"] = direction

                out_dir = table_dir_path(dst_base, "", schema) / table
                out_dir.mkdir(parents=True, exist_ok=True)
                (out_dir / f"batch_{batch_idx:04d}.json").write_text(
                    json.dumps(batch_data["rows"], default=json_serializer))
                batch_idx += 1

            # Write meta
            out_dir = table_dir_path(dst_base, "", schema) / table
            meta_data = {**out_meta, "total_rows": total_rows, "batched": True}
            (out_dir / "_meta.json").write_text(json.dumps(meta_data, default=json_serializer, indent=2))

            # If single batch, collapse to single file
            if batch_idx == 1:
                batch_file = out_dir / "batch_0000.json"
                rows = json.loads(batch_file.read_text())
                shutil.rmtree(out_dir)
                single = {**meta_data, "rows": rows}
                del single["batched"]
                del single["total_rows"]
                (table_dir_path(dst_base, "", schema) / f"{table}.json").write_text(
                    json.dumps(single, default=json_serializer, indent=2))

            print(f"    [ok] {table}: {total_rows} rows, {patch_count} patches")

        print()

    # ── Post-patch audit: scan output for remaining sensitive patterns ──
    audit_warnings = _audit_patched_output(dst_base, src_base, config_tables, schemas)
    if audit_warnings:
        print(f"\n{'=' * 60}")
        print(f"PATCH AUDIT WARNINGS ({len(audit_warnings)} issues)")
        print(f"{'=' * 60}")
        for w in audit_warnings:
            print(f"  {w}")
        print()

    print(f"Patched data written to {dst_base}/")
    print(f"Run: DEV=true python config_transfer.py import --from {from_env} --to {to_env}")


# ── Post-patch audit ───────────────────────────────────────────────────


# Safe URL domains: CDN/content/app-links — not service endpoints
_AUDIT_SAFE_DOMAINS = {
    # CDN / static assets
    "firebasestorage.googleapis.com", "fonts.googleapis.com",
    "cdn.britannica.com", "sandbox.assets.moving.tech",
    "assets.moving.tech", "assets.juspay.in", "ny.assets.juspay.in",
    "raw.githubusercontent.com",
    # Content / docs
    "docs.google.com",
    "youtube.com", "www.youtube.com", "youtu.be",
    # App store / deep links
    "play.google.com", "apps.apple.com", "dl.flipkart.com",
    "manayatri.page.link", "yatrisathi.page.link",
    # Consumer-facing websites
    "nammayatri.in", "www.nammayatri.in",
    "manayatri.in", "yatrisathi.in", "odishayatri.in",
    "www.getyatri.com", "app-nammayatri.redbus.in",
    "web.yatrisathi.in", "www.chennaione.in",
    "metro-terms.triffy.in",
    # Social media
    "twitter.com", "x.com", "www.instagram.com", "www.facebook.com",
    "medium.com", "t.me", "in.linkedin.com", "whatsapp.com",
    # Internal dashboards (user-facing, not API endpoints)
    "control-center.moving.tech", "logs.moving.tech", "moving.tech",
    # Placeholder domains
    "example.com", "dummyurl", "test.org",
    "www.ondcTextApi.com", "sandbox.assets.moving.techmaster",
    "webhook.site", "www.google.com",
    # Misc — bitbucket/internal (not API)
    "bitbucket.juspay.net",
    # Dead/test endpoints in registry (third-party BPP stubs)
    "cabs.dev.bap.urownsite.xyz", "fmd-test.free.beeceptor.com",
    "shop.pinpark.co.in", "api-d2c.marutisuzukicollatex.com",
    # Dead ngrok tunnels
    "12d0-65-2-105-122.ngrok-free.app",
}

_RE_HTTPS = re.compile(r'https://([^/\s"\\,}{)\]]+)([^\s"\\,}{)\]]*)')
_RE_HTTP_NON_LOCAL = re.compile(
    r'http://(?!localhost)(?!0\.0\.0\.0)(?!127\.0\.0\.1)([^\s"\\,}{)\]]+)')
_RE_SVC_CLUSTER = re.compile(r'[a-z0-9-]+\.[a-z]+\.svc\.cluster\.local')
_RE_IP_PORT = re.compile(r'(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}):(\d+)')


def _audit_patched_output(patched_base, source_base, config_tables, schemas):
    """Scan patched output for remaining sensitive patterns.

    Returns a list of warning strings.
    """
    from collections import defaultdict

    warnings = []

    # Collect all encrypted values from source for comparison
    source_encrypted = set()
    for schema in schemas:
        if schema not in config_tables:
            continue
        for table in list_table_files(source_base, "", schema):
            for _, batch in iter_table_rows(source_base, "", schema, table):
                for row in batch:
                    for val in row.values():
                        if isinstance(val, str):
                            source_encrypted.update(_RE_ENCRYPT.findall(val))

    # Scan patched output
    ext_urls = defaultdict(set)       # domain -> set of files
    non_local_http = defaultdict(set)  # host -> set of files
    unchanged_enc = defaultdict(int)   # file -> count
    cluster_urls = defaultdict(set)    # url -> set of files
    ip_ports = defaultdict(set)        # ip:port -> set of files

    for schema in schemas:
        if schema not in config_tables:
            continue
        for table in list_table_files(patched_base, "", schema):
            rel = f"{schema}/{table}"
            for _, batch in iter_table_rows(patched_base, "", schema, table):
                for row in batch:
                    for val in row.values():
                        if not isinstance(val, str):
                            continue

                        # External HTTPS
                        for domain, _ in _RE_HTTPS.findall(val):
                            host = domain.split(":")[0]
                            if host not in _AUDIT_SAFE_DOMAINS and host != "localhost":
                                ext_urls[host].add(rel)

                        # Non-localhost HTTP
                        for host_path in _RE_HTTP_NON_LOCAL.findall(val):
                            host = host_path.split("/")[0].split(":")[0]
                            if host not in _AUDIT_SAFE_DOMAINS and \
                               host not in ("dummyReturnUrl", "dummyUrl", "string"):
                                non_local_http[host].add(rel)

                        # Unchanged encrypted values
                        for enc in _RE_ENCRYPT.findall(val):
                            if enc in source_encrypted:
                                unchanged_enc[rel] += 1

                        # K8s cluster URLs
                        for svc in _RE_SVC_CLUSTER.findall(val):
                            cluster_urls[svc].add(rel)

                        # IP:port
                        for ip, port in _RE_IP_PORT.findall(val):
                            if ip not in ("0.0.0.0", "127.0.0.1"):
                                ip_ports[f"{ip}:{port}"].add(rel)

    # Build warnings
    if ext_urls:
        warnings.append(f"[URL] {len(ext_urls)} external HTTPS domains still present:")
        for domain, files in sorted(ext_urls.items()):
            warnings.append(f"       https://{domain}  ({', '.join(sorted(files))})")

    if non_local_http:
        warnings.append(f"[URL] {len(non_local_http)} non-localhost HTTP hosts:")
        for host, files in sorted(non_local_http.items()):
            warnings.append(f"       http://{host}  ({', '.join(sorted(files))})")

    if unchanged_enc:
        total = sum(unchanged_enc.values())
        warnings.append(f"[ENCRYPT] {total} encrypted values unchanged from source:")
        for rel, count in sorted(unchanged_enc.items(), key=lambda x: -x[1]):
            warnings.append(f"       {rel}: {count} values")

    if cluster_urls:
        warnings.append(f"[K8S] {len(cluster_urls)} cluster-internal URLs:")
        for url, files in sorted(cluster_urls.items()):
            warnings.append(f"       {url}  ({', '.join(sorted(files))})")

    if ip_ports:
        warnings.append(f"[IP] {len(ip_ports)} raw IP:port addresses:")
        for addr, files in sorted(ip_ports.items()):
            warnings.append(f"       {addr}  ({', '.join(sorted(files))})")

    return warnings


def cmd_import(args):
    """Import: read patched table files, validate, write SQL per table. No patching.

    SQL output structure (--dry-run):
      <direction>_sql/
        _warnings.txt              — coverage warnings for review
        _run_order.txt             — ordered list of SQL files to execute
        <schema>/
          <table>_delete.sql       — DELETE statements
          <table>_insert_0000.sql  — INSERT batch 0
          <table>_insert_0001.sql  — INSERT batch 1 (large tables)
          ...

    Without --dry-run: executes all SQL files in order within BEGIN/COMMIT.
    """
    from_env = args.source_env
    to_env = args.target_env

    if to_env == "prod":
        sys.exit("FATAL: Importing into prod is NEVER allowed.")
    if (from_env, to_env) not in ALLOWED_TRANSFERS:
        allowed = ", ".join(f"{s}->{t}" for s, t in sorted(ALLOWED_TRANSFERS))
        sys.exit(f"Transfer {from_env}->{to_env} not allowed.\nAllowed: {allowed}")

    direction = f"{from_env}_to_{to_env}"
    print(f"Import: {from_env} -> {to_env} {'(DEV)' if IS_DEV else '(S3)'}\n")

    env_config = load_environments()
    config_tables = load_config_tables()

    schemas = args.schemas or list(config_tables.keys())

    # Read from patched data directory
    patched_base = DATA_DIR / direction
    if not patched_base.exists():
        sys.exit(f"Patched data not found at {patched_base}\n"
                 f"Run: DEV=true python config_transfer.py patch --from {from_env} --to {to_env}")

    # SQL output directory
    sql_dir = SCRIPT_DIR / f"{direction}_sql"
    if sql_dir.exists():
        shutil.rmtree(sql_dir)
    sql_dir.mkdir()

    all_warnings = []
    run_order = []  # ordered list of SQL file paths (relative to sql_dir)

    for schema in schemas:
        if schema not in config_tables:
            continue
        schema_config = config_tables[schema]

        if IS_DEV or args.local_dir:
            base = Path(args.local_dir) if args.local_dir else patched_base
            available = list_table_files(base, "", schema)
        else:
            # S3 mode — list from config, fetch on demand
            base = patched_base
            available = table_names_for_schema(schema_config)

        if not available:
            print(f"[skip] No data for {schema}")
            continue

        target_db = get_db_config(env_config, to_env, schema)
        target_db_schema = target_db.get("db_schema", schema)

        target_cursor = None
        try:
            target_conn = get_connection(target_db)
            target_cursor = target_conn.cursor(cursor_factory=psycopg2.extras.RealDictCursor)
        except Exception as e:
            print(f"  WARNING: Cannot connect to target ({e}), using source columns")

        schema_sql_dir = sql_dir / schema
        schema_sql_dir.mkdir()

        print(f"{'=' * 60}")
        print(f"{schema} -> {target_db_schema} ({len(available)} tables)")
        print(f"{'=' * 60}")

        for table in available:
            dim_cols = get_dim_columns(schema_config, table)

            meta = read_table_meta(base, "", schema, table)
            if not meta:
                print(f"    [skip] {table}: no data")
                continue

            columns = meta.get("columns", [])

            tgt_cols = None
            not_null_cols = set()
            col_types = {}
            head_row = {}
            if target_cursor:
                try:
                    cols_result, col_types, not_null_cols = fetch_target_table_columns(target_cursor, target_db_schema, table)
                    tgt_cols = cols_result if cols_result else None
                    head_row = fetch_target_table_head(target_cursor, target_db_schema, table)
                except Exception as e:
                    print(f"    [WARN] {table}: cannot read target schema ({e})")

            # ── First pass: collect dim values ──
            source_dims = set()
            total_rows = 0
            for _, batch_rows in iter_table_rows(base, "", schema, table):
                total_rows += len(batch_rows)
                for row in batch_rows:
                    if dim_cols:
                        source_dims.add(tuple(row.get(c) for c in dim_cols))

            if total_rows == 0:
                print(f"    [skip] {table}: 0 rows")
                continue

            # ── Coverage warnings ──
            if target_cursor and dim_cols:
                try:
                    warns = check_target_coverage_from_dims(
                        target_cursor, target_db_schema, table, source_dims, dim_cols
                    )
                    if warns:
                        all_warnings.append(f"{target_db_schema}.{table}: {len(warns)} dim values left untouched:")
                        for w in warns:
                            all_warnings.append(f"    {w}")
                except Exception:
                    pass

            # ── Write DELETE SQL file ──
            delete_sql = generate_delete_sql(target_db_schema, table, dim_cols, source_dims, total_rows)
            delete_file = schema_sql_dir / f"{table}_delete.sql"
            delete_file.write_text(delete_sql)
            run_order.append(f"{schema}/{table}_delete.sql")

            # ── Second pass: write INSERT SQL files batch by batch (data already patched) ──
            batch_idx = 0
            for _, batch_rows in iter_table_rows(base, "", schema, table):
                insert_sql = generate_insert_sql(
                    target_db_schema, table, batch_rows, columns,
                    tgt_cols, head_row, not_null_cols=not_null_cols, col_types=col_types
                )
                insert_file = schema_sql_dir / f"{table}_insert_{batch_idx:04d}.sql"
                insert_file.write_text(insert_sql)
                run_order.append(f"{schema}/{table}_insert_{batch_idx:04d}.sql")
                batch_idx += 1

            files_msg = f", {batch_idx + 1} sql files" if batch_idx > 1 else ""
            print(f"    [ok] {table}: {total_rows} rows{files_msg}")

        if target_cursor:
            target_cursor.close()
            target_conn.close()
        print()

    if not run_order:
        print("Nothing to import.")
        shutil.rmtree(sql_dir)
        return

    # ── Write run order + warnings ──
    (sql_dir / "_run_order.txt").write_text("\n".join(run_order) + "\n")
    if all_warnings:
        (sql_dir / "_warnings.txt").write_text("\n".join(all_warnings) + "\n")

    print(f"Generated {len(run_order)} SQL files in {sql_dir}/")
    if all_warnings:
        print(f"{len(all_warnings)} coverage warnings (see {sql_dir}/_warnings.txt)")

    if args.dry_run:
        print(f"\nDry run complete. Review SQL files, then run without --dry-run to execute.")
        return

    # ── Discover feature-migration SQL files (will be executed after all schema imports) ──
    feature_migrations_dir = SCRIPT_DIR.parent / "feature-migrations"
    feature_migration_files = []
    if feature_migrations_dir.is_dir():
        feature_migration_files = sorted(
            f for f in feature_migrations_dir.iterdir()
            if f.suffix == ".sql" and not f.name.startswith("_")
        )
    if feature_migration_files:
        print(f"  Found {len(feature_migration_files)} feature-migration(s) to run after import.")

    # ── Execute SQL files per schema (each schema may need a different DB user) ──
    # Group run_order by schema
    from collections import OrderedDict
    schema_files = OrderedDict()
    for rel_path in run_order:
        schema_name = rel_path.split("/")[0]
        schema_files.setdefault(schema_name, []).append(rel_path)

    print(f"\nExecuting {len(run_order)} SQL files against {to_env}...")
    total_executed = 0

    for schema_name, files in schema_files.items():
        target_db = get_db_config(env_config, to_env, schema_name)
        print(f"  {schema_name} ({len(files)} files, user={target_db['user']})...")

        conn = get_connection(target_db)
        cursor = conn.cursor()
        try:
            db_schema = target_db.get("db_schema", schema_name)
            cursor.execute("BEGIN;")
            cursor.execute("SET session_replication_role = 'replica';")  # disable FK checks

            # Collect PK constraints + their columns, then drop
            # (master often has no PKs; source data may have duplicates on local PK columns)
            tables_touched = list(set(
                f.split("/")[1].rsplit("_delete", 1)[0].rsplit("_insert_", 1)[0] for f in files
            ))
            # Drop PK + UNIQUE constraints on touched tables (master often has none)
            cursor.execute("""
                SELECT tc.constraint_name, tc.table_name, tc.constraint_type,
                       kcu.column_name, kcu.ordinal_position
                FROM information_schema.table_constraints tc
                JOIN information_schema.key_column_usage kcu
                    ON tc.constraint_name = kcu.constraint_name AND tc.table_schema = kcu.table_schema
                WHERE tc.table_schema = %s AND tc.constraint_type IN ('PRIMARY KEY', 'UNIQUE')
                  AND tc.table_name = ANY(%s)
                ORDER BY tc.table_name, kcu.ordinal_position
            """, (db_schema, tables_touched))
            constraint_info = {}  # {(constraint_name, table_name, type): [columns]}
            for cname, tname, ctype, col, _ in cursor.fetchall():
                constraint_info.setdefault((cname, tname, ctype), []).append(col)

            for (cname, tname, _) in constraint_info:
                cursor.execute(f'ALTER TABLE "{db_schema}"."{tname}" DROP CONSTRAINT IF EXISTS "{cname}" CASCADE;')
            if constraint_info:
                pk_count = sum(1 for (_, _, t) in constraint_info if t == 'PRIMARY KEY')
                uq_count = sum(1 for (_, _, t) in constraint_info if t == 'UNIQUE')
                print(f"    Dropped {pk_count} PK + {uq_count} UNIQUE constraints")

            for i, rel_path in enumerate(files):
                sql_file = sql_dir / rel_path
                sql = sql_file.read_text()
                if sql.strip():
                    cursor.execute(sql)
                if (i + 1) % 100 == 0:
                    print(f"    {i + 1}/{len(files)}...")

            # Re-add constraints (skip if data has duplicates — matches master)
            re_added = 0
            skipped = []
            for (cname, tname, ctype), cols in constraint_info.items():
                col_list = ", ".join(f'"{c}"' for c in cols)
                cursor.execute(f"SAVEPOINT constraint_restore;")
                try:
                    if ctype == 'PRIMARY KEY':
                        cursor.execute(f'ALTER TABLE "{db_schema}"."{tname}" ADD CONSTRAINT "{cname}" PRIMARY KEY ({col_list});')
                    else:
                        cursor.execute(f'ALTER TABLE "{db_schema}"."{tname}" ADD CONSTRAINT "{cname}" UNIQUE ({col_list});')
                    cursor.execute(f"RELEASE SAVEPOINT constraint_restore;")
                    re_added += 1
                except Exception:
                    cursor.execute(f"ROLLBACK TO SAVEPOINT constraint_restore;")
                    skipped.append(f"{tname}.{cname}")
            if re_added:
                print(f"    Re-added {re_added}/{len(constraint_info)} constraints")
            if skipped:
                print(f"    Skipped (data has dupes): {', '.join(skipped)}")

            cursor.execute("SET session_replication_role = 'origin';")  # re-enable FK checks
            cursor.execute("COMMIT;")
            total_executed += len(files)
            print(f"    Done — {len(files)} files.")
        except Exception as e:
            conn.rollback()
            print(f"    FAILED at {rel_path}: {e}", file=sys.stderr)
            print(f"    SQL files preserved in {sql_dir}/", file=sys.stderr)
            sys.exit(1)
        finally:
            cursor.close()
            conn.close()

    # ── Run feature-migrations (each file may reference multiple schemas) ──
    if feature_migration_files:
        # Use the target env's default connection (all schemas accessible on local)
        first_schema = list(schema_files.keys())[0]
        fm_db = get_db_config(env_config, to_env, first_schema)
        fm_conn = get_connection(fm_db)
        fm_cursor = fm_conn.cursor()
        try:
            print(f"\nRunning {len(feature_migration_files)} feature-migration(s)...")
            for fm_file in feature_migration_files:
                print(f"  {fm_file.name}...")
                fm_cursor.execute("BEGIN;")
                fm_cursor.execute(fm_file.read_text())
                fm_cursor.execute("COMMIT;")
            print(f"  Feature-migrations complete.")
        except Exception as e:
            fm_conn.rollback()
            print(f"  FAILED at {fm_file.name}: {e}", file=sys.stderr)
            sys.exit(1)
        finally:
            fm_cursor.close()
            fm_conn.close()

    print(f"\nAll done — {total_executed} SQL files executed across {len(schema_files)} schemas.")


def cmd_list(args):
    config_tables = load_config_tables()
    schemas = args.schemas or list(config_tables.keys())
    env_config = load_environments() if args.env else None

    for schema in schemas:
        sc = config_tables.get(schema, {})
        tables = table_names_for_schema(sc)
        print(f"\n{schema} ({len(tables)} tables):")

        if env_config:
            db = get_db_config(env_config, args.env, schema)
            db_schema = db.get("db_schema", schema)
            try:
                conn = get_connection(db)
                cur = conn.cursor()
                for t in tables:
                    dim = get_dim_columns(sc, t)
                    dim_str = ", ".join(dim) if dim else "(full replace)"
                    if table_exists(cur, db_schema, t):
                        cur.execute(f'SELECT COUNT(*) FROM "{db_schema}"."{t}"')
                        cnt = cur.fetchone()[0]
                        print(f"  {t}: {cnt} rows  (dim: {dim_str})")
                    else:
                        print(f"  {t}: (missing)  (dim: {dim_str})")
                cur.close(); conn.close()
            except Exception as e:
                print(f"  DB error: {e}")
        else:
            for t in tables:
                dim = get_dim_columns(sc, t)
                dim_str = ", ".join(dim) if dim else "(full replace)"
                print(f"  {t}  (dim: {dim_str})")


def cmd_show_patches(args):
    direction = f"{args.source_env}_to_{args.target_env}"
    pc = load_patches().get(direction, {})
    if not pc:
        print(f"No patches for {direction}")
        return

    print(f"Patches for {direction}:\n")
    gr = pc.get("global_replacements", [])
    if gr:
        print(f"  Global replacements ({len(gr)}):")
        for r in gr:
            if not r.get("find"):
                continue
            print(f"    '{r['find']}' -> '{r['replace']}'")

    for schema, rules in pc.get("schema_replacements", {}).items():
        if schema.startswith("_") or not rules:
            continue
        print(f"\n  Schema replacements for {schema}:")
        for r in rules:
            print(f"    '{r['find']}' -> '{r['replace']}'")

    for schema, tables in pc.get("table_overrides", {}).items():
        if schema.startswith("_"):
            continue
        for tbl, ovrs in tables.items():
            if ovrs:
                print(f"\n  Field overrides for {schema}.{tbl}:")
                for o in ovrs:
                    print(f"    {o['field']} = {json.dumps(o['value'])}")

    for schema, tables in pc.get("dimension_overrides", {}).items():
        if schema.startswith("_"):
            continue
        for tbl, rules in tables.items():
            if rules:
                print(f"\n  Dimension overrides for {schema}.{tbl}:")
                for rule in rules:
                    w = ", ".join(f"{k}={v}" for k, v in rule["where"].items())
                    s = ", ".join(f"{k}={json.dumps(v)}" for k, v in rule["set"].items())
                    print(f"    WHERE ({w}) SET ({s})")


# ── CLI ──────────────────────────────────────────────────────────────────────


def main():
    parser = argparse.ArgumentParser(
        description="NammayYatri Config Transfer",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Three-step workflow:
  1. export — reads source DB table-by-table, stores raw JSON
  2. patch  — applies overrides (URL replacements, re-encryption, merge_json)
  3. import — reads patched JSON, generates SQL, executes

Examples:
  DEV=true python config_transfer.py export --from master --parallel 10
  DEV=true python config_transfer.py patch --from master --to local
  DEV=true python config_transfer.py import --from master --to local --dry-run
  DEV=true python config_transfer.py import --from master --to local
        """,
    )
    sub = parser.add_subparsers(dest="command", required=True)

    p_exp = sub.add_parser("export", help="Export raw config from source DB")
    p_exp.add_argument("--from", dest="source_env", required=True, choices=VALID_ENVS)
    p_exp.add_argument("--schema", dest="schemas", action="append")
    p_exp.add_argument("--table", dest="tables", action="append", help="Export only specific tables (e.g. --table fare_policy --table fare_product)")
    p_exp.add_argument("--parallel", type=int, default=4, help="Number of parallel table exports")
    p_exp.add_argument("--s3-bucket")
    p_exp.add_argument("--s3-prefix", default="config-sync")

    p_pat = sub.add_parser("patch", help="Apply overrides to exported data")
    p_pat.add_argument("--from", dest="source_env", required=True, choices=VALID_ENVS)
    p_pat.add_argument("--to", dest="target_env", required=True, choices=VALID_ENVS)
    p_pat.add_argument("--schema", dest="schemas", action="append")

    p_imp = sub.add_parser("import", help="Import patched config into target DB")
    p_imp.add_argument("--from", dest="source_env", required=True, choices=VALID_ENVS)
    p_imp.add_argument("--to", dest="target_env", required=True, choices=VALID_ENVS)
    p_imp.add_argument("--schema", dest="schemas", action="append")
    p_imp.add_argument("--dry-run", action="store_true")
    p_imp.add_argument("--local-dir", help="Read from custom dir instead of patched data")
    p_imp.add_argument("--s3-bucket")
    p_imp.add_argument("--s3-prefix", default="config-sync")

    p_list = sub.add_parser("list", help="List config tables")
    p_list.add_argument("--schema", dest="schemas", action="append")
    p_list.add_argument("--env")

    p_patch = sub.add_parser("show-patches", help="Show patches for a direction")
    p_patch.add_argument("--from", dest="source_env", required=True, choices=VALID_ENVS)
    p_patch.add_argument("--to", dest="target_env", required=True, choices=VALID_ENVS)

    args = parser.parse_args()
    if args.command == "export":
        cmd_export(args)
    elif args.command == "patch":
        cmd_patch(args)
    elif args.command == "import":
        cmd_import(args)
    elif args.command == "list":
        cmd_list(args)
    elif args.command == "show-patches":
        cmd_show_patches(args)


if __name__ == "__main__":
    main()
