#!/usr/bin/env python3
"""
Unified mock server for NammaYatri merchant service providers.

Each service handler lives in services/<name>.py with a handle(handler, path, body) function.

Two override mechanisms for test automation:

1. POST /mock/status  — keyed by explicit ID (e.g., payment order ID)
   {"service": "juspay", "id": ["order-123"], "status": "CHARGED", "data": {"amount": 150}}

2. POST /mock/override — keyed by request field extraction (middleware)
   {"service": "cmrl", "extract": "body.mob", "value": "9876543210",
    "response": {"returnCode": "500", "returnMessage": "Technical Failure"}}

   Extract syntax:
     body.<json_path>  — from JSON body (nested: body.data.customer.phone)
     path.<index>      — from URL path segment (0-based)
     query.<param>     — from query parameter
     header.<name>     — from request header

   Every handler receives matched overrides via handler._request_override dict.
"""

import argparse
import json
import logging
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse

import status_store
from services import juspay, stripe, paytm, exotel, acko, sos, kapture, whatsapp, mmi, nextbillion, gridline, transit, hyperverge, gullak, openai, cmrl, cris, ebix, mlpricing, ondc

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
log = logging.getLogger("mock-server")

# Route table: (path substring, module, service name for overrides)
ROUTES = [
    ("/juspay",      juspay,      "juspay"),
    ("/stripe",      stripe,      "stripe"),
    ("/paytm",       paytm,       "paytm"),
    ("/exotel",      exotel,      "exotel"),
    ("/acko",        acko,        "acko"),
    ("/erss",        sos,         "sos"),
    ("/sos",         sos,         "sos"),
    ("/kapture",     kapture,     "kapture"),
    ("/gupshup",     whatsapp,    "whatsapp"),
    ("/karix",       whatsapp,    "whatsapp"),
    ("/mmi",         mmi,         "mmi"),
    ("/nextbillion", nextbillion, "nextbillion"),
    ("/gridline",    gridline,    "gridline"),
    ("/nandi",       transit,     "transit"),
    ("/gtfs",        transit,     "transit"),
    ("/hyperverge",  hyperverge,  "hyperverge"),
    ("/gullak",      gullak,      "gullak"),
    ("/openai",      openai,      "openai"),
    ("/cmrl",        cmrl,        "cmrl"),
    ("/cris",        cris,        "cris"),
    ("/ebix",        ebix,        "ebix"),
    ("/mlpricing",   mlpricing,   "mlpricing"),
    ("/ondc",        ondc,        "ondc"),
]


class MockHandler(BaseHTTPRequestHandler):

    def do_GET(self):
        self._handle()

    def do_POST(self):
        self._handle()

    def do_PUT(self):
        self._handle()

    def do_DELETE(self):
        self._handle()

    # ── Override access for handlers ──

    def _get_override(self, service, *candidate_ids):
        """Check status store for an override matching any candidate ID.
        Also checks extract-based rules and merges request-level overrides."""
        status_val = None
        data = {}
        # Check explicit ID-based status store
        for cid in candidate_ids:
            if cid:
                entry = status_store.get_status(service, cid)
                if entry:
                    status_val = entry["status"]
                    data = entry.get("data", {})
                    break
        # If no explicit match, try extract-based rules
        if status_val is None:
            parsed = urlparse(self.path)
            extract_status, extract_data = status_store.get_extract_status(
                service, parsed.path, None, None, None)
            if extract_status:
                status_val = extract_status
                data = extract_data
        # Merge request-level overrides from middleware (set by _apply_overrides)
        req_override = getattr(self, "_request_override", {})
        if req_override:
            data = status_store.deep_merge(data, req_override)
        return status_val, data

    # ── Main handler ──

    def _handle(self):
        parsed = urlparse(self.path)
        path = parsed.path
        query = parsed.query
        body = self._read_body()

        log.info(f"{self.command} {self.path}")

        # ── Mock APIs ──
        if path == "/mock/status":
            return self._mock_status(body)
        if path == "/mock/override":
            return self._mock_override(body)

        if path == "/" or path == "/health":
            return self._json({"status": "UP", "service": "mock-server"})

        # ── Flush Redis (for test orchestration) ──
        if path == "/flush-redis" and self.command == "POST":
            import subprocess
            subprocess.run(["redis-cli", "-p", "6379", "FLUSHALL"], capture_output=True)
            for p in [30001, 30002, 30003, 30004, 30005, 30006]:
                subprocess.run(["redis-cli", "-p", str(p), "-c", "FLUSHALL"], capture_output=True)
            return self._json({"flushed": True})

        # ── Generic SQL endpoints (for scheduler tests and DB fixtures) ──
        if path == "/mock/sql/select" and self.command == "POST":
            return self._mock_sql_select(body)
        if path == "/mock/sql/update" and self.command == "POST":
            return self._mock_sql_update(body)

        # ── Generic scheduler-job trigger (for scheduler tests) ──
        if path == "/mock/scheduler/trigger" and self.command == "POST":
            return self._mock_scheduler_trigger(body)
        if path == "/mock/scheduler/peek" and self.command == "POST":
            return self._mock_scheduler_peek(body)
        if path == "/mock/scheduler/clear" and self.command == "POST":
            return self._mock_scheduler_clear(body)

        # ── Route to service handler with middleware override check ──
        for prefix, module, svc_name in ROUTES:
            if prefix in path:
                # Middleware: check override rules and attach to handler
                self._apply_overrides(svc_name, path, query, body)
                module.handle(self, path, body)
                return

        self._json({"status": "ok", "mock": True, "path": path})

    def _apply_overrides(self, service, path, query, body):
        """Middleware: check all override rules for this service, extract IDs from request,
        and attach matched response overrides to self._request_override.
        For services with encrypted bodies (CRIS, CMRL), registered body decoders
        are used to decrypt and parse the request before matching."""
        body_parsed = None
        if body:
            try:
                text = body.decode("utf-8") if isinstance(body, bytes) else body
                body_parsed = json.loads(text)
            except (json.JSONDecodeError, ValueError):
                pass

        headers = {k.lower(): v for k, v in self.headers.items()}
        override = status_store.check_overrides(service, path, query, headers, body_parsed, body_raw=body)
        self._request_override = override

    # ── Mock status API (existing) ──

    def _mock_status(self, body):
        if self.command == "GET":
            result = {"statuses": status_store.list_statuses(), "overrides": status_store.list_overrides()}
            return self._json(result)

        if self.command == "DELETE":
            status_store.clear_statuses()
            status_store.clear_overrides()
            return self._json({"cleared": True})

        if self.command == "POST":
            try:
                data = json.loads(body) if body else {}
            except json.JSONDecodeError:
                return self._json({"error": "invalid JSON"}, status=400)

            service = data.get("service")
            identifiers = data.get("id")
            status = data.get("status")
            extract = data.get("extract")  # e.g. "path.2" to extract ID from URL path index 2

            if not service or not status:
                return self._json({"error": "service and status are required"}, status=400)
            if not identifiers and not extract:
                return self._json({"error": "either id (string or array) or extract (e.g. 'path.2') is required"}, status=400)

            if identifiers:
                count = status_store.set_status(service, identifiers, status, data.get("data"))
                return self._json({"ok": True, "service": service, "id": identifiers, "status": status, "ttl": status_store.STATUS_TTL, "updated": count})
            else:
                # Store as an extract-based rule: when a request to this service
                # has a matching path/body/query value, return this status
                match = data.get("match")  # e.g. "/orders" — only trigger on paths containing this
                status_store.set_extract_status(service, extract, status, match=match, data=data.get("data"))
                return self._json({"ok": True, "service": service, "extract": extract, "match": match, "status": status, "ttl": status_store.STATUS_TTL})

        return self._json({"error": "method not allowed"}, status=405)

    # ── Mock override API (new) ──

    def _mock_override(self, body):
        """
        POST /mock/override — add a request-matching override rule
          {"service": "cmrl", "extract": "body.mob", "value": "9876543210",
           "response": {"returnCode": "500", "returnMessage": "Technical Failure"}}

        GET /mock/override — list all active overrides

        DELETE /mock/override — clear all overrides
        """
        if self.command == "GET":
            return self._json(status_store.list_overrides())

        if self.command == "DELETE":
            try:
                data = json.loads(body) if body else {}
            except json.JSONDecodeError:
                return self._json({"error": "invalid JSON"}, status=400)

            service = data.get("service")
            extract = data.get("extract")
            value = data.get("value")

            if not service or not extract or value is None:
                return self._json({"error": "service, extract, and value are required"}, status=400)

            removed = status_store.delete_override(service, extract, str(value))
            return self._json({"ok": True, "removed": removed, "service": service, "extract": extract, "value": str(value)})

        if self.command == "POST":
            try:
                data = json.loads(body) if body else {}
            except json.JSONDecodeError:
                return self._json({"error": "invalid JSON"}, status=400)

            service = data.get("service")
            extract = data.get("extract")
            value = data.get("value")
            response = data.get("response")

            if not service or not extract or value is None or not response:
                return self._json({"error": "service, extract, value, and response are required"}, status=400)

            status_store.add_override(service, extract, value, response)
            return self._json({"ok": True, "service": service, "extract": extract, "value": str(value), "ttl": status_store.STATUS_TTL})

        return self._json({"error": "method not allowed"}, status=405)

    # ── Generic SQL query endpoints (for scheduler tests / DB fixtures) ──
    #
    # Two endpoints expose SELECT and UPDATE against any dev-DB table. Table,
    # schema, and column identifiers go through psycopg2.sql.Identifier so
    # they're safely quoted; values are bound as parameters. Whitelisted
    # operators only. These are dev-only — the mock server is not exposed
    # outside the dev stack.

    _SQL_WHERE_OPS = {"=", "!=", "<", "<=", ">", ">=", "LIKE", "IN", "IS NULL", "IS NOT NULL"}
    _SQL_DB_DEFAULT = {"host": "localhost", "port": 5434, "user": "atlas_app_user"}

    def _build_where(self, clauses):
        """Build a (SQL, params) pair from a where_clause list.
        Each clause is {"column_name": ..., "val": ..., "op": "="}.
        `val` is ignored for IS NULL / IS NOT NULL; for IN, `val` must be a list."""
        from psycopg2 import sql as psql
        if not clauses:
            return psql.SQL(""), []
        parts = []
        params = []
        for c in clauses:
            col = c.get("column_name")
            op = (c.get("op") or "=").upper()
            if not col:
                raise ValueError("where_clause entry missing column_name")
            if op not in self._SQL_WHERE_OPS:
                raise ValueError(f"unsupported op {op!r}; allowed: {sorted(self._SQL_WHERE_OPS)}")
            if op in ("IS NULL", "IS NOT NULL"):
                parts.append(psql.SQL("{} {}").format(psql.Identifier(col), psql.SQL(op)))
            elif op == "IN":
                val = c.get("val")
                if not isinstance(val, list) or not val:
                    raise ValueError(f"op=IN on column {col} requires non-empty list val")
                placeholders = psql.SQL(", ").join(psql.Placeholder() * len(val))
                parts.append(psql.SQL("{} IN ({})").format(psql.Identifier(col), placeholders))
                params.extend(val)
            else:
                parts.append(psql.SQL("{} " + op + " {}").format(psql.Identifier(col), psql.Placeholder()))
                params.append(c.get("val"))
        return psql.SQL(" WHERE ") + psql.SQL(" AND ").join(parts), params

    def _pg_connect(self, db_name):
        import psycopg2
        return psycopg2.connect(dbname=db_name, **self._SQL_DB_DEFAULT)

    def _parse_sql_target(self, body):
        """Parse common SQL body fields. Returns (data, db_name, schema, table).
        Raises ValueError on invalid JSON or missing required fields."""
        try:
            data = json.loads(body) if body else {}
        except json.JSONDecodeError:
            raise ValueError("invalid JSON")
        db_name = data.get("db_name")
        schema = data.get("db_schema")
        table = data.get("table_name")
        if not db_name or not schema or not table:
            raise ValueError("db_name, db_schema, table_name are required")
        return data, db_name, schema, table

    def _mock_sql_select(self, body):
        """POST /mock/sql/select — run a SELECT against a dev DB table.
        Body: {
          "db_name": "atlas_dev",
          "db_schema": "atlas_app",
          "table_name": "purchased_pass",
          "select": ["id", "status"],         # optional, defaults to ["*"]
          "where_clause": [{"column_name": "id", "val": "uuid", "op": "="}],
          "limit": 100                        # optional
        }
        Returns: {"rows": [{...}, ...], "count": N}"""
        from psycopg2 import sql as psql
        try:
            data, db_name, schema, table = self._parse_sql_target(body)
            where_sql, where_params = self._build_where(data.get("where_clause") or [])
        except ValueError as ve:
            return self._json({"error": str(ve)}, status=400)

        cols = data.get("select") or ["*"]
        limit = data.get("limit")

        select_sql = (
            psql.SQL("*") if cols == ["*"]
            else psql.SQL(", ").join(psql.Identifier(c) for c in cols)
        )
        query = psql.SQL("SELECT {cols} FROM {sch}.{tbl}{where}").format(
            cols=select_sql,
            sch=psql.Identifier(schema),
            tbl=psql.Identifier(table),
            where=where_sql,
        )
        if isinstance(limit, int) and limit > 0:
            query = query + psql.SQL(" LIMIT {}").format(psql.Literal(limit))

        try:
            conn = self._pg_connect(db_name)
            try:
                cur = conn.cursor()
                cur.execute(query, where_params)
                col_names = [d[0] for d in cur.description] if cur.description else []
                rows = [dict(zip(col_names, r)) for r in cur.fetchall()]
                cur.close()
            finally:
                conn.close()
            return self._json({"rows": rows, "count": len(rows)}, default=str)
        except Exception:
            log.exception("sql-select error")
            return self._json({"error": "internal server error"}, status=500)

    def _mock_sql_update(self, body):
        """POST /mock/sql/update — run an UPDATE against a dev DB table.
        Body: {
          "db_name": "atlas_dev",
          "db_schema": "atlas_app",
          "table_name": "purchased_pass",
          "set": {"status": "PreBooked"},     # column → value map
          "touch_updated_at": true,           # optional, appends updated_at = NOW()
          "where_clause": [{"column_name": "id", "val": "uuid", "op": "="}]
        }
        Returns: {"ok": true, "rowcount": N}"""
        from psycopg2 import sql as psql
        try:
            data, db_name, schema, table = self._parse_sql_target(body)
        except ValueError as ve:
            return self._json({"error": str(ve)}, status=400)

        set_map = data.get("set") or {}
        where = data.get("where_clause") or []
        if not isinstance(set_map, dict) or not set_map:
            return self._json({"error": "set must be a non-empty object"}, status=400)
        if not where:
            return self._json({"error": "where_clause required (refusing unscoped UPDATE)"}, status=400)

        try:
            where_sql, where_params = self._build_where(where)
        except ValueError as ve:
            return self._json({"error": str(ve)}, status=400)

        set_parts = [
            psql.SQL("{} = {}").format(psql.Identifier(c), psql.Placeholder())
            for c in set_map.keys()
        ]
        set_params = list(set_map.values())
        if data.get("touch_updated_at"):
            set_parts.append(psql.SQL("updated_at = NOW()"))

        query = psql.SQL("UPDATE {sch}.{tbl} SET {sets}{where}").format(
            sch=psql.Identifier(schema),
            tbl=psql.Identifier(table),
            sets=psql.SQL(", ").join(set_parts),
            where=where_sql,
        )

        try:
            conn = self._pg_connect(db_name)
            try:
                conn.autocommit = True
                cur = conn.cursor()
                cur.execute(query, set_params + where_params)
                rowcount = cur.rowcount
                cur.close()
            finally:
                conn.close()
            return self._json({"ok": True, "rowcount": rowcount})
        except Exception:
            log.exception("sql-update error")
            return self._json({"error": "internal server error"}, status=500)

    # ── Generic scheduler job trigger (for scheduler tests) ──

    # WARNING: mirrors the Haskell ToJSON (AnyJob t) instance at
    # lib/scheduler/src/Lib/Scheduler/Types.hs:115 — keep in sync.
    # Shard math mirrors createJobImpl at lib/scheduler/src/Lib/Scheduler/ScheduleJob.hs:120.
    _SCHEDULER_TARGETS = {
        "rider":  {"schedulerSetName": "Scheduled_Jobs_Rider",  "redisPort": 30001, "maxShards": 5},
        "driver": {"schedulerSetName": "Scheduled_Jobs_Driver", "redisPort": 30001, "maxShards": 5},
    }

    def _mock_scheduler_trigger(self, body):
        """POST /mock/scheduler/trigger — push an AnyJob into the scheduler's Redis ZSET.

        Body:
          {"jobType": "DailyPassStatusUpdate",
           "jobData": {...},                    # JobContent for this jobType (JSON object)
           "delaySeconds": 10,                  # when to fire (default 10)
           "merchantId": "optional-id",         # passed through on the AnyJob
           "merchantOperatingCityId": "optional-id",
           "target": "rider",                   # "rider" | "driver" (default "rider")
           "maxShards": 5,                      # override target default
           "schedulerSetName": "Scheduled_Jobs_Rider",  # override target default
           "redisPort": 30001}                  # override target default

        Reproduces what Haskell's createJobIn does for a RedisBased scheduler:
        computes the shard, builds the AnyJob JSON (id, storedJobInfo, status=Pending, etc.),
        and ZADDs it with score = scheduledAt epoch-ms. Producer picks it up on its next poll.

        Note: the KV layer needs a few seconds to settle after a write before the scheduler
        handler can read the row — keep delaySeconds >= 10 for tests that depend on a
        freshly-inserted row.
        """
        import uuid as _uuid
        import struct
        import subprocess
        import math
        from datetime import datetime, timedelta, timezone

        try:
            data = json.loads(body) if body else {}
        except json.JSONDecodeError:
            return self._json({"error": "invalid JSON"}, status=400)

        job_type = data.get("jobType")
        if not job_type:
            return self._json({"error": "jobType is required"}, status=400)

        target = data.get("target", "rider")
        preset = self._SCHEDULER_TARGETS.get(target)
        if preset is None:
            return self._json({"error": f"unknown target {target}; use one of {list(self._SCHEDULER_TARGETS)}"}, status=400)

        job_data = data.get("jobData", {})
        try:
            delay = float(data.get("delaySeconds", 10))
            max_shards = int(data.get("maxShards", preset["maxShards"]))
            redis_port = int(data.get("redisPort", preset["redisPort"]))
        except (TypeError, ValueError):
            return self._json({"error": "delaySeconds/maxShards/redisPort must be numeric"}, status=400)
        if not math.isfinite(delay) or delay < 0:
            return self._json({"error": "delaySeconds must be a finite non-negative number"}, status=400)
        if max_shards <= 0:
            return self._json({"error": "maxShards must be > 0"}, status=400)
        set_name = data.get("schedulerSetName", preset["schedulerSetName"])
        merchant_id = data.get("merchantId")
        moc_id = data.get("merchantOperatingCityId")

        job_uuid = str(_uuid.uuid4())
        # Haskell: shard = sum of 4 Word32s of the UUID, mod maxShards
        u = _uuid.UUID(job_uuid).bytes
        a, b, c, d = struct.unpack(">IIII", u)
        shard_id = (a + b + c + d) % max_shards

        # Haskell Aeson UTCTime: "YYYY-MM-DDTHH:MM:SS.ffffffZ"
        def iso(dt):
            return dt.strftime("%Y-%m-%dT%H:%M:%S.") + f"{dt.microsecond:06d}Z"

        now_utc = datetime.now(timezone.utc).replace(tzinfo=None)
        scheduled_at = now_utc + timedelta(seconds=delay)
        score_ms = int(scheduled_at.replace(tzinfo=timezone.utc).timestamp() * 1000)

        any_job = {
            "id": job_uuid,
            "storedJobInfo": {
                "storedJobType": job_type,
                "storedJobContent": json.dumps(job_data, separators=(",", ":")),
            },
            "shardId": shard_id,
            "scheduledAt": iso(scheduled_at),
            "createdAt": iso(now_utc),
            "updatedAt": iso(now_utc),
            "maxErrors": 5,
            "currErrors": 0,
            "status": "Pending",
            "parentJobId": job_uuid,
            "merchantId": merchant_id,
            "merchantOperatingCityId": moc_id,
        }
        member = json.dumps(any_job, separators=(",", ":"))
        zset_key = f"{set_name}{{{shard_id}}}"

        try:
            result = subprocess.run(
                ["redis-cli", "-c", "-p", str(redis_port),
                 "ZADD", zset_key, str(score_ms), member],
                capture_output=True, text=True, timeout=5,
            )
            if result.returncode != 0:
                return self._json({"error": "redis-cli failed", "stderr": result.stderr}, status=500)
            return self._json({
                "ok": True,
                "jobId": job_uuid,
                "jobType": job_type,
                "shardId": shard_id,
                "zsetKey": zset_key,
                "scheduledAt": iso(scheduled_at),
                "score": score_ms,
                "zaddResult": result.stdout.strip(),
            })
        except Exception:
            log.exception("scheduler-trigger error")
            return self._json({"error": "internal server error"}, status=500)

    def _mock_scheduler_peek(self, body):
        """POST /mock/scheduler/peek — list jobs in the scheduler's Redis ZSET, filtered by jobType.

        Body:
          {"jobType": "DailyPassStatusUpdate",   # required — filter by storedJobType
           "target": "rider",                    # "rider" | "driver" (default "rider")
           "schedulerSetName": "...",            # override target default
           "redisPort": 30001,                   # override target default
           "maxShards": 5}                       # override target default

        Returns: { "jobs": [{ "jobId", "jobType", "scheduledAt", "scoreMs", "shardId",
                              "merchantId", "merchantOperatingCityId", "jobData" }, ...] }

        Useful for asserting chain completion — e.g. that a DailyPassStatusUpdate job
        is scheduled for tomorrow 01:00 local after the current chain drains.
        """
        import subprocess
        from datetime import datetime, timezone

        try:
            data = json.loads(body) if body else {}
        except json.JSONDecodeError:
            return self._json({"error": "invalid JSON"}, status=400)

        job_type = data.get("jobType")
        if not job_type:
            return self._json({"error": "jobType is required"}, status=400)

        target = data.get("target", "rider")
        preset = self._SCHEDULER_TARGETS.get(target)
        if preset is None:
            return self._json({"error": f"unknown target {target}"}, status=400)

        try:
            max_shards = int(data.get("maxShards", preset["maxShards"]))
            redis_port = int(data.get("redisPort", preset["redisPort"]))
        except (TypeError, ValueError):
            return self._json({"error": "maxShards/redisPort must be numeric"}, status=400)
        set_name = data.get("schedulerSetName", preset["schedulerSetName"])

        jobs = []
        for shard in range(max_shards):
            zset_key = f"{set_name}{{{shard}}}"
            try:
                result = subprocess.run(
                    ["redis-cli", "-c", "-p", str(redis_port),
                     "ZRANGE", zset_key, "0", "-1", "WITHSCORES"],
                    capture_output=True, text=True, timeout=5,
                )
            except (subprocess.TimeoutExpired, OSError):
                continue
            if result.returncode != 0:
                continue
            lines = result.stdout.splitlines()
            # ZRANGE WITHSCORES returns alternating member/score lines
            for i in range(0, len(lines) - 1, 2):
                member_json = lines[i]
                score_str = lines[i + 1]
                try:
                    any_job = json.loads(member_json)
                except (json.JSONDecodeError, ValueError):
                    continue
                stored = any_job.get("storedJobInfo") or {}
                if stored.get("storedJobType") != job_type:
                    continue
                try:
                    score_ms = int(float(score_str))
                except (TypeError, ValueError):
                    score_ms = None
                scheduled_iso = None
                if score_ms is not None:
                    try:
                        scheduled_iso = datetime.fromtimestamp(
                            score_ms / 1000, tz=timezone.utc
                        ).isoformat()
                    except (OSError, ValueError):
                        scheduled_iso = None
                try:
                    job_data = json.loads(stored.get("storedJobContent") or "null")
                except (json.JSONDecodeError, ValueError):
                    job_data = None
                jobs.append({
                    "jobId": any_job.get("id"),
                    "jobType": job_type,
                    "scheduledAt": scheduled_iso,
                    "scoreMs": score_ms,
                    "shardId": shard,
                    "merchantId": any_job.get("merchantId"),
                    "merchantOperatingCityId": any_job.get("merchantOperatingCityId"),
                    "jobData": job_data,
                })

        jobs.sort(key=lambda j: j.get("scoreMs") or 0)
        return self._json({"jobs": jobs, "count": len(jobs)})

    def _mock_scheduler_clear(self, body):
        """POST /mock/scheduler/clear — purge all scheduler state for one jobType so a test starts clean.

        Removes (a) any pending entries of this jobType from Scheduled_Jobs_*{0..maxShards-1}
        ZSETs, optionally filtered by merchantOperatingCityId, and (b) any dedup-lock keys
        the handler may have set (matching `*mobility:locker:<jobType>:*`).

        Body:
          {"jobType": "DailyPassStatusUpdate",   # required
           "target": "rider",                    # "rider" | "driver" (default "rider")
           "merchantOperatingCityId": "...",     # optional — only clear jobs matching this MOC
           "schedulerSetName": "...",            # override target default
           "redisPort": 30001,                   # override target default
           "maxShards": 5,                       # override target default
           "lockPorts": [30001..30006]}          # cluster nodes to scan for lock keys

        Returns: {"removedJobs": N, "removedLocks": M}

        Use as the first step of a scheduler integration test to make it idempotent —
        the handler's date-keyed dedup lock would otherwise block re-enqueues across runs.
        """
        import subprocess

        try:
            data = json.loads(body) if body else {}
        except json.JSONDecodeError:
            return self._json({"error": "invalid JSON"}, status=400)

        job_type = data.get("jobType")
        if not job_type:
            return self._json({"error": "jobType is required"}, status=400)

        target = data.get("target", "rider")
        preset = self._SCHEDULER_TARGETS.get(target)
        if preset is None:
            return self._json({"error": f"unknown target {target}"}, status=400)

        try:
            max_shards = int(data.get("maxShards", preset["maxShards"]))
            redis_port = int(data.get("redisPort", preset["redisPort"]))
        except (TypeError, ValueError):
            return self._json({"error": "maxShards/redisPort must be numeric"}, status=400)
        set_name = data.get("schedulerSetName", preset["schedulerSetName"])
        moc_filter = data.get("merchantOperatingCityId")
        lock_ports = data.get("lockPorts") or [30001, 30002, 30003, 30004, 30005, 30006]

        # ── 1. ZREM matching pending jobs from each shard ──
        removed_jobs = 0
        for shard in range(max_shards):
            zset_key = f"{set_name}{{{shard}}}"
            try:
                result = subprocess.run(
                    ["redis-cli", "-c", "-p", str(redis_port), "ZRANGE", zset_key, "0", "-1"],
                    capture_output=True, text=True, timeout=5,
                )
            except (subprocess.TimeoutExpired, OSError):
                continue
            if result.returncode != 0:
                continue
            for member in result.stdout.splitlines():
                try:
                    any_job = json.loads(member)
                except (json.JSONDecodeError, ValueError):
                    continue
                stored = any_job.get("storedJobInfo") or {}
                if stored.get("storedJobType") != job_type:
                    continue
                if moc_filter and any_job.get("merchantOperatingCityId") != moc_filter:
                    continue
                try:
                    rm = subprocess.run(
                        ["redis-cli", "-c", "-p", str(redis_port), "ZREM", zset_key, member],
                        capture_output=True, text=True, timeout=5,
                    )
                    if rm.returncode == 0 and (rm.stdout.strip() or "0") != "0":
                        removed_jobs += 1
                except (subprocess.TimeoutExpired, OSError):
                    pass

        # ── 2. DEL dedup-lock keys (handler may have stored these via tryLockRedis) ──
        removed_locks = 0
        pattern = f"*mobility:locker:{job_type}:*"
        if moc_filter:
            pattern = f"*mobility:locker:{job_type}:*{moc_filter}*"
        for port in lock_ports:
            try:
                scan = subprocess.run(
                    ["redis-cli", "-c", "-p", str(port), "--scan", "--pattern", pattern],
                    capture_output=True, text=True, timeout=5,
                )
            except (subprocess.TimeoutExpired, OSError):
                continue
            if scan.returncode != 0:
                continue
            for key in scan.stdout.splitlines():
                if not key.strip():
                    continue
                try:
                    rm = subprocess.run(
                        ["redis-cli", "-c", "-p", str(port), "DEL", key],
                        capture_output=True, text=True, timeout=5,
                    )
                    if rm.returncode == 0 and (rm.stdout.strip() or "0") != "0":
                        removed_locks += 1
                except (subprocess.TimeoutExpired, OSError):
                    pass

        return self._json({"removedJobs": removed_jobs, "removedLocks": removed_locks})

    # ── Helpers ──

    def _read_body(self):
        length = int(self.headers.get("Content-Length", 0))
        if length > 0:
            return self.rfile.read(length)
        return b""

    def _json(self, data, status=200, default=None):
        body = json.dumps(data, default=default).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, format, *args):
        pass


def main():
    parser = argparse.ArgumentParser(description="Unified mock server for NammaYatri")
    parser.add_argument("--port", type=int, default=8080, help="Port (default: 8080)")
    args = parser.parse_args()

    server = HTTPServer(("0.0.0.0", args.port), MockHandler)
    log.info(f"Mock server running on :{args.port}")
    log.info("APIs: POST/GET/DELETE /mock/status, POST/GET/DELETE /mock/override, POST /mock/sql/select, POST /mock/sql/update, POST /mock/scheduler/trigger, POST /mock/scheduler/peek, POST /mock/scheduler/clear")
    log.info(f"Services: {', '.join(r[0].strip('/') for r in ROUTES)}")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        log.info("Shutting down")
        server.server_close()


if __name__ == "__main__":
    main()
