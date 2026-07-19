#!/usr/bin/env python3
"""
Unified mock server for NammaYatri merchant service providers.

Each service handler lives in services/<name>.py with a handle(handler, path, body) function.

Override mechanism for test automation:

  POST /mock/override — keyed by request field extraction (middleware)
   {"service": "cmrl", "extract": "body.mob", "value": "9876543210",
    "match": "/orders",   // optional: only fire when path contains this
    "response": {"returnCode": "500", "returnMessage": "Technical Failure"}}

   Extract syntax:
     body.<json_path>  — from JSON body (nested: body.data.customer.phone)
     path.<index>      — from URL path segment (0-based)
     query.<param>     — from query parameter
     header.<name>     — from request header

   Every handler receives matched overrides via handler._request_override dict.
"""

import argparse
import collections
import itertools
import json
import logging
import os
import queue
import threading
import time
from http.server import ThreadingHTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse

import status_store
from services import juspay, stripe, paytm, exotel, acko, sos, kapture, whatsapp, mmi, nextbillion, gridline, transit, hyperverge, gullak, openai, cmrl, cris, ebix, mlpricing, ondc, cac, fcm, sms, idfy, google, signzy, sandbox_proxy, sap

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
log = logging.getLogger("mock-server")

# ── Hit recording for the test-dashboard "Mock calls" panel ──
#
# Every routed outbound-service call captures a snapshot of (req, resp, duration).
# Capped ring buffer so a long-running mock-server doesn't grow unbounded.
# Admin endpoints (/mock/...) are NOT recorded — they're noise, not outbound calls.
_HITS_MAX = 4000
_HIT_BODY_MAX = 64 * 1024  # bytes, per direction
_HITS = collections.deque(maxlen=_HITS_MAX)
_HITS_LOCK = threading.Lock()
_HIT_SEQ = itertools.count(1)


def _truncate_for_hit(body):
    """Return body as a JSON-serializable value, truncating oversize payloads."""
    if body is None:
        return None
    if isinstance(body, bytes):
        if len(body) > _HIT_BODY_MAX:
            body = body[:_HIT_BODY_MAX]
        try:
            text = body.decode("utf-8")
        except UnicodeDecodeError:
            return {"_truncated": True, "_binary": True, "_bytes": len(body)}
    else:
        text = body if isinstance(body, str) else json.dumps(body, default=str)
        if len(text) > _HIT_BODY_MAX:
            text = text[:_HIT_BODY_MAX]
    try:
        return json.loads(text)
    except (json.JSONDecodeError, ValueError):
        return text


# SSE subscriber queues — each /mock/hits/stream client gets one, populated by
# record_hit. Bounded to avoid pinning memory if a slow client falls behind.
_STREAM_SUBSCRIBERS: list[queue.Queue] = []
_STREAM_LOCK = threading.Lock()


def record_hit(hit):
    with _HITS_LOCK:
        hit["id"] = next(_HIT_SEQ)
        _HITS.append(hit)
        published = dict(hit)
    with _STREAM_LOCK:
        for q in _STREAM_SUBSCRIBERS:
            try:
                q.put_nowait(published)
            except queue.Full:
                pass


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
    ("/nandi",         sandbox_proxy, "nandi"),
    ("/gtfs-inmemory", sandbox_proxy, "gtfs_inmemory"),
    ("/gtfs",          transit,       "transit"),
    ("/hyperverge",  hyperverge,  "hyperverge"),
    ("/gullak",      gullak,      "gullak"),
    ("/openai",      openai,      "openai"),
    ("/cmrl",        cmrl,        "cmrl"),
    ("/cris",        cris,        "cris"),
    ("/ebix",        ebix,        "ebix"),
    ("/mlpricing",   mlpricing,   "mlpricing"),
    ("/sap",         sap,         "sap"),
    ("/ondc",        ondc,        "ondc"),
    ("/cac",         cac,         "cac"),
    ("/fcm",         fcm,         "fcm"),
    ("/sms",         sms,         "sms"),
    ("/idfy",        idfy,        "idfy"),
    ("/maps",        google,      "google"),
    ("/signzy",      signzy,      "signzy"),
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
        """Return (status, extra_data) from the middleware-matched override.

        `candidate_ids` is accepted for signature compatibility with existing
        handlers but unused — matching is driven entirely by /mock/override
        rules (extract + value), evaluated in _apply_overrides.
        """
        del candidate_ids  # unused
        req_override = dict(getattr(self, "_request_override", {}) or {})
        if not req_override:
            return None, {}
        status_val = req_override.pop("status", None)
        return status_val, req_override

    # ── Main handler ──

    def _handle(self):
        parsed = urlparse(self.path)
        path = parsed.path
        query = parsed.query
        body = self._read_body()

        log.info(f"{self.command} {self.path}")

        # Set up hit recording for this request. `_captured_*` are filled by
        # `_json()` on the way out; non-admin paths get a record appended in
        # the finally below. Admin paths (/mock/..., /health, /) are tagged
        # so the SSE stream can filter them out by default.
        is_admin = path.startswith("/mock/") or path in ("/", "/health", "/flush-redis")
        self._hit_start = time.time()
        self._hit_request_headers = {k: v for k, v in self.headers.items()}
        self._hit_request_body = body
        self._hit_run_id = self.headers.get("X-Test-Run-Id") or None
        self._captured_status = None
        self._captured_response_body = None
        self._captured_response_headers = None
        self._hit_admin = is_admin
        try:
            return self._dispatch(parsed, path, query, body)
        finally:
            try:
                record_hit({
                    "timestamp": self._hit_start,
                    "duration_ms": int((time.time() - self._hit_start) * 1000),
                    "service": getattr(self, "_hit_service", None),
                    "method": self.command,
                    "path": path,
                    "query": query,
                    "url": self.path,
                    "request_headers": dict(self._hit_request_headers),
                    "request_body": _truncate_for_hit(self._hit_request_body),
                    "status": self._captured_status if self._captured_status is not None else 200,
                    "response_headers": self._captured_response_headers or {},
                    "response_body": self._captured_response_body,
                    "run_id": self._hit_run_id,
                    "admin": is_admin,
                })
            except Exception:
                log.exception("failed to record mock hit")

    def _dispatch(self, parsed, path, query, body):
        # ── Hit-buffer admin endpoints (must come before the /mock prefix dispatch) ──
        if path == "/mock/hits/stream":
            return self._mock_hits_stream(parsed)

        # ── Mock APIs ──
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
        if path == "/mock/sql/insert" and self.command == "POST":
            return self._mock_sql_insert(body)

        # ── Generic Redis SET / DEL (for tests that need to seed or invalidate Redis cache state) ──
        if path == "/mock/redis/set" and self.command == "POST":
            return self._mock_redis_set(body)
        if path == "/mock/redis/del" and self.command == "POST":
            return self._mock_redis_del(body)

        # ── Generic scheduler-job trigger (for scheduler tests) ──
        if path == "/mock/scheduler/trigger" and self.command == "POST":
            return self._mock_scheduler_trigger(body)
        if path == "/mock/scheduler/peek" and self.command == "POST":
            return self._mock_scheduler_peek(body)
        if path == "/mock/scheduler/clear" and self.command == "POST":
            return self._mock_scheduler_clear(body)

        # ── Refund-flow targeted reset (analogue of /mock/scheduler/clear) ──
        if path == "/mock/refunds/clear" and self.command == "POST":
            return self._mock_refunds_clear(body)

        # ── Route to service handler with middleware override check ──
        for prefix, module, svc_name in ROUTES:
            if prefix in path:
                # Middleware: check override rules and attach to handler
                self._hit_service = svc_name
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

    # ── Mock hits API (test-dashboard "Mock calls" panel) ──

    def _mock_hits_stream(self, parsed):
        """GET /mock/hits/stream?include_admin=1

        Server-Sent Events: pushes a `data: {hit-json}\\n\\n` frame for every
        hit recorded while this connection is open. No backlog replay — the
        caller opens the stream before the API call (mirroring the log-tail
        pattern) and closes it once it's done + a small grace window for
        async fan-out. Heartbeats let the server detect a closed client.
        """
        if self.command != "GET":
            return self._json({"error": "method not allowed"}, status=405)
        from urllib.parse import parse_qs
        params = parse_qs(parsed.query or "")
        include_admin = params.get("include_admin", ["0"])[0] in ("1", "true", "yes")

        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Cache-Control", "no-cache")
        self.send_header("Connection", "keep-alive")
        self.send_header("X-Accel-Buffering", "no")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()

        # Subscribe BEFORE returning headers so any hit recorded between now
        # and the client's first read is queued (not dropped).
        q: queue.Queue = queue.Queue(maxsize=1024)
        with _STREAM_LOCK:
            _STREAM_SUBSCRIBERS.append(q)
        try:
            while True:
                try:
                    hit = q.get(timeout=10)
                except queue.Empty:
                    try:
                        self.wfile.write(b": keepalive\n\n")
                        self.wfile.flush()
                        continue
                    except (BrokenPipeError, ConnectionResetError):
                        return
                if not include_admin and hit.get("admin"):
                    continue
                try:
                    self.wfile.write(b"data: ")
                    self.wfile.write(json.dumps(hit, default=str).encode("utf-8"))
                    self.wfile.write(b"\n\n")
                    self.wfile.flush()
                except (BrokenPipeError, ConnectionResetError):
                    return
        finally:
            with _STREAM_LOCK:
                try:
                    _STREAM_SUBSCRIBERS.remove(q)
                except ValueError:
                    pass

    # ── Mock override API ──

    def _mock_override(self, body):
        """
        POST /mock/override — add a request-matching override rule
          {"service": "cmrl", "extract": "body.mob", "value": "9876543210",
           "match": "/orders",      // optional: only fire when path contains this
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

            # Bodyless DELETE → clear ALL overrides (test reset); a body of {service,extract,value} removes just that one.
            if not data:
                status_store.clear_overrides()
                return self._json({"ok": True, "cleared": "all"})

            service = data.get("service")
            extract = data.get("extract")
            value = data.get("value")

            # Empty body → clear all overrides
            if not service and not extract and value is None:
                status_store.clear_overrides()
                return self._json({"ok": True, "cleared": "all"})

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
            match = data.get("match")

            if not service or not extract or value is None or not response:
                return self._json({"error": "service, extract, value, and response are required"}, status=400)

            status_store.add_override(service, extract, value, response, match=match)
            return self._json({"ok": True, "service": service, "extract": extract, "value": str(value), "match": match, "ttl": status_store.STATUS_TTL})

        return self._json({"error": "method not allowed"}, status=405)

    # ── Generic SQL query endpoints (for scheduler tests / DB fixtures) ──
    #
    # Two endpoints expose SELECT and UPDATE against any dev-DB table. Table,
    # schema, and column identifiers go through psycopg2.sql.Identifier so
    # they're safely quoted; values are bound as parameters. Whitelisted
    # operators only. These are dev-only — the mock server is not exposed
    # outside the dev stack.

    _SQL_WHERE_OPS = {"=", "!=", "<", "<=", ">", ">=", "LIKE", "IN", "IS NULL", "IS NOT NULL"}
    # `atlas_superuser` has SELECT across all schemas (atlas_app + atlas_driver_offer_bpp + …).
    # `atlas_app_user` is scoped to atlas_app only and 500s on cross-schema queries.
    # Local dev Postgres uses trust auth so password is ignored — kept optional for
    # environments that switch to md5/scram. Override any of these via env vars.
    _SQL_DB_DEFAULT = {
        "host": os.environ.get("MOCK_SQL_HOST", "localhost"),
        "port": int(os.environ.get("MOCK_SQL_PORT", "5434")),
        "user": os.environ.get("MOCK_SQL_USER", "atlas_superuser"),
        **({"password": os.environ["MOCK_SQL_PASSWORD"]} if os.environ.get("MOCK_SQL_PASSWORD") else {}),
    }

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

    def _mock_sql_insert(self, body):
        """POST /mock/sql/insert — run an INSERT into a dev DB table.
        Body: {
          "db_name": "atlas_dev",
          "db_schema": "atlas_app",
          "table_name": "purchased_pass",
          "values": {"id": "...", "status": "Active", ...},   # column → value
          "touch_timestamps": true,                            # optional, sets created_at/updated_at = NOW()
          "on_conflict_do_nothing": true                       # optional, appends ON CONFLICT DO NOTHING
        }
        Returns: {"ok": true, "rowcount": N}"""
        from psycopg2 import sql as psql
        try:
            data, db_name, schema, table = self._parse_sql_target(body)
        except ValueError as ve:
            return self._json({"error": str(ve)}, status=400)

        values = data.get("values") or {}
        if not isinstance(values, dict) or not values:
            return self._json({"error": "values must be a non-empty object"}, status=400)

        cols = list(values.keys())
        params = list(values.values())
        col_idents = psql.SQL(", ").join(psql.Identifier(c) for c in cols)
        placeholders = psql.SQL(", ").join(psql.Placeholder() * len(cols))

        if data.get("touch_timestamps"):
            col_idents = col_idents + psql.SQL(", created_at, updated_at")
            placeholders = placeholders + psql.SQL(", NOW(), NOW()")

        on_conflict = psql.SQL(" ON CONFLICT DO NOTHING") if data.get("on_conflict_do_nothing") else psql.SQL("")
        query = psql.SQL("INSERT INTO {sch}.{tbl} ({cols}) VALUES ({vals}){oc}").format(
            sch=psql.Identifier(schema),
            tbl=psql.Identifier(table),
            cols=col_idents,
            vals=placeholders,
            oc=on_conflict,
        )

        try:
            conn = self._pg_connect(db_name)
            try:
                conn.autocommit = True
                cur = conn.cursor()
                cur.execute(query, params)
                rowcount = cur.rowcount
                cur.close()
            finally:
                conn.close()
            return self._json({"ok": True, "rowcount": rowcount})
        except Exception:
            log.exception("sql-insert error")
            return self._json({"error": "internal server error"}, status=500)

    # ── Generic scheduler job trigger (for scheduler tests) ──

    # WARNING: mirrors the Haskell ToJSON (AnyJob t) instance at
    # lib/scheduler/src/Lib/Scheduler/Types.hs:115 — keep in sync.
    # Shard math mirrors createJobImpl at lib/scheduler/src/Lib/Scheduler/ScheduleJob.hs:120.
    _SCHEDULER_TARGETS = {
        "rider":  {"schedulerSetName": "Scheduled_Jobs",  "redisPort": 30001, "maxShards": 5},
        "driver": {"schedulerSetName": "Scheduled_Jobs", "redisPort": 30001, "maxShards": 5},
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

    def _mock_redis_set(self, body):
        """POST /mock/redis/set — write a key/value into Redis (for cache pre-seeding).

        Body:
          {"key": "rider-app-scheduler:NAMMA_YATRI:mobility:fcm_token",
           "value": "{\"access_token\":\"fake\",\"expires_in\":32503680000,\"token_type\":\"Bearer\"}",
           "target": "cluster",  # "cluster" (port 30001, with -c) or "standalone" (port 6379) — default "cluster"
           "port": 30001}        # optional — overrides target preset

        Used by integration tests that need to pre-populate cache state the production
        code reads via Redis.get (e.g. the FCM JWT cache, to bypass Google OAuth in dev).

        Returns: {"ok": true, "key": "...", "stored": "OK"}
        """
        import subprocess

        try:
            data = json.loads(body) if body else {}
        except json.JSONDecodeError:
            return self._json({"error": "invalid JSON"}, status=400)

        key = data.get("key")
        value = data.get("value")
        if not key or value is None:
            return self._json({"error": "key and value are required"}, status=400)

        target = data.get("target", "cluster")
        port_default = 30001 if target == "cluster" else 6379
        try:
            port = int(data.get("port", port_default))
        except (TypeError, ValueError):
            return self._json({"error": "port must be numeric"}, status=400)

        cmd = ["redis-cli"]
        if target == "cluster":
            cmd += ["-c"]
        cmd += ["-p", str(port), "SET", key, str(value)]

        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=5)
        except subprocess.TimeoutExpired:
            return self._json({"error": "redis-cli timed out"}, status=500)

        out = (result.stdout or "").strip()
        if result.returncode != 0 or out != "OK":
            return self._json({"error": "redis SET failed", "stdout": out, "stderr": (result.stderr or "").strip()}, status=500)
        return self._json({"ok": True, "key": key, "stored": out})

    def _mock_redis_del(self, body):
        """POST /mock/redis/del — delete a Redis key, or a SCAN pattern, on every cluster node.

        Cluster keys are partitioned by hash slot. To delete a specific key, send {"key": "..."}.
        To delete every key matching a pattern, send {"pattern": "..."} — this scans all six
        cluster nodes (30001-30006) and DELs every match. Use a pattern when invalidating a
        cached-query family (e.g. all MerchantPushNotification entries for one mocId).

        Body:
          {"key": "..."}                             # single-key delete
          {"pattern": "*MerchantPushNotification*"}  # SCAN+DEL across all cluster nodes
          {"target": "cluster"|"standalone", "port": ...}  # both default to cluster/30001

        Returns: {"deleted": N}
        """
        import subprocess

        try:
            data = json.loads(body) if body else {}
        except json.JSONDecodeError:
            return self._json({"error": "invalid JSON"}, status=400)

        key = data.get("key")
        pattern = data.get("pattern")
        if not key and not pattern:
            return self._json({"error": "either key or pattern is required"}, status=400)

        target = data.get("target", "cluster")
        ports_default = [30001, 30002, 30003, 30004, 30005, 30006] if target == "cluster" else [6379]
        ports = [int(p) for p in (data.get("ports") or ports_default)]

        deleted = 0
        if key:
            cmd = ["redis-cli"]
            if target == "cluster":
                cmd += ["-c", "-p", str(ports[0])]
            else:
                cmd += ["-p", str(ports[0])]
            cmd += ["DEL", key]
            try:
                result = subprocess.run(cmd, capture_output=True, text=True, timeout=5)
                if result.returncode == 0:
                    deleted += int((result.stdout or "0").strip() or 0)
            except subprocess.TimeoutExpired:
                pass
        if pattern:
            for p in ports:
                try:
                    scan = subprocess.run(["redis-cli", "-p", str(p), "--scan", "--pattern", pattern], capture_output=True, text=True, timeout=10)
                    if scan.returncode != 0:
                        continue
                    for k in (scan.stdout or "").splitlines():
                        k = k.strip()
                        if not k:
                            continue
                        d = subprocess.run(["redis-cli", "-c", "-p", "30001", "DEL", k], capture_output=True, text=True, timeout=5)
                        if d.returncode == 0:
                            deleted += int((d.stdout or "0").strip() or 0)
                except subprocess.TimeoutExpired:
                    continue
        return self._json({"deleted": deleted})

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

    def _mock_refunds_clear(self, body):
        """POST /mock/refunds/clear — reset the refund-flow rows for ONE ride so a refund test
        scenario starts clean (the refund analogue of /mock/scheduler/clear). Deletes
        refund_request / refunds / Refund-type finance_invoice / refund finance_ledger_entry on
        BOTH schemas, and flushes the refund/finance KV (the refund flow writes Redis-first, so a
        Postgres-only delete would be resurrected on the next API read). KEEPS finance_account
        (persistent, cached account ids) and the 'Ride' fare invoice (the BAP refund clones it).

        Body: {"rideId": "...", "orderId": "...", "orderShortId": "...", "bppBookingId": "...",
               "db_name": "atlas_dev" (opt), "redisPorts": [6379,30001,30002,30003] (opt)}
        Returns: {"ok": true, "deleted": {"schema.table": rowcount, ...}, "kvDeleted": N}
        """
        from psycopg2 import sql as psql
        import subprocess
        try:
            data = json.loads(body) if body else {}
        except json.JSONDecodeError:
            return self._json({"error": "invalid JSON"}, status=400)

        ride_id = data.get("rideId")
        order_id = data.get("orderId")
        order_short_id = data.get("orderShortId")
        bpp_booking_id = data.get("bppBookingId")
        db_name = data.get("db_name", "atlas_dev")
        if not (ride_id and order_id and order_short_id and bpp_booking_id):
            return self._json({"error": "rideId, orderId, orderShortId, bppBookingId are required"}, status=400)

        # ── 1. Postgres: delete refund rows on both schemas. KEEP finance_account + the 'Ride' invoice. ──
        # Each delete = (schema, table, [(col, op, val), ...]); op ∈ {"=", "IN"}, all conditions AND-ed.
        #
        # scope="refundsOnly" (opt-in) preserves the ride's ORIGINAL payment/settlement ledger legs
        # (so a test can assert they survive a refund) and the original Ride/Commission invoices,
        # deleting ONLY the refund-typed legs + Refund invoices. The default (unscoped) behaviour
        # hard-wipes ALL legs/invoices for the ride/booking.
        refunds_only = (data.get("scope") or "").strip() == "refundsOnly"
        refund_ref_types = (
            "RideFareRefund", "RideFareRefundVAT", "TollRefund", "TollRefundVAT",
            "ParkingRefund", "ParkingRefundVAT", "RideFareRefundCommission",
            "RideFareRefundCommissionVAT",
        )
        if refunds_only:
            deletes = [
                ("atlas_app", "finance_ledger_entry", [("reference_id", "=", ride_id), ("reference_type", "IN", refund_ref_types)]),
                ("atlas_app", "finance_invoice", [("reference_id", "=", ride_id), ("invoice_type", "=", "Refund")]),
                ("atlas_app", "refund_request", [("order_id", "=", order_id)]),
                ("atlas_app", "refunds", [("order_id", "=", order_short_id)]),
                ("atlas_driver_offer_bpp", "finance_ledger_entry", [("reference_id", "=", bpp_booking_id), ("reference_type", "IN", refund_ref_types)]),
                ("atlas_driver_offer_bpp", "finance_invoice", [("reference_id", "=", bpp_booking_id), ("invoice_type", "=", "Refund")]),
            ]
        else:
            deletes = [
                ("atlas_app", "finance_ledger_entry", [("reference_id", "=", ride_id)]),
                ("atlas_app", "finance_invoice", [("reference_id", "=", ride_id), ("invoice_type", "=", "Refund")]),
                ("atlas_app", "refund_request", [("order_id", "=", order_id)]),
                ("atlas_app", "refunds", [("order_id", "=", order_short_id)]),
                ("atlas_driver_offer_bpp", "finance_ledger_entry", [("reference_id", "=", bpp_booking_id)]),
                ("atlas_driver_offer_bpp", "finance_invoice", [("reference_id", "=", bpp_booking_id)]),
            ]
        deleted = {}
        try:
            conn = self._pg_connect(db_name)
            try:
                conn.autocommit = True
                cur = conn.cursor()
                for schema, table, conditions in deletes:
                    where_parts, where_vals = [], []
                    for col, op, val in conditions:
                        if op == "IN":
                            vals = tuple(val)
                            placeholders = psql.SQL(", ").join(psql.Placeholder() for _ in vals)
                            where_parts.append(psql.SQL("{c} IN ({p})").format(c=psql.Identifier(col), p=placeholders))
                            where_vals.extend(vals)
                        else:
                            where_parts.append(psql.SQL("{c} = %s").format(c=psql.Identifier(col)))
                            where_vals.append(val)
                    q = psql.SQL("DELETE FROM {s}.{t} WHERE ").format(
                        s=psql.Identifier(schema), t=psql.Identifier(table)) + psql.SQL(" AND ").join(where_parts)
                    cur.execute(q, where_vals)
                    deleted[f"{schema}.{table}"] = cur.rowcount
                cur.close()
            finally:
                conn.close()
        except Exception:
            log.exception("refunds-clear: postgres error")
            return self._json({"error": "internal server error (postgres)"}, status=500)

        # ── 2. Redis: flush stale refund/finance KV (refund flow writes Redis-first; reads are KV-first, so a
        # stale copy resurrects deleted rows AND carries the cumulative-refund total across runs). NOT finance_account*.
        # KEY NAMING (the entity/KV prefix, NOT the snake_case table name):
        #   refund_request -> refundRequest_* ; refunds -> refunds_* ;
        #   finance_invoice -> invoice_*  (NOT finance_invoice! — invoice_id_/invoice_invoiceNumber_/invoice_referenceId_) ;
        #   finance_ledger_entry -> ledgerEntry_*  (NOT finance_ledger_entry!).
        # Lowercase *invoice_* deliberately does NOT match the CamelCase FinanceInvoiceSequence
        # counter, so invoice numbers keep incrementing. NOT finance_account* (its balance is intentionally cumulative).
        patterns = ["*refundRequest*", "*refunds_*", "*invoice_*", "*ledgerEntry*", "*refundLedger*"]
        redis_ports = data.get("redisPorts") or [6379, 30001, 30002, 30003]
        kv_deleted = 0
        for port in redis_ports:
            for pattern in patterns:
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
                            kv_deleted += 1
                    except (subprocess.TimeoutExpired, OSError):
                        pass

        return self._json({"ok": True, "deleted": deleted, "kvDeleted": kv_deleted})

    # ── Helpers ──

    def _read_body(self):
        length = int(self.headers.get("Content-Length", 0))
        if length > 0:
            return self.rfile.read(length)
        return b""

    def _json(self, data, status=200, default=None):
        body = json.dumps(data, default=default).encode()
        # Capture for the hit recorder before sending.
        try:
            self._captured_status = status
            self._captured_response_body = _truncate_for_hit(body)
            self._captured_response_headers = {"Content-Type": "application/json", "Content-Length": str(len(body))}
        except Exception:
            pass
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _raw(self, status, content_type, body, extra_headers=None):
        if isinstance(body, str):
            body = body.encode()
        try:
            self._captured_status = status
            self._captured_response_body = _truncate_for_hit(body)
            self._captured_response_headers = {"Content-Type": content_type, "Content-Length": str(len(body))}
        except Exception:
            pass
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        if extra_headers:
            for k, v in extra_headers.items():
                self.send_header(k, v)
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, format, *args):
        pass


class _QuietThreadingHTTPServer(ThreadingHTTPServer):
    """ThreadingHTTPServer that silently drops client-disconnect exceptions.

    `socketserver.BaseServer.handle_error` prints a traceback for every
    exception raised out of the request handler. The dashboard's SSE stream
    (/mock/hits/stream) and Postman/test-runner clients routinely close
    connections mid-request, producing a flood of ConnectionResetError /
    BrokenPipeError tracebacks that are pure noise — the handler itself
    already catches these where they matter.
    """

    def handle_error(self, request, client_address):
        import sys as _sys
        exc = _sys.exc_info()[1]
        if isinstance(exc, (ConnectionResetError, BrokenPipeError, ConnectionAbortedError)):
            return
        super().handle_error(request, client_address)


def main():
    parser = argparse.ArgumentParser(description="Unified mock server for NammaYatri")
    parser.add_argument("--port", type=int, default=8080, help="Port (default: 8080)")
    args = parser.parse_args()

    server = _QuietThreadingHTTPServer(("0.0.0.0", args.port), MockHandler)
    log.info(f"Mock server running on :{args.port}")
    log.info("APIs: POST/GET/DELETE /mock/override, POST /mock/sql/select, POST /mock/sql/update, POST /mock/sql/insert, POST /mock/scheduler/trigger, POST /mock/scheduler/peek, POST /mock/scheduler/clear, POST /mock/refunds/clear")
    log.info(f"Services: {', '.join(r[0].strip('/') for r in ROUTES)}")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        log.info("Shutting down")
        server.server_close()


if __name__ == "__main__":
    main()
