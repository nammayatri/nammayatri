#!/usr/bin/env python3
"""
Test Context API + CORS Proxy

1. Serves test context from local DB (merchants, riders, drivers, tokens)
2. Scans integration-test collections and environments
3. Captures per-API service log deltas
4. Proxies API calls to rider-app/driver-app with CORS headers

Endpoints:
  GET  /api/context              → All test context data
  GET  /api/riders               → Available riders
  GET  /api/drivers              → Available drivers
  GET  /api/merchants            → Available merchants
  GET  /api/variants             → Vehicle variants
  GET  /api/collections          → Scan integration-test collection dirs
  GET  /api/collection/<dir>/<f> → Serve raw Postman collection JSON
  POST /api/logs/start           → Start tail -f on all service logs, returns token
  POST /api/logs/stop            → Stop tails, return captured log text
  ANY  /proxy/rider/*            → Proxy to rider-app (localhost:8013)
  ANY  /proxy/driver/*           → Proxy to driver-app (localhost:8016)
  ANY  /proxy/provider-dashboard/*  → Proxy to provider-dashboard (localhost:8018)
  POST /api/finance/trigger-recon             → Provider-dashboard merchant/scheduler/trigger (ReconciliationTrigger)
  POST /api/finance/run-recon-db-prerequisites → Enable reconciliation on transporter_config + GRANT scheduler_job + Redis cache (standalone)
  GET  /api/finance/readiness                  → Same DB + Redis prerequisites as above, then reconciliation readiness checks

Port: 7082
"""

import json
import re
import sys
import os
import glob
import subprocess
import threading
import time
import math
import uuid
from pathlib import Path
import uuid
from datetime import datetime, timedelta, timezone
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs
import urllib.request
import urllib.error


def redis_cmd(*args):
    """Run a redis-cli command against the cluster, fallback to single-node."""
    cluster_cmd = ["redis-cli", "--cluster", "call", "localhost:30001"] + list(args)
    r = subprocess.run(cluster_cmd, capture_output=True, text=True, timeout=10)
    if r.returncode == 0:
        return True, r.stdout
    # fallback single-node
    r2 = subprocess.run(["redis-cli"] + list(args), capture_output=True, text=True, timeout=10)
    return r2.returncode == 0, r2.stdout

PORT = 7082

# ── Paths ──
SCRIPT_DIR = Path(__file__).resolve().parent
COLLECTIONS_DIR = SCRIPT_DIR.parent.parent / "integration-tests" / "collections"
PROJECT_ROOT = SCRIPT_DIR.parent.parent.parent.parent  # nammayatri/

# ── Service log files for per-API capture ──
# Haskell service logs (/tmp) — EulerHS logger writes here
SERVICE_LOGS = {
    "rider-app": Path("/tmp/rider-app.log"),
    "rider-app-eul": Path("/tmp/rider-app-eul.log"),
    "driver-app": Path("/tmp/dynamic-offer-driver-app.log"),
    "driver-app-eul": Path("/tmp/dynamic-offer-driver-app-eul.log"),
    "beckn-gateway": Path("/tmp/beckn-gateway.log"),
    "search-result-aggregator": Path("/tmp/search-result-aggregator.log"),
    "producer": Path("/tmp/producer.log"),
    "rider-producer": Path("/tmp/rider-producer.log"),
    "provider-dashboard": Path("/tmp/provider-dashboard.log"),
    "provider-dashboard-eul": Path("/tmp/provider-dashboard-eul.log"),
    "rider-dashboard": Path("/tmp/rider-dashboard.log"),
    "rider-dashboard-eul": Path("/tmp/rider-dashboard-eul.log"),
}
MAX_LOG_DELTA_BYTES = 64 * 1024  # 64KB per service

RIDER_URL = os.environ.get("RIDER_URL", "http://localhost:8013")
DRIVER_URL = os.environ.get("DRIVER_URL", "http://localhost:8016")
RECON_TRIGGER_API_URL = os.environ.get("RECON_TRIGGER_API_URL", "")
RECON_TRIGGER_API_BASE_URL = os.environ.get("RECON_TRIGGER_API_BASE_URL", "http://localhost:8018")
# Fallback only: prefer atlas_bpp_dashboard.registration_token scoped to merchant + city (see resolve_dashboard_token).
RECON_TRIGGER_API_TOKEN = os.environ.get(
    "RECON_TRIGGER_API_TOKEN",
    "local-admin-token-bangalore-namma-yatri",
)

DB_CONFIG = {
    "host": os.environ.get("DB_HOST", "localhost"),
    "port": int(os.environ.get("DB_PORT", "5434")),
    "dbname": os.environ.get("DB_NAME", "atlas_dev"),
    "user": os.environ.get("DB_USER", os.environ.get("USER", "atlas")),
    "password": os.environ.get("DB_PASS", ""),
}


def get_conn():
    import psycopg2
    return psycopg2.connect(**DB_CONFIG)


def query(sql, params=()):
    try:
        conn = get_conn()
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute(sql, params)
        cols = [d[0] for d in cur.description] if cur.description else []
        rows = [dict(zip(cols, r)) for r in cur.fetchall()] if cols else []
        conn.close()
        return rows
    except Exception as e:
        return {"error": str(e)}


def execute(sql, params=()):
    try:
        conn = get_conn()
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute(sql, params)
        rowcount = cur.rowcount
        conn.close()
        return {"ok": True, "rowcount": rowcount}
    except Exception as e:
        return {"error": str(e)}


def _transporter_config_cache_keys(merchant_operating_city_id):
    """
    Redis keys used by driver-offer CachedQueries.TransporterConfig (see TransporterConfig.hs).
    Hedis prepends hedisPrefix + ':' — driver-offer-allocator uses 'driver-offer-scheduler'.
    """
    base = (
        "driver-offer:CachedQueries:TransporterConfig:MerchantOperatingCityId-"
        + merchant_operating_city_id
    )
    extra = os.environ.get("RECON_REDIS_KEY_PREFIXES", "driver-offer-scheduler").strip()
    prefixes = [p.strip() for p in extra.split(",") if p.strip()]
    keys = [base]
    for p in prefixes:
        keys.append(f"{p}:{base}" if not p.endswith(":") else f"{p}{base}")
    return list(dict.fromkeys(keys))


def _flush_transporter_config_redis_cache(merchant_operating_city_id):
    """DEL TransporterConfig cache keys so allocator/driver pick up DB changes."""
    keys = _transporter_config_cache_keys(merchant_operating_city_id)
    host = os.environ.get("REDIS_HOST", "localhost")
    port = int(os.environ.get("REDIS_PORT", "6379"))
    db = int(os.environ.get("REDIS_DB", "0"))
    password = os.environ.get("REDIS_PASSWORD") or None
    results = []

    try:
        import redis as redis_lib

        r = redis_lib.Redis(
            host=host,
            port=port,
            db=db,
            password=password,
            socket_connect_timeout=3,
            decode_responses=False,
        )
        for k in keys:
            try:
                n = r.delete(k)
                results.append({"key": k, "deleted": int(n)})
            except Exception as e:
                results.append({"key": k, "error": str(e)})
        return {"ok": True, "results": results}
    except ImportError:
        pass
    except Exception as e:
        return {"ok": False, "error": str(e), "results": results}

    # Fallback: redis-cli (no Python redis package)
    import subprocess

    for k in keys:
        try:
            cmd = ["redis-cli", "-h", host, "-p", str(port), "-n", str(db), "DEL", k]
            if password:
                cmd = ["redis-cli", "-h", host, "-p", str(port), "-n", str(db), "-a", password, "DEL", k]
            out = subprocess.run(cmd, capture_output=True, text=True, timeout=5)
            n = int(out.stdout.strip() or "0") if out.returncode == 0 else -1
            results.append({"key": k, "deleted": n, "via": "redis-cli"})
        except Exception as e:
            results.append({"key": k, "error": str(e), "via": "redis-cli"})
    return {"ok": True, "results": results, "note": "used redis-cli fallback"}


def run_recon_db_prerequisites(merchant_operating_city_id):
    """
    For the selected merchant operating city: enable reconciliation_jobs on transporter_config,
    GRANT DML on scheduler_job to the allocator DB role (default kaal_chakra_user),
    and flush TransporterConfig Redis cache for that city.
    Role name can be overridden with env RECON_DB_GRANT_ROLE.
    Redis: REDIS_HOST, REDIS_PORT, REDIS_DB, REDIS_PASSWORD; optional RECON_REDIS_KEY_PREFIXES
    (comma-separated hedis prefixes, default 'driver-offer-scheduler').
    """
    from psycopg2 import sql as psql

    if not merchant_operating_city_id:
        return {"error": "merchantOperatingCityId required"}
    grant_role = os.environ.get("RECON_DB_GRANT_ROLE", "kaal_chakra_user")
    if not re.match(r"^[a-zA-Z0-9_]+$", grant_role):
        return {"error": "Invalid RECON_DB_GRANT_ROLE (letters, digits, underscore only)"}

    try:
        conn = get_conn()
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute(
            """
            UPDATE atlas_driver_offer_bpp.transporter_config
            SET reconciliation_jobs_enabled = true,
                reconciliation_scheduler_time = 10800
            WHERE merchant_operating_city_id = %s
            """,
            (merchant_operating_city_id,),
        )
        rowcount = cur.rowcount
        grant_result = {"role": grant_role, "ok": True}
        try:
            cur.execute(
                psql.SQL(
                    "GRANT SELECT, INSERT, UPDATE, DELETE ON atlas_driver_offer_bpp.scheduler_job TO {}"
                ).format(psql.Identifier(grant_role))
            )
        except Exception as ge:
            grant_result = {"role": grant_role, "ok": False, "error": str(ge)}
        conn.close()

        cache_flush = _flush_transporter_config_redis_cache(merchant_operating_city_id)

        return {
            "ok": True,
            "transporterConfigRowsUpdated": rowcount,
            "merchantOperatingCityId": merchant_operating_city_id,
            "schedulerJobGrant": grant_result,
            "transporterConfigCacheFlush": cache_flush,
        }
    except Exception as e:
        return {"error": str(e)}


def query_one(sql, params=()):
    rows = query(sql, params)
    if isinstance(rows, dict) and rows.get("error"):
        return rows
    return rows[0] if rows else None


def parse_iso8601(value):
    if not value:
        return None
    if value.endswith("Z"):
        value = value[:-1] + "+00:00"
    dt = datetime.fromisoformat(value)
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


def iso_date_bounds(day_str):
    start = parse_iso8601(f"{day_str}T00:00:00Z")
    end = parse_iso8601(f"{day_str}T23:59:59Z")
    return start, end


def get_param(params, key, default=None):
    values = params.get(key)
    return values[0] if values else default


def send_query_result(handler, result):
    if isinstance(result, dict) and result.get("error"):
        handler._send_json(result, 500)
    else:
        handler._send_json(result)


def get_ledger_entries(filters):
    clauses = []
    params = []
    if filters.get("bookingId"):
        clauses.append("le.reference_id = %s")
        params.append(filters["bookingId"])
    if filters.get("referenceId"):
        clauses.append("le.reference_id = %s")
        params.append(filters["referenceId"])
    if filters.get("referenceType"):
        clauses.append("le.reference_type = %s")
        params.append(filters["referenceType"])
    if filters.get("merchantId"):
        clauses.append("le.merchant_id = %s")
        params.append(filters["merchantId"])
    if filters.get("merchantOperatingCityId"):
        clauses.append("le.merchant_operating_city_id = %s")
        params.append(filters["merchantOperatingCityId"])
    if filters.get("fromTime"):
        clauses.append("le.timestamp >= %s")
        params.append(filters["fromTime"])
    if filters.get("toTime"):
        clauses.append("le.timestamp <= %s")
        params.append(filters["toTime"])

    where_sql = f"WHERE {' AND '.join(clauses)}" if clauses else ""
    return query(
        f"""
        SELECT
          le.id,
          le.timestamp,
          le.from_account_id,
          le.to_account_id,
          le.amount,
          le.currency,
          le.entry_type,
          le.status,
          le.reference_type,
          le.reference_id,
          le.settlement_status,
          le.settlement_id,
          le.settlement_timestamp,
          le.merchant_id,
          le.merchant_operating_city_id,
          le.created_at,
          le.reconciliation_status
        FROM atlas_driver_offer_bpp.finance_ledger_entry le
        {where_sql}
        ORDER BY le.timestamp DESC
        LIMIT 200
        """,
        tuple(params),
    )


def get_recon_summaries(filters):
    clauses = []
    params = []
    if filters.get("fromTime"):
        clauses.append("rs.reconciliation_date >= %s")
        params.append(filters["fromTime"])
    if filters.get("toTime"):
        clauses.append("rs.reconciliation_date <= %s")
        params.append(filters["toTime"])
    if filters.get("reconciliationType"):
        clauses.append("rs.reconciliation_type = %s")
        params.append(filters["reconciliationType"])
    if filters.get("merchantId"):
        clauses.append("rs.merchant_id = %s")
        params.append(filters["merchantId"])
    if filters.get("merchantOperatingCityId"):
        clauses.append("rs.merchant_operating_city_id = %s")
        params.append(filters["merchantOperatingCityId"])

    where_sql = f"WHERE {' AND '.join(clauses)}" if clauses else ""
    return query(
        f"""
        SELECT
          rs.id,
          rs.reconciliation_date,
          rs.reconciliation_type,
          rs.total_discrepancies,
          rs.matched_records,
          rs.match_rate,
          rs.source_total,
          rs.target_total,
          rs.variance_amount,
          rs.status,
          rs.error_message,
          rs.merchant_id,
          rs.merchant_operating_city_id,
          rs.created_at,
          rs.updated_at
        FROM atlas_driver_offer_bpp.finance_reconciliation_summary rs
        {where_sql}
        ORDER BY rs.created_at DESC
        LIMIT 100
        """,
        tuple(params),
    )


def get_recon_entries(filters):
    clauses = []
    params = []
    if filters.get("summaryId"):
        clauses.append("re.summary_id = %s")
        params.append(filters["summaryId"])
    if filters.get("bookingId"):
        clauses.append("re.booking_id = %s")
        params.append(filters["bookingId"])
    if filters.get("reconciliationType"):
        clauses.append("re.reconciliation_type = %s")
        params.append(filters["reconciliationType"])
    if filters.get("merchantId"):
        clauses.append("re.merchant_id = %s")
        params.append(filters["merchantId"])
    if filters.get("merchantOperatingCityId"):
        clauses.append("re.merchant_operating_city_id = %s")
        params.append(filters["merchantOperatingCityId"])

    where_sql = f"WHERE {' AND '.join(clauses)}" if clauses else ""
    return query(
        f"""
        SELECT
          re.id,
          re.summary_id,
          re.reconciliation_date,
          re.reconciliation_type,
          re.booking_id,
          re.dco_id,
          re.status,
          re.mode,
          re.expected_dsr_value,
          re.actual_ledger_value,
          re.variance,
          re.recon_status,
          re.mismatch_reason,
          re.timestamp,
          re.finance_component,
          re.source_id,
          re.target_id,
          re.settlement_id,
          re.settlement_date,
          re.transaction_date,
          re.rrn,
          re.settlement_mode,
          re.source_details,
          re.target_details,
          re.merchant_operating_city_id,
          re.merchant_id,
          re.created_at,
          re.updated_at
        FROM atlas_driver_offer_bpp.finance_reconciliation_entry re
        {where_sql}
        ORDER BY re.created_at DESC
        LIMIT 200
        """,
        tuple(params),
    )


def get_invoices(filters):
    clauses = []
    params = []
    if filters.get("referenceId"):
        clauses.append("le.reference_id = %s")
        params.append(filters["referenceId"])
    if filters.get("invoiceId"):
        clauses.append("i.id = %s")
        params.append(filters["invoiceId"])
    where_sql = f"WHERE {' AND '.join(clauses)}" if clauses else ""
    return query(
        f"""
        SELECT DISTINCT
          i.id,
          i.invoice_number,
          i.invoice_type,
          i.payment_order_id,
          i.issued_to_type,
          i.issued_to_id,
          i.total_amount,
          i.currency,
          i.status,
          i.issued_at,
          i.merchant_id,
          i.merchant_operating_city_id,
          i.created_at
        FROM atlas_driver_offer_bpp.finance_invoice i
        LEFT JOIN atlas_driver_offer_bpp.finance_invoice_ledger_link ill
          ON ill.invoice_id = i.id
        LEFT JOIN atlas_driver_offer_bpp.finance_ledger_entry le
          ON le.id = ill.ledger_entry_id
        {where_sql}
        ORDER BY i.created_at DESC
        LIMIT 100
        """,
        tuple(params),
    )


def get_subscription_purchases(filters):
    clauses = []
    params = []
    if filters.get("ownerId"):
        clauses.append("sp.owner_id = %s")
        params.append(filters["ownerId"])
    if filters.get("status"):
        clauses.append("sp.status = %s")
        params.append(filters["status"])
    if filters.get("merchantId"):
        clauses.append("sp.merchant_id = %s")
        params.append(filters["merchantId"])
    if filters.get("merchantOperatingCityId"):
        clauses.append("sp.merchant_operating_city_id = %s")
        params.append(filters["merchantOperatingCityId"])
    if filters.get("fromTime"):
        clauses.append("sp.purchase_timestamp >= %s")
        params.append(filters["fromTime"])
    if filters.get("toTime"):
        clauses.append("sp.purchase_timestamp <= %s")
        params.append(filters["toTime"])

    where_sql = f"WHERE {' AND '.join(clauses)}" if clauses else ""
    return query(
        f"""
        SELECT
          sp.id,
          sp.owner_id,
          sp.owner_type,
          sp.plan_id,
          sp.payment_order_id,
          sp.plan_fee,
          sp.plan_ride_credit,
          sp.purchase_timestamp,
          sp.start_date,
          sp.expiry_date,
          sp.status,
          sp.service_name,
          sp.finance_invoice_id,
          sp.reconciliation_status,
          sp.merchant_id,
          sp.merchant_operating_city_id,
          sp.created_at,
          sp.updated_at
        FROM atlas_driver_offer_bpp.subscription_purchase sp
        {where_sql}
        ORDER BY sp.created_at DESC
        LIMIT 100
        """,
        tuple(params),
    )


def get_job_status(job_id=None, recon_type=None, merchant_id=None, merchant_operating_city_id=None, start_time=None, end_time=None):
    if job_id:
        return query_one(
            """
            SELECT
              id, job_type, job_data, shard_id, scheduled_at, created_at, updated_at,
              max_errors, curr_errors, status, parent_job_id, merchant_id, merchant_operating_city_id
            FROM atlas_driver_offer_bpp.scheduler_job
            WHERE id = %s
            """,
            (job_id,),
        )

    clauses = ["job_type = 'Reconciliation'"]
    params = []
    if merchant_id:
        clauses.append("merchant_id = %s")
        params.append(merchant_id)
    if merchant_operating_city_id:
        clauses.append("merchant_operating_city_id = %s")
        params.append(merchant_operating_city_id)
    if recon_type:
        clauses.append("job_data::jsonb ->> 'reconciliationType' = %s")
        params.append(recon_type)
    if start_time:
        clauses.append("job_data::jsonb ->> 'startTime' = %s")
        params.append(start_time.isoformat().replace("+00:00", "Z"))
    if end_time:
        clauses.append("job_data::jsonb ->> 'endTime' = %s")
        params.append(end_time.isoformat().replace("+00:00", "Z"))

    return query_one(
        f"""
        SELECT
          id, job_type, job_data, shard_id, scheduled_at, created_at, updated_at,
          max_errors, curr_errors, status, parent_job_id, merchant_id, merchant_operating_city_id
        FROM atlas_driver_offer_bpp.scheduler_job
        WHERE {' AND '.join(clauses)}
        ORDER BY created_at DESC
        LIMIT 1
        """,
        tuple(params),
    )


def build_readiness(recon_type, merchant_id, merchant_operating_city_id, start_time, end_time):
    base = {
        "reconciliationType": recon_type,
        "merchantId": merchant_id,
        "merchantOperatingCityId": merchant_operating_city_id,
        "startTime": start_time.isoformat().replace("+00:00", "Z") if start_time else None,
        "endTime": end_time.isoformat().replace("+00:00", "Z") if end_time else None,
        "status": "BLOCKED",
        "checks": [],
    }

    def add_check(name, count, minimum=1, meta=None):
        ok = count >= minimum
        check = {"name": name, "count": count, "ok": ok}
        if meta is not None:
            check["meta"] = meta
        base["checks"].append(check)
        return ok

    if recon_type == "DSR_VS_LEDGER":
        completed = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.booking
            WHERE merchant_operating_city_id = %s
              AND status = 'COMPLETED'
              AND updated_at >= %s
              AND updated_at <= %s
            """,
            (merchant_operating_city_id, start_time, end_time),
        )
        cancelled = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.booking
            WHERE merchant_operating_city_id = %s
              AND status = 'CANCELLED'
              AND updated_at >= %s
              AND updated_at <= %s
            """,
            (merchant_operating_city_id, start_time, end_time),
        )
        ledger = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.finance_ledger_entry
            WHERE merchant_operating_city_id = %s
              AND timestamp >= %s
              AND timestamp <= %s
            """,
            (merchant_operating_city_id, start_time, end_time),
        )
        all_ok = (
            add_check("completedBookings", int((completed or {}).get("count", 0)))
            and add_check("cancelledBookings", int((cancelled or {}).get("count", 0)))
            and add_check("ledgerEntries", int((ledger or {}).get("count", 0)))
        )
    elif recon_type == "DSSR_VS_SUBSCRIPTION":
        subs = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.subscription_purchase
            WHERE merchant_operating_city_id = %s
              AND status = 'ACTIVE'
              AND purchase_timestamp >= %s
              AND purchase_timestamp <= %s
            """,
            (merchant_operating_city_id, start_time, end_time),
        )
        credits = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.finance_ledger_entry
            WHERE merchant_operating_city_id = %s
              AND reference_type = 'SubscriptionPurchase'
              AND timestamp >= %s
              AND timestamp <= %s
            """,
            (merchant_operating_city_id, start_time, end_time),
        )
        all_ok = (
            add_check("activeSubscriptions", int((subs or {}).get("count", 0)))
            and add_check("subscriptionPurchaseLedgerEntries", int((credits or {}).get("count", 0)))
        )
    elif recon_type == "DSR_VS_SUBSCRIPTION":
        subs = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.subscription_purchase
            WHERE merchant_operating_city_id = %s
              AND status IN ('ACTIVE', 'EXPIRED', 'EXHAUSTED')
              AND purchase_timestamp <= %s
            """,
            (merchant_operating_city_id, end_time),
        )
        rides = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.booking
            WHERE merchant_operating_city_id = %s
              AND status = 'COMPLETED'
              AND updated_at >= %s
              AND updated_at <= %s
            """,
            (merchant_operating_city_id, start_time, end_time),
        )
        all_ok = (
            add_check("subscriptionsAvailable", int((subs or {}).get("count", 0)))
            and add_check("completedBookings", int((rides or {}).get("count", 0)))
        )
    elif recon_type == "PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION":
        settlements = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.pg_payment_settlement_report
            WHERE merchant_operating_city_id = %s
              AND txn_status = 'SUCCESS'
              AND txn_date >= %s
              AND txn_date <= %s
            """,
            (merchant_operating_city_id, start_time, end_time),
        )
        subscriptions = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.subscription_purchase sp
            WHERE sp.merchant_operating_city_id = %s
              AND sp.id IN (
                SELECT reference_id
                FROM atlas_driver_offer_bpp.pg_payment_settlement_report
                WHERE reference_id IS NOT NULL
                  AND merchant_operating_city_id = %s
              )
            """,
            (merchant_operating_city_id, merchant_operating_city_id),
        )
        all_ok = (
            add_check("paymentSettlementRows", int((settlements or {}).get("count", 0)))
            and add_check("matchingSubscriptions", int((subscriptions or {}).get("count", 0)))
        )
    elif recon_type == "PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST":
        settlements = query_one(
            """
            SELECT COUNT(*) AS count
            FROM atlas_driver_offer_bpp.pg_payout_settlement_report
            WHERE merchant_operating_city_id = %s
              AND txn_status = 'SUCCESS'
              AND txn_date >= %s
              AND txn_date <= %s
            """,
            (merchant_operating_city_id, start_time, end_time),
        )
        payout_requests = query_one(
            """
            SELECT COUNT(*) AS count
            FROM payout_request
            WHERE merchant_operating_city_id = %s
              AND status = 'CREDITED'
            """,
            (merchant_operating_city_id,),
        )
        all_ok = (
            add_check("payoutSettlementRows", int((settlements or {}).get("count", 0)))
            and add_check("creditedPayoutRequests", int((payout_requests or {}).get("count", 0)))
        )
    else:
        base["error"] = f"Unknown reconciliation type: {recon_type}"
        return base

    ok_count = sum(1 for check in base["checks"] if check["ok"])
    if all_ok:
        base["status"] = "READY"
    elif ok_count > 0:
        base["status"] = "PARTIALLY_READY"
    return base


def seed_payment_settlement(subscription_purchase_id, overrides=None):
    overrides = overrides or {}
    subscription = query_one(
        """
        SELECT
          id, payment_order_id, plan_fee, purchase_timestamp, status,
          merchant_id, merchant_operating_city_id
        FROM atlas_driver_offer_bpp.subscription_purchase
        WHERE id = %s
        """,
        (subscription_purchase_id,),
    )
    if not subscription:
        return {"error": f"subscription_purchase not found: {subscription_purchase_id}"}
    if isinstance(subscription, dict) and subscription.get("error"):
        return subscription

    purchase_ts = subscription["purchase_timestamp"]
    if purchase_ts.tzinfo is None:
        purchase_ts = purchase_ts.replace(tzinfo=timezone.utc)
    txn_date = overrides.get("txnDate")
    if txn_date:
        txn_date = parse_iso8601(txn_date)
    else:
        txn_date = purchase_ts
    settlement_date = txn_date + timedelta(hours=20)

    settlement_id = overrides.get("settlementId") or f"SETTLE-{txn_date.strftime('%Y-%m-%d')}-{uuid.uuid4().hex[:3].upper()}"
    row_id = overrides.get("id") or f"psr-{uuid.uuid4().hex[:6]}"
    order_id = overrides.get("orderId") or str(subscription["payment_order_id"])
    payment_gateway = overrides.get("paymentGateway") or "BILLDESK"
    payment_method = overrides.get("paymentMethod") or "UPI"
    rrn = overrides.get("rrn") or f"RRN{uuid.uuid4().int % 1000000000:09d}"
    utr = overrides.get("utr") or f"UTR{uuid.uuid4().int % 1000000000:09d}"
    approval_code = overrides.get("pgApprovalCode") or f"APR{uuid.uuid4().hex[:3].upper()}"
    bank_id = overrides.get("bankId") or "ICIC0001234"
    amount = overrides.get("txnAmount", subscription["plan_fee"])
    created_at = datetime.now(timezone.utc)

    result = execute(
        """
        INSERT INTO atlas_driver_offer_bpp.pg_payment_settlement_report (
          id, order_id, txn_id, rrn, utr, bank_id, txn_type, txn_status, txn_date,
          txn_amount, pg_base_fee, pg_tax, settlement_amount, currency,
          payment_gateway, pg_approval_code, payment_method, payment_method_sub_type,
          settlement_type, settlement_mode, settlement_id, settlement_date,
          reference_id, reference_type, recon_status, merchant_id,
          merchant_operating_city_id, created_at, updated_at
        ) VALUES (
          %s, %s, %s, %s, %s, %s, 'ORDER', 'SUCCESS', %s,
          %s, %s, %s, %s, 'INR',
          %s, %s, %s, %s,
          'CREDIT', 'NET', %s, %s,
          %s, 'SUBSCRIPTION_PURCHASE', 'PENDING', %s,
          %s, %s, %s
        )
        """,
        (
            row_id,
            order_id,
            overrides.get("txnId") or f"nammayatri-{order_id}-1",
            rrn,
            utr,
            bank_id,
            txn_date,
            amount,
            overrides.get("pgBaseFee", 0),
            overrides.get("pgTax", 0),
            overrides.get("settlementAmount", amount),
            payment_gateway,
            approval_code,
            payment_method,
            overrides.get("paymentMethodSubType") or payment_method,
            settlement_id,
            settlement_date,
            subscription_purchase_id,
            subscription["merchant_id"],
            subscription["merchant_operating_city_id"],
            created_at,
            created_at,
        ),
    )
    if result.get("error"):
        return result
    return {
        "ok": True,
        "seededRow": {
            "id": row_id,
            "subscriptionPurchaseId": subscription_purchase_id,
            "orderId": order_id,
            "txnDate": txn_date,
            "settlementId": settlement_id,
            "merchantId": subscription["merchant_id"],
            "merchantOperatingCityId": subscription["merchant_operating_city_id"],
        },
    }


def resolve_dashboard_recon_trigger_url(merchant_id, merchant_operating_city_id):
    route = query_one(
        """
        SELECT
          m.short_id,
          moc.city
        FROM atlas_driver_offer_bpp.merchant m
        JOIN atlas_driver_offer_bpp.merchant_operating_city moc
          ON moc.merchant_id = m.id
        WHERE m.id = %s
          AND moc.id = %s
        """,
        (merchant_id, merchant_operating_city_id),
    )
    if not route:
        return None, {
            "error": (
                "could_not_resolve_dashboard_route "
                f"for merchant_id={merchant_id}, merchant_operating_city_id={merchant_operating_city_id}"
            )
        }
    if isinstance(route, dict) and route.get("error"):
        return None, route

    short_id = route["short_id"]
    city = route["city"]
    base_url = RECON_TRIGGER_API_BASE_URL.rstrip("/")
    return f"{base_url}/bpp/driver-offer/{short_id}/{city}/merchant/scheduler/trigger", None


def bpp_dashboard_merchant_id_for_driver_merchant(driver_merchant_id):
    """
    atlas_bpp_dashboard.registration_token.merchant_id references atlas_bpp_dashboard.merchant.id,
    not atlas_driver_offer_bpp.merchant.id. Resolve via matching short_id.
    """
    row = query_one(
        """
        SELECT dm.id::text AS id
        FROM atlas_driver_offer_bpp.merchant d
        INNER JOIN atlas_bpp_dashboard.merchant dm ON dm.short_id = d.short_id
        WHERE d.id = %s
        LIMIT 1
        """,
        (driver_merchant_id,),
    )
    if isinstance(row, dict) and row.get("error"):
        return None
    if row and row.get("id"):
        return row["id"]
    return None


def resolve_dashboard_token(merchant_id=None, city=None):
    """
    Pick a provider-dashboard token that can call scheduler/trigger for this merchant + city.
    merchant_id should be atlas_bpp_dashboard.merchant.id (use bpp_dashboard_merchant_id_for_driver_merchant).
    Prefer registration_token rows scoped to merchant_id / operating_city; fall back to
    RECON_TRIGGER_API_TOKEN (default is Bangalore-only — wrong merchant → 403 ACCESS_DENIED).
    """
    scoped = query_one(
        """
        SELECT rt.token
        FROM atlas_bpp_dashboard.registration_token rt
        JOIN atlas_bpp_dashboard.person p ON p.id = rt.person_id
        JOIN atlas_bpp_dashboard.role r ON r.id = p.role_id
        WHERE r.name = 'JUSPAY_ADMIN'
          AND COALESCE(rt.enabled, TRUE) = TRUE
          AND (%s IS NULL OR rt.merchant_id = %s OR rt.merchant_id IS NULL)
          AND (%s IS NULL OR rt.operating_city = %s OR rt.operating_city IS NULL)
        ORDER BY
          CASE
            WHEN rt.merchant_id = %s AND rt.operating_city = %s THEN 0
            WHEN rt.merchant_id = %s THEN 1
            WHEN rt.operating_city = %s THEN 2
            ELSE 3
          END,
          rt.created_at DESC
        LIMIT 1
        """,
        (merchant_id, merchant_id, city, city, merchant_id, city, merchant_id, city),
    )
    if scoped and not scoped.get("error") and scoped.get("token"):
        return scoped["token"]
    if RECON_TRIGGER_API_TOKEN:
        return RECON_TRIGGER_API_TOKEN
    return None


def trigger_reconciliation_via_api(body, incoming_headers):
    target_url = RECON_TRIGGER_API_URL.strip()
    route_meta = None
    if not target_url:
        target_url, route_error = resolve_dashboard_recon_trigger_url(
            body["merchantId"], body["merchantOperatingCityId"]
        )
        if route_error:
            return route_error
        route_meta = query_one(
            """
            SELECT
              m.short_id,
              moc.city
            FROM atlas_driver_offer_bpp.merchant m
            JOIN atlas_driver_offer_bpp.merchant_operating_city moc
              ON moc.merchant_id = m.id
            WHERE m.id = %s
              AND moc.id = %s
            """,
            (body["merchantId"], body["merchantOperatingCityId"]),
        )

    forward_headers = {"Content-Type": "application/json"}
    for key in incoming_headers:
        lower = key.lower()
        if lower not in ("host", "origin", "referer", "content-length", "token"):
            forward_headers[key] = incoming_headers[key]
    dash_merchant_id = bpp_dashboard_merchant_id_for_driver_merchant(body["merchantId"])
    resolved_token = resolve_dashboard_token(
        dash_merchant_id or body["merchantId"],
        None if not route_meta or route_meta.get("error") else route_meta["city"],
    )
    if resolved_token:
        forward_headers["token"] = resolved_token
    else:
        return {"error": "dashboard_api_token_not_found", "url": target_url}

    scheduled_at = body.get("scheduledAt") or (
        datetime.now(timezone.utc) + timedelta(minutes=2)
    ).isoformat().replace("+00:00", "Z")
    payload = json.dumps(
        {
            "scheduledAt": scheduled_at,
            "jobName": "ReconciliationTrigger",
            "jobData": json.dumps(
                {
                    "merchantId": body["merchantId"],
                    "merchantOperatingCityId": body["merchantOperatingCityId"],
                    "startTime": body["startTime"],
                    "endTime": body["endTime"],
                    "reconciliationType": body["reconciliationType"],
                }
            ),
        }
    ).encode()

    try:
        req = urllib.request.Request(target_url, data=payload, headers=forward_headers, method="POST")
        with urllib.request.urlopen(req, timeout=30) as resp:
            data = json.loads(resp.read().decode() or "{}")
            return {
                "ok": True,
                "mode": "scheduler_api",
                "url": target_url,
                "statusCode": resp.status,
                "response": data,
            }
    except urllib.error.HTTPError as e:
        try:
            body = json.loads(e.read().decode() or "{}")
        except Exception:
            body = {"error": "management api http error"}
        return {
            "error": "management_api_http_error",
            "url": target_url,
            "statusCode": e.code,
            "response": body,
        }
    except Exception as e:
        return {"error": f"management_api_error: {str(e)}", "url": target_url}


def get_merchants():
    riders = query("""
        SELECT m.id, m.short_id, m.name, m.online_payment,
               moc.id as city_id, moc.city, moc.country, moc.state
        FROM atlas_app.merchant m
        LEFT JOIN atlas_app.merchant_operating_city moc ON moc.merchant_id = m.id
        ORDER BY m.short_id, moc.city
    """)
    drivers = query("""
        SELECT m.id, m.short_id, m.name,
               moc.id as city_id, moc.city, moc.country, moc.currency
        FROM atlas_driver_offer_bpp.merchant m
        LEFT JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.merchant_id = m.id
        ORDER BY m.short_id, moc.city
    """)
    return {"rider_merchants": riders, "driver_merchants": drivers}


def get_riders():
    return query("""
        SELECT p.id as person_id, p.first_name, p.role,
               m.short_id as merchant, moc.city,
               rt.token, rt.verified
        FROM atlas_app.person p
        JOIN atlas_app.merchant m ON m.id = p.merchant_id
        LEFT JOIN atlas_app.merchant_operating_city moc ON moc.id = p.merchant_operating_city_id
        LEFT JOIN atlas_app.registration_token rt ON rt.entity_id = p.id AND rt.verified = true
        WHERE p.role = 'USER' AND rt.token IS NOT NULL
        ORDER BY m.short_id, p.first_name
    """)


def get_drivers():
    return query("""
        SELECT p.id as person_id, p.first_name, p.role,
               m.short_id as merchant, m.id as merchant_id, moc.id as city_id, moc.city, moc.currency,
               rt.token, rt.verified, v.variant as vehicle_variant
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.merchant m ON m.id = p.merchant_id
        LEFT JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = p.merchant_operating_city_id
        LEFT JOIN atlas_driver_offer_bpp.registration_token rt ON rt.entity_id = p.id AND rt.verified = true
        LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
        WHERE p.role = 'DRIVER' AND rt.token IS NOT NULL
        ORDER BY m.short_id, p.first_name
    """)


def get_variants(city_id=None):
    sql = """
        SELECT vst.id, vst.service_tier_type, vst.name, vst.seating_capacity,
               vst.is_air_conditioned, vst.is_enabled, vst.priority,
               vst.allowed_vehicle_variant,
               m.short_id as merchant, moc.city, moc.currency
        FROM atlas_driver_offer_bpp.vehicle_service_tier vst
        JOIN atlas_driver_offer_bpp.merchant m ON m.id = vst.merchant_id
        JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = vst.merchant_operating_city_id
        WHERE vst.is_enabled = true
    """
    params = ()
    if city_id:
        sql += " AND vst.merchant_operating_city_id = %s"
        params = (city_id,)
    sql += " ORDER BY m.short_id, vst.priority"
    return query(sql, params)


def get_admin_credentials():
    """Return known admin credentials per merchant for the test dashboard.
    These are created in provider-dashboard seed migrations with known email/password."""
    return {
        "MSIL_PARTNER": {"email": "admin@msil.test", "password": "msil1234"},
        "LYNX_PARTNER": {"email": "admin@lynx.test", "password": "lynx1234"},
    }


# ── Collection Scanner ──

def scan_collections():
    """Walk integration-tests/collections/ and return metadata for each collection group."""
    result = []
    if not COLLECTIONS_DIR.is_dir():
        return result
    for subdir in sorted(COLLECTIONS_DIR.iterdir()):
        if not subdir.is_dir():
            continue
        group = {"directory": subdir.name, "environments": [], "suites": []}
        for f in sorted(subdir.iterdir()):
            if not f.suffix == ".json":
                continue
            if f.name.startswith("Local_") and f.name.endswith(".postman_environment.json"):
                try:
                    env_data = json.loads(f.read_text())
                    vals = {v["key"]: v["value"] for v in env_data.get("values", []) if v.get("enabled", True)}
                    env_name = f.name.replace("Local_", "").replace(".postman_environment.json", "")
                    group["environments"].append({
                        "filename": f.name,
                        "envName": env_name,
                        "name": env_data.get("name", env_name),
                        "city": vals.get("city", ""),
                        "state": vals.get("state", ""),
                        "merchant": vals.get("dashboard_merchant_id", ""),
                        "bapShortId": vals.get("bap_short_id", ""),
                        "origin": {"lat": float(vals.get("origin_lat", 0)), "lon": float(vals.get("origin_lon", 0))},
                        "destination": {"lat": float(vals.get("dest_lat", 0)), "lon": float(vals.get("dest_lon", 0))},
                        "variables": vals,
                    })
                except Exception:
                    pass
            elif not f.name.startswith("Local_"):
                try:
                    col_data = json.loads(f.read_text())
                    info = col_data.get("info", {})
                    group["suites"].append({
                        "filename": f.name,
                        "name": info.get("name", f.stem),
                        "description": info.get("description", ""),
                        "itemCount": len(col_data.get("item", [])),
                    })
                except Exception:
                    pass
        if group["environments"] or group["suites"]:
            result.append(group)
    return result


def get_collection_file(directory, filename):
    """Return raw Postman collection JSON."""
    path = COLLECTIONS_DIR / directory / filename
    if path.is_file() and path.suffix == ".json":
        return json.loads(path.read_text())
    return None


# ── Service Log Capture (tail -f based) ──

# Global state: active tail processes keyed by a session token
_tail_sessions = {}  # token -> { svc: { proc, lines } }
_tail_lock = threading.Lock()


def _reader_thread(lines_list, proc):
    """Background thread to read lines from tail -f stdout."""
    try:
        for line in proc.stdout:
            lines_list.append(line)
    except (ValueError, OSError):
        pass  # proc closed


def start_log_tails():
    """Start tail -f for each service log. Returns a session token."""
    import uuid
    token = str(uuid.uuid4())[:8]
    session = {}
    for svc, path in SERVICE_LOGS.items():
        if not path.exists():
            continue
        try:
            proc = subprocess.Popen(
                ["tail", "-n", "0", "-f", str(path)],
                stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                text=True, bufsize=1
            )
            lines = []
            t = threading.Thread(target=_reader_thread, args=(lines, proc), daemon=True)
            t.start()
            session[svc] = {"proc": proc, "lines": lines}
        except OSError:
            pass
    with _tail_lock:
        _tail_sessions[token] = session
    return token


def stop_log_tails(token, settle_ms=300, max_wait_ms=1000):
    """Wait for logs to settle (no new lines for settle_ms), then stop tails and return captured logs.
    Max total wait is max_wait_ms."""
    with _tail_lock:
        session = _tail_sessions.get(token, {})
    if not session:
        with _tail_lock:
            _tail_sessions.pop(token, None)
        return {}

    # Wait for logs to settle: poll until no new lines appear for settle_ms
    import time
    deadline = time.monotonic() + max_wait_ms / 1000.0
    settle_deadline = time.monotonic() + settle_ms / 1000.0
    prev_total = sum(len(e["lines"]) for e in session.values())

    while time.monotonic() < deadline:
        time.sleep(0.2)
        curr_total = sum(len(e["lines"]) for e in session.values())
        if curr_total != prev_total:
            # New lines appeared — reset settle timer
            prev_total = curr_total
            settle_deadline = time.monotonic() + settle_ms / 1000.0
        elif time.monotonic() >= settle_deadline:
            # No new lines for settle_ms — logs have settled
            break

    # Now stop and collect
    with _tail_lock:
        _tail_sessions.pop(token, None)
    logs = {}
    for svc, entry in session.items():
        proc = entry["proc"]
        proc.terminate()
        try:
            proc.wait(timeout=1)
        except subprocess.TimeoutExpired:
            proc.kill()
        text = "".join(entry["lines"]).strip()
        if text:
            if len(text) > MAX_LOG_DELTA_BYTES:
                text = text[-MAX_LOG_DELTA_BYTES:]
            logs[svc] = text
    return logs


def get_recon_harness_defaults():
    """
    Reconciliation test harness uses NAMMA_YATRI_PARTNER + Kochi by default — matches typical
    atlas_bpp_dashboard.registration_token seeds (JUSPAY_ADMIN). Override with env:
    RECON_HARNESS_MERCHANT_SHORT_ID, RECON_HARNESS_CITY
    """
    short = os.environ.get("RECON_HARNESS_MERCHANT_SHORT_ID", "NAMMA_YATRI_PARTNER")
    city = os.environ.get("RECON_HARNESS_CITY", "Kochi")
    row = query_one(
        """
        SELECT
          m.id::text AS merchant_id,
          moc.id::text AS merchant_operating_city_id,
          m.short_id AS merchant_short_id,
          moc.city
        FROM atlas_driver_offer_bpp.merchant m
        JOIN atlas_driver_offer_bpp.merchant_operating_city moc
          ON moc.merchant_id = m.id
        WHERE m.short_id = %s AND moc.city = %s
        LIMIT 1
        """,
        (short, city),
    )
    if isinstance(row, dict) and row.get("error"):
        return {"error": row["error"], "merchant_short_id": short, "city": city}
    if not row:
        return {"missing": True, "merchant_short_id": short, "city": city}
    return row


def get_full_context():
    return {
        "merchants": get_merchants(),
        "riders": get_riders(),
        "drivers": get_drivers(),
        "variants": get_variants(),
        "admin_credentials": get_admin_credentials(),
        "recon_harness": get_recon_harness_defaults(),
    }


class ContextHandler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        print(f"  \033[93m[Context API]\033[0m {args[0]}")

    def handle_one_request(self):
        try:
            super().handle_one_request()
        except BrokenPipeError:
            pass  # Client disconnected

    def _cors_headers(self):
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "*")

    def _send_json(self, data, status=200):
        try:
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self._cors_headers()
            self.end_headers()
            self.wfile.write(json.dumps(data, default=str).encode())
        except BrokenPipeError:
            pass  # Client disconnected before response was fully written

    def do_OPTIONS(self):
        self.send_response(200)
        self._cors_headers()
        self.end_headers()

    def _proxy(self, method):
        parsed = urlparse(self.path)
        path = parsed.path

        # Determine target — rider uses /v2 prefix, driver uses /ui prefix, lts direct, fleet direct
        LTS_URL = os.environ.get("LTS_URL", "http://localhost:8081")
        PROVIDER_DASHBOARD_URL = os.environ.get("PROVIDER_DASHBOARD_URL", "http://localhost:8018")
        MOCK_IDFY_URL = os.environ.get("MOCK_IDFY_URL", "http://localhost:6235")
        MOCK_SERVER_URL = os.environ.get("MOCK_SERVER_URL", "http://localhost:8080")
        RIDER_DASHBOARD_URL = os.environ.get("RIDER_DASHBOARD_URL", "http://localhost:8017")
        if path.startswith("/proxy/rider-dashboard/"):
            target_base = RIDER_DASHBOARD_URL
            target_path = path[len("/proxy/rider-dashboard"):]
        elif path.startswith("/proxy/mock-server/"):
            target_base = MOCK_SERVER_URL
            target_path = path[len("/proxy/mock-server"):]
        elif path.startswith("/proxy/mock-idfy/"):
            target_base = MOCK_IDFY_URL
            target_path = path[len("/proxy/mock-idfy"):]
        elif path.startswith("/proxy/rider-raw/"):
            target_base = RIDER_URL
            target_path = path[len("/proxy/rider-raw"):]
        elif path.startswith("/proxy/rider/"):
            target_base = RIDER_URL
            target_path = "/v2" + path[len("/proxy/rider"):]
        elif path.startswith("/proxy/lts-raw/"):
            target_base = LTS_URL
            target_path = path[len("/proxy/lts-raw"):]
        elif path.startswith("/proxy/lts/"):
            target_base = LTS_URL
            target_path = "/ui" + path[len("/proxy/lts"):]
        elif path.startswith("/proxy/provider-dashboard/"):
            target_base = PROVIDER_DASHBOARD_URL
            target_path = path[len("/proxy/provider-dashboard"):]
        elif path.startswith("/proxy/driver-raw/"):
            target_base = DRIVER_URL
            target_path = path[len("/proxy/driver-raw"):]
        elif path.startswith("/proxy/driver/"):
            target_base = DRIVER_URL
            target_path = "/ui" + path[len("/proxy/driver"):]
        else:
            return False

        target_url = f"{target_base}{target_path}"
        if parsed.query:
            target_url += f"?{parsed.query}"

        # Read request body
        content_len = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(content_len) if content_len > 0 else None

        # Forward headers (except Host)
        fwd_headers = {}
        for key in self.headers:
            if key.lower() not in ("host", "origin", "referer"):
                fwd_headers[key] = self.headers[key]

        try:
            req = urllib.request.Request(target_url, data=body, headers=fwd_headers, method=method)
            with urllib.request.urlopen(req, timeout=30) as resp:
                resp_body = resp.read()
                self.send_response(resp.status)
                self.send_header("Content-Type", resp.headers.get("Content-Type", "application/json"))
                self._cors_headers()
                self.end_headers()
                self.wfile.write(resp_body)
        except urllib.error.HTTPError as e:
            resp_body = e.read()
            self.send_response(e.code)
            self.send_header("Content-Type", "application/json")
            self._cors_headers()
            self.end_headers()
            self.wfile.write(resp_body)
        except Exception as e:
            self._send_json({"error": str(e)}, 502)

        return True

    def _read_json_body(self):
        content_len = int(self.headers.get("Content-Length", 0))
        if content_len > 0:
            return json.loads(self.rfile.read(content_len))
        return {}

    def _handle(self, method):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")

        # Proxy requests
        if path.startswith("/proxy/"):
            return self._proxy(method)

        # POST API endpoints
        if method == "POST" and path == "/api/logs/start":
            token = start_log_tails()
            self._send_json({"token": token})
            return True

        if method == "POST" and path == "/api/logs/stop":
            body = self._read_json_body()
            token = body.get("token", "")
            logs = stop_log_tails(token)
            self._send_json({"logs": logs})
            return True

        if method == "POST" and path == "/api/redis/flushall":
            try:
                import subprocess
                result = subprocess.run(
                    ["redis-cli", "--cluster", "call", "localhost:30001", "flushall"],
                    capture_output=True, text=True, timeout=10
                )
                if result.returncode == 0:
                    self._send_json({"result": "ok", "output": result.stdout.strip()})
                else:
                    # Fallback to single-node flush (non-cluster local setup)
                    result2 = subprocess.run(
                        ["redis-cli", "-p", "6379", "flushall"],
                        capture_output=True, text=True, timeout=10
                    )
                    if result2.returncode == 0:
                        self._send_json({"result": "ok", "output": result2.stdout.strip(), "mode": "single-node"})
                    else:
                        self._send_json({"error": result.stderr.strip() or result2.stderr.strip()}, 500)
            except Exception as e:
                self._send_json({"error": str(e)}, 500)
            return True

        if method == "POST" and path == "/api/inflate-distance":
            body = self._read_json_body()
            ride_id = body.get("rideId")
            multiplier = body.get("multiplier", 3)
            if not ride_id:
                self._send_json({"error": "rideId required"}, 400)
                return True
            try:
                conn = get_conn()
                conn.autocommit = True
                cur = conn.cursor()
                cur.execute("""
                    UPDATE atlas_driver_offer_bpp.ride r
                    SET traveled_distance = COALESCE(
                        (SELECT estimated_distance FROM atlas_driver_offer_bpp.booking WHERE id = r.booking_id), 5000
                    ) * %s
                    WHERE r.id = %s
                """, (multiplier, ride_id))
                rows = cur.rowcount
                conn.close()
                if rows > 0:
                    self._send_json({"result": "Success", "rowsUpdated": rows, "multiplier": multiplier})
                else:
                    self._send_json({"error": f"No ride found with id {ride_id}"}, 404)
            except Exception as e:
                self._send_json({"error": str(e)}, 500)
            return True

        if method == "POST" and path == "/api/finance/run-recon-db-prerequisites":
            body = self._read_json_body()
            moc_id = body.get("merchantOperatingCityId")
            result = run_recon_db_prerequisites(moc_id)
            if result.get("error"):
                self._send_json(result, 400 if "required" in result.get("error", "") else 500)
            else:
                self._send_json(result, 200)
            return True

        if method == "POST" and path == "/api/finance/seed-payment-settlement":
            body = self._read_json_body()
            subscription_purchase_id = body.get("subscriptionPurchaseId")
            if not subscription_purchase_id:
                self._send_json({"error": "subscriptionPurchaseId required"}, 400)
                return True
            result = seed_payment_settlement(subscription_purchase_id, body.get("overrides"))
            if result.get("error"):
                self._send_json(result, 500)
            else:
                self._send_json(result, 201)
            return True

        if method == "POST" and path == "/api/finance/seed-payout-settlement":
            self._send_json(
                {
                    "error": "Not implemented yet: payout settlement seeding needs a real sample row shape",
                    "status": "PENDING_FIXTURE",
                },
                501,
            )
            return True

        if method == "POST" and path == "/api/finance/trigger-recon":
            body = self._read_json_body()
            reconciliation_type = body.get("reconciliationType")
            merchant_id = body.get("merchantId")
            merchant_operating_city_id = body.get("merchantOperatingCityId")
            start_time = parse_iso8601(body.get("startTime"))
            end_time = parse_iso8601(body.get("endTime"))
            if not all([reconciliation_type, merchant_id, merchant_operating_city_id, start_time, end_time]):
                self._send_json(
                    {
                        "error": "reconciliationType, merchantId, merchantOperatingCityId, startTime, endTime are required"
                    },
                    400,
                )
                return True
            result = trigger_reconciliation_via_api(body, self.headers)
            if result.get("error"):
                self._send_json(result, 500)
            else:
                self._send_json(result, 201)
            return True

        # GET API endpoints
        if method != "GET" and not path.startswith("/proxy/"):
            self._send_json({"error": "method not allowed"}, 405)
            return True

        params = parse_qs(parsed.query)

        if path == "/api/context":
            self._send_json(get_full_context())
        elif path == "/api/riders":
            self._send_json(get_riders())
        elif path == "/api/drivers":
            self._send_json(get_drivers())
        elif path == "/api/merchants":
            self._send_json(get_merchants())
        elif path.startswith("/api/variants"):
            parts = path.split("/")
            city_id = parts[3] if len(parts) > 3 else None
            self._send_json(get_variants(city_id))
        elif path == "/api/collections":
            self._send_json(scan_collections())
        elif path.startswith("/api/collection/"):
            parts = path.split("/")
            if len(parts) >= 5:
                directory = parts[3]
                filename = "/".join(parts[4:])
                data = get_collection_file(directory, filename)
                if data:
                    self._send_json(data)
                else:
                    self._send_json({"error": "collection not found"}, 404)
            else:
                self._send_json({"error": "usage: /api/collection/<dir>/<file>"}, 400)
        elif path == "/api/finance/ledger-entries":
            send_query_result(
                self,
                get_ledger_entries(
                    {
                        "bookingId": get_param(params, "bookingId"),
                        "referenceId": get_param(params, "referenceId"),
                        "referenceType": get_param(params, "referenceType"),
                        "merchantId": get_param(params, "merchantId"),
                        "merchantOperatingCityId": get_param(params, "merchantOperatingCityId"),
                        "fromTime": parse_iso8601(get_param(params, "from")),
                        "toTime": parse_iso8601(get_param(params, "to")),
                    }
                ),
            )
        elif path == "/api/finance/recon-summaries":
            send_query_result(
                self,
                get_recon_summaries(
                    {
                        "fromTime": parse_iso8601(get_param(params, "from")),
                        "toTime": parse_iso8601(get_param(params, "to")),
                        "reconciliationType": get_param(params, "reconciliationType"),
                        "merchantId": get_param(params, "merchantId"),
                        "merchantOperatingCityId": get_param(params, "merchantOperatingCityId"),
                    }
                ),
            )
        elif path == "/api/finance/recon-entries":
            send_query_result(
                self,
                get_recon_entries(
                    {
                        "summaryId": get_param(params, "summaryId"),
                        "bookingId": get_param(params, "bookingId"),
                        "reconciliationType": get_param(params, "reconciliationType"),
                        "merchantId": get_param(params, "merchantId"),
                        "merchantOperatingCityId": get_param(params, "merchantOperatingCityId"),
                    }
                ),
            )
        elif path == "/api/finance/invoices":
            send_query_result(
                self,
                get_invoices(
                    {
                        "referenceId": get_param(params, "referenceId"),
                        "invoiceId": get_param(params, "invoiceId"),
                    }
                ),
            )
        elif path == "/api/finance/subscription-purchases":
            send_query_result(
                self,
                get_subscription_purchases(
                    {
                        "ownerId": get_param(params, "ownerId"),
                        "status": get_param(params, "status"),
                        "merchantId": get_param(params, "merchantId"),
                        "merchantOperatingCityId": get_param(params, "merchantOperatingCityId"),
                        "fromTime": parse_iso8601(get_param(params, "from")),
                        "toTime": parse_iso8601(get_param(params, "to")),
                    }
                ),
            )
        elif path == "/api/finance/job-status":
            result = get_job_status(
                job_id=get_param(params, "jobId"),
                recon_type=get_param(params, "reconciliationType"),
                merchant_id=get_param(params, "merchantId"),
                merchant_operating_city_id=get_param(params, "merchantOperatingCityId"),
                start_time=parse_iso8601(get_param(params, "startTime")),
                end_time=parse_iso8601(get_param(params, "endTime")),
            )
            send_query_result(self, result or {"error": "job not found"})
        elif path == "/api/finance/readiness":
            recon_type = get_param(params, "reconciliationType")
            merchant_id = get_param(params, "merchantId")
            merchant_operating_city_id = get_param(params, "merchantOperatingCityId")
            start_time = parse_iso8601(get_param(params, "startTime"))
            end_time = parse_iso8601(get_param(params, "endTime"))
            if not all([recon_type, merchant_id, merchant_operating_city_id, start_time, end_time]):
                self._send_json(
                    {
                        "error": "reconciliationType, merchantId, merchantOperatingCityId, startTime, endTime are required"
                    },
                    400,
                )
                return True
            pre = run_recon_db_prerequisites(merchant_operating_city_id)
            if pre.get("error"):
                self._send_json(pre, 400 if "required" in pre.get("error", "") else 500)
                return True
            if pre.get("transporterConfigRowsUpdated", -1) == 0:
                self._send_json(
                    {
                        "error": "No transporter_config row for merchantOperatingCityId — select a driver/city in the config bar",
                        "dbPrerequisites": pre,
                    },
                    400,
                )
                return True
            payload = build_readiness(
                recon_type,
                merchant_id,
                merchant_operating_city_id,
                start_time,
                end_time,
            )
            if isinstance(payload, dict):
                payload["dbPrerequisites"] = pre
            self._send_json(payload)
            return True
        elif path == "/api/health":
            self._send_json({"status": "ok"})
        else:
            self._send_json({"error": "not found"}, 404)
        return True

    def do_GET(self):
        self._handle("GET")

    def do_POST(self):
        self._handle("POST")

    def do_PUT(self):
        self._handle("PUT")

    def do_DELETE(self):
        self._handle("DELETE")


def main():
    port = PORT
    for i, arg in enumerate(sys.argv):
        if arg == "--port" and i + 1 < len(sys.argv):
            port = int(sys.argv[i + 1])

    server = HTTPServer(("0.0.0.0", port), ContextHandler)
    print(f"\n  \033[93m📋 Context API + CORS Proxy on http://localhost:{port}\033[0m")
    print(f"  DB: {DB_CONFIG['user']}@{DB_CONFIG['host']}:{DB_CONFIG['port']}/{DB_CONFIG['dbname']}")
    print(f"  Proxy: /proxy/rider/* → {RIDER_URL}")
    print(f"  Proxy: /proxy/driver/* → {DRIVER_URL}\n")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutdown.")
        server.server_close()


if __name__ == "__main__":
    main()
