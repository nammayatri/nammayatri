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

Port: 7082
"""

import json
import sys
import os
import glob
import subprocess
import threading
import time
import math
import uuid
from pathlib import Path
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse
import urllib.request
import urllib.error


def redis_cmd(*args):
    """Run a redis-cli command against the cluster, fallback to single-node."""
    cluster_cmd = ["redis-cli", "--cluster",
                   "call", "localhost:30001"] + list(args)
    r = subprocess.run(cluster_cmd, capture_output=True, text=True, timeout=10)
    if r.returncode == 0:
        return True, r.stdout
    # fallback single-node
    r2 = subprocess.run(["redis-cli"] + list(args),
                        capture_output=True, text=True, timeout=10)
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
               m.short_id as merchant, m.id as merchant_id, moc.city, moc.currency,
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
                    vals = {v["key"]: v["value"] for v in env_data.get(
                        "values", []) if v.get("enabled", True)}
                    env_name = f.name.replace("Local_", "").replace(
                        ".postman_environment.json", "")
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
            t = threading.Thread(target=_reader_thread,
                                 args=(lines, proc), daemon=True)
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


def get_full_context():
    return {
        "merchants": get_merchants(),
        "riders": get_riders(),
        "drivers": get_drivers(),
        "variants": get_variants(),
        "admin_credentials": get_admin_credentials(),
    }


# ── Finance Visualization Queries ──

def get_finance_accounts(schema):
    """Get all finance accounts with balances."""
    return query(f"""
        SELECT a.id, a.account_type, a.counterparty_type, a.counterparty_id,
               a.balance, a.currency, a.status
        FROM {schema}.finance_account a
        ORDER BY a.account_type, a.counterparty_type
    """)


def get_finance_ledger_entries(schema, reference_type=None, reference_id=None, counterparty_id=None, limit=50, offset=0):
    """Get finance ledger entries with optional filters."""
    conditions = []
    params = []
    if reference_type:
        conditions.append("le.reference_type = %s")
        params.append(reference_type)
    if reference_id:
        conditions.append("le.reference_id = %s")
        params.append(reference_id)
    if counterparty_id:
        conditions.append(
            "(fa_from.counterparty_id = %s OR fa_to.counterparty_id = %s)")
        params.extend([counterparty_id, counterparty_id])
    where = (" WHERE " + " AND ".join(conditions)) if conditions else ""
    params.extend([limit, offset])
    return query(f"""
        SELECT le.id, le.reference_type, le.reference_id, le.amount, le.currency,
               le.status, le.entry_type, le.created_at,
               fa_from.account_type as from_account_type, fa_from.counterparty_type as from_counterparty,
               fa_to.account_type as to_account_type, fa_to.counterparty_type as to_counterparty
        FROM {schema}.finance_ledger_entry le
        LEFT JOIN {schema}.finance_account fa_from ON fa_from.id = le.from_account_id
        LEFT JOIN {schema}.finance_account fa_to ON fa_to.id = le.to_account_id
        {where}
        ORDER BY le.created_at DESC
        LIMIT %s OFFSET %s
    """, tuple(params))


def get_finance_invoices(schema, issued_to_id=None, invoice_type=None, limit=20, offset=0):
    """Get finance invoices with optional filters."""
    conditions = []
    params = []
    if issued_to_id:
        conditions.append("fi.issued_to_id = %s")
        params.append(issued_to_id)
    if invoice_type:
        conditions.append("fi.invoice_type = %s")
        params.append(invoice_type)
    where = (" WHERE " + " AND ".join(conditions)) if conditions else ""
    params.extend([limit, offset])
    return query(f"""
        SELECT fi.id, fi.invoice_number, fi.invoice_type, fi.status,
               fi.total_amount, fi.subtotal, fi.currency,
               fi.issued_to_type, fi.issued_to_id, fi.issued_to_name,
               fi.issued_by_name, fi.line_items::text as line_items,
               fi.created_at, fi.tax_breakdown
        FROM {schema}.finance_invoice fi
        {where}
        ORDER BY fi.created_at DESC
        LIMIT %s OFFSET %s
    """, tuple(params))


def get_finance_earnings(schema, counterparty_id=None):
    """Wallet-style earnings summary for BPP drivers, matching the shape
    returned by the driver-app /wallet/transactions endpoint.

    Entries where a DRIVER Asset account is the `to` side are additions
    (credits to the driver's wallet); where DRIVER Asset is the `from` side
    are deductions. If counterparty_id is given, scope to that driver;
    otherwise aggregate platform-wide across every DRIVER wallet.
    """
    # Friendly labels — mirror referenceTypeToItemName in
    # Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/DriverWallet.hs
    LABELS = {
        "BaseRide": "Ride Earnings",
        "Tips": "Tips",
        "TollCharges": "Toll Charges",
        "ParkingCharges": "Parking Charges",
        "Topup": "Wallet Top-up",
        "GSTOnline": "GST (Online)",
        "GSTCash": "GST (Cash)",
        "VATOnline": "VAT (Online)",
        "VATCash": "VAT (Cash)",
        "TDSDeductionOnline": "TDS (Online)",
        "TDSDeductionCash": "TDS (Cash)",
        "TDSDeductionCancellation": "TDS (Cancellation)",
        "Payout": "Withdrawal",
        "AirportCashRecharge": "Airport cash recharge (booth)",
        "AirportEntryFee": "Airport Entry Fee",
        "AirportEntryFeeGST": "Airport Entry Fee GST",
        "CommissionOnline": "Platform Commission (Online)",
        "CommissionCash": "Platform Commission (Cash)",
        "DriverCancellationCharges": "Driver Cancellation Charges",
        "CustomerCancellationCharges": "Customer Cancellation Charges",
        "CustomerCancellationGST": "Customer Cancellation GST",
        "WalletIncentive": "Incentive",
        "D2DReferral": "Referral Bonus",
        "DiscountsOnline": "Discounts (Online)",
        "DiscountsCash": "Discounts (Cash)",
        "RideVatOnDiscount": "VAT on Discount",
        "VATAbsorbedOnDiscount": "VAT Absorbed on Discount",
        "VATInput": "VAT Input",
        "CancellationVATInput": "Cancellation VAT Input",
    }

    driver_filter = ""
    params: list = []
    if counterparty_id:
        driver_filter = "AND fa.counterparty_id = %s"
        params = [counterparty_id]

    def grouped(direction_col: str):
        # direction_col is either 'to_account_id' (credits/additions)
        # or 'from_account_id' (debits/deductions).
        sql = f"""
            SELECT le.reference_type,
                   COALESCE(SUM(le.amount), 0) AS total,
                   COUNT(*) AS cnt,
                   MAX(le.currency) AS currency
            FROM {schema}.finance_ledger_entry le
            JOIN {schema}.finance_account fa ON fa.id = le.{direction_col}
            WHERE fa.counterparty_type IN ('DRIVER', 'FLEET_OWNER')
              AND fa.account_type IN ('Liability', 'Control')
              AND le.status = 'SETTLED'
              {driver_filter}
            GROUP BY le.reference_type
            ORDER BY total DESC
        """
        rows = query(sql, tuple(params))
        return [] if isinstance(rows, dict) else rows

    add_rows = grouped("to_account_id")
    ded_rows = grouped("from_account_id")

    def to_items(rows):
        out = []
        for r in rows:
            amt = float(r["total"]) if r["total"] is not None else 0.0
            out.append({
                "refType": r["reference_type"],
                "itemName": LABELS.get(r["reference_type"], r["reference_type"]),
                "amount": amt,
                "entries": r["cnt"],
                "currency": r.get("currency"),
            })
        return out

    additions = to_items(add_rows)
    deductions = to_items(ded_rows)
    add_total = sum(i["amount"] for i in additions)
    ded_total = sum(i["amount"] for i in deductions)
    currency = (add_rows[0]["currency"] if add_rows
                else (ded_rows[0]["currency"] if ded_rows else None))

    # Wallet balance (real platform-owed-to-driver): Liability only.
    # Cash-earnings tracker: Control account (no real platform obligation,
    # just cumulative memo of direct rider→driver cash).
    balance = None
    cash_earnings_tracked = None
    if counterparty_id:
        bal_rows = query(
            f"""
            SELECT account_type, balance FROM {schema}.finance_account
            WHERE counterparty_type IN ('DRIVER', 'FLEET_OWNER')
              AND counterparty_id = %s
              AND account_type IN ('Liability', 'Control')
            """,
            (counterparty_id,),
        )
        if bal_rows and not isinstance(bal_rows, dict):
            for row in bal_rows:
                if row["account_type"] == "Liability":
                    balance = float(row["balance"])
                elif row["account_type"] == "Control":
                    cash_earnings_tracked = float(row["balance"])

    return {
        "counterpartyId": counterparty_id,
        "balance": balance,
        "cashEarningsTracked": cash_earnings_tracked,
        "currency": currency,
        "additions": {"totalAmount": add_total, "items": additions},
        "deductions": {"totalAmount": ded_total, "items": deductions},
        "net": add_total - ded_total,
    }


def get_finance_reference_types(schema):
    """Get distinct reference types from ledger entries."""
    return query(f"""
        SELECT DISTINCT reference_type FROM {schema}.finance_ledger_entry ORDER BY reference_type
    """)


def get_finance_dashboard(schema):
    """Full finance dashboard data: accounts, timeline grouped by reference_id, summary stats."""
    accounts = query(f"""
        SELECT a.id, a.account_type, a.counterparty_type, a.counterparty_id,
               a.balance, a.currency, a.status,
               COALESCE((SELECT SUM(le.amount) FROM {schema}.finance_ledger_entry le WHERE le.to_account_id = a.id AND le.amount > 0), 0) as total_credits,
               COALESCE((SELECT SUM(ABS(le.amount)) FROM {schema}.finance_ledger_entry le WHERE le.from_account_id = a.id AND le.amount > 0), 0) as total_debits
        FROM {schema}.finance_account a
        ORDER BY a.account_type, a.counterparty_type
    """)

    # Ledger entries with account info, ordered by time
    entries = query(f"""
        SELECT le.id, le.reference_type, le.reference_id, le.amount, le.currency,
               le.status, le.entry_type, le.created_at,
               le.from_account_id, le.to_account_id,
               COALESCE(fa_from.account_type, 'Unknown') as from_type,
               COALESCE(fa_from.counterparty_type, '') as from_counterparty,
               COALESCE(fa_to.account_type, 'Unknown') as to_type,
               COALESCE(fa_to.counterparty_type, '') as to_counterparty
        FROM {schema}.finance_ledger_entry le
        LEFT JOIN {schema}.finance_account fa_from ON fa_from.id = le.from_account_id
        LEFT JOIN {schema}.finance_account fa_to ON fa_to.id = le.to_account_id
        ORDER BY le.created_at DESC
        LIMIT 500
    """)

    # Invoices
    invoices = query(f"""
        SELECT fi.id, fi.invoice_number, fi.invoice_type, fi.status,
               fi.total_amount, fi.subtotal, fi.currency,
               fi.issued_to_type, fi.issued_to_id, fi.issued_to_name,
               fi.issued_by_name, fi.line_items::text as line_items,
               fi.created_at, fi.tax_breakdown
        FROM {schema}.finance_invoice fi
        ORDER BY fi.created_at DESC
        LIMIT 100
    """)

    # Invoice-to-entry links
    links = query(f"""
        SELECT l.invoice_id, l.ledger_entry_id
        FROM {schema}.finance_invoice_ledger_link l
    """)

    if isinstance(entries, dict) and "error" in entries:
        entries = []
    if isinstance(invoices, dict) and "error" in invoices:
        invoices = []
    if isinstance(links, dict) and "error" in links:
        links = []
    if isinstance(accounts, dict) and "error" in accounts:
        accounts = []

    # Build invoice lookup by entry_id AND by reference_id
    entry_to_invoice = {}
    ref_to_invoice = {}
    invoice_map = {inv["id"]: inv for inv in invoices}
    for lnk in links:
        inv = invoice_map.get(lnk["invoice_id"])
        if inv:
            entry_to_invoice[lnk["ledger_entry_id"]] = inv
    # Also map invoices by issued_to_id for fallback matching
    for inv in invoices:
        ref_to_invoice.setdefault(inv.get("issued_to_id"), []).append(inv)

    # Group entries by reference_id
    from collections import OrderedDict
    groups = OrderedDict()
    for le in entries:
        rid = le["reference_id"] or "unknown"
        if rid not in groups:
            groups[rid] = {"reference_id": rid, "entries": [
            ], "primary_type": None, "earliest": le["created_at"], "invoice": None}
        groups[rid]["entries"].append(le)
        if not groups[rid]["primary_type"]:
            groups[rid]["primary_type"] = le["reference_type"]
        if le["created_at"] and (not groups[rid]["earliest"] or le["created_at"] < groups[rid]["earliest"]):
            groups[rid]["earliest"] = le["created_at"]
        # Attach invoice if linked (via entry link or by reference_id match)
        inv = entry_to_invoice.get(le["id"])
        if inv and not groups[rid]["invoice"]:
            groups[rid]["invoice"] = inv

    timeline = list(groups.values())

    # Summary stats
    active_inv = sum(1 for i in invoices if i.get(
        "status") not in ("Voided", "Cancelled"))
    voided_inv = sum(1 for i in invoices if i.get("status") == "Voided")

    return {
        "accounts": accounts,
        "timeline": timeline,
        "summary": {
            "totalEntries": len(entries),
            "totalInvoices": len(invoices),
            "activeInvoices": active_inv,
            "voidedInvoices": voided_inv,
        },
    }


def clear_finance_data(schema):
    """Truncate every finance-module table in the given schema for a clean slate.
    Uses TRUNCATE ... RESTART IDENTITY CASCADE so FK deps are handled and any
    sequences reset. Tables that don't exist in this schema are skipped."""
    # Every table owned by the finance-kernel storage specs. Keep in sync with
    # Backend/lib/finance-kernel/spec/Storage/*.yaml `tableName` values.
    FINANCE_TABLES = [
        "finance_account",
        "finance_audit_entry",
        "finance_current_state",
        "finance_invoice",
        "finance_invoice_ledger_link",
        "finance_ledger_entry",
        "finance_reconciliation_entry",
        "finance_reconciliation_summary",
        "finance_state_transition",
        "direct_tax_transaction",
        "indirect_tax_transaction",
        "pg_payment_settlement_report",
        "pg_payout_settlement_report",
    ]
    try:
        conn = get_conn()
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute(
            """
            SELECT table_name FROM information_schema.tables
            WHERE table_schema = %s AND table_name = ANY(%s)
            """,
            (schema, FINANCE_TABLES),
        )
        present = [r[0] for r in cur.fetchall()]
        if not present:
            conn.close()
            return {"result": "Success", "message": f"No finance tables in {schema}", "tables": []}
        qualified = ", ".join(f"{schema}.{t}" for t in present)
        cur.execute(f"TRUNCATE TABLE {qualified} RESTART IDENTITY CASCADE")
        conn.close()
        return {"result": "Success", "message": f"Truncated finance tables in {schema}", "tables": present}
    except Exception as e:
        return {"error": str(e)}


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
        self.send_header("Access-Control-Allow-Methods",
                         "GET, POST, PUT, DELETE, OPTIONS")
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
        PROVIDER_DASHBOARD_URL = os.environ.get(
            "PROVIDER_DASHBOARD_URL", "http://localhost:8018")
        MOCK_IDFY_URL = os.environ.get(
            "MOCK_IDFY_URL", "http://localhost:6235")
        MOCK_SERVER_URL = os.environ.get(
            "MOCK_SERVER_URL", "http://localhost:8080")
        RIDER_DASHBOARD_URL = os.environ.get(
            "RIDER_DASHBOARD_URL", "http://localhost:8017")
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
            req = urllib.request.Request(
                target_url, data=body, headers=fwd_headers, method=method)
            with urllib.request.urlopen(req, timeout=30) as resp:
                resp_body = resp.read()
                self.send_response(resp.status)
                self.send_header(
                    "Content-Type", resp.headers.get("Content-Type", "application/json"))
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
                    ["redis-cli", "--cluster", "call",
                        "localhost:30001", "flushall"],
                    capture_output=True, text=True, timeout=10
                )
                if result.returncode == 0:
                    self._send_json(
                        {"result": "ok", "output": result.stdout.strip()})
                else:
                    # Fallback to single-node flush (non-cluster local setup)
                    result2 = subprocess.run(
                        ["redis-cli", "-p", "6379", "flushall"],
                        capture_output=True, text=True, timeout=10
                    )
                    if result2.returncode == 0:
                        self._send_json(
                            {"result": "ok", "output": result2.stdout.strip(), "mode": "single-node"})
                    else:
                        self._send_json(
                            {"error": result.stderr.strip() or result2.stderr.strip()}, 500)
            except Exception as e:
                self._send_json({"error": str(e)}, 500)

        if method == "POST" and path == "/api/finance/clear-all":
            body = self._read_json_body()
            side = body.get("side", "bpp")
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            result = clear_finance_data(schema)
            self._send_json(result)
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
                    self._send_json(
                        {"result": "Success", "rowsUpdated": rows, "multiplier": multiplier})
                else:
                    self._send_json(
                        {"error": f"No ride found with id {ride_id}"}, 404)
            except Exception as e:
                self._send_json({"error": str(e)}, 500)
            return True

        # GET API endpoints
        if method != "GET" and not path.startswith("/proxy/"):
            self._send_json({"error": "method not allowed"}, 405)
            return True

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
                self._send_json(
                    {"error": "usage: /api/collection/<dir>/<file>"}, 400)
        elif path == "/api/health":
            self._send_json({"status": "ok"})
        # ── Finance Dashboard ──
        elif path == "/api/finance/dashboard":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_dashboard(schema))
        # ── Finance Visualization (granular) ──
        elif path == "/api/finance/accounts":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_accounts(schema))
        elif path == "/api/finance/ledger":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_ledger_entries(
                schema,
                reference_type=qs.get("referenceType", [None])[0],
                reference_id=qs.get("referenceId", [None])[0],
                counterparty_id=qs.get("counterpartyId", [None])[0],
                limit=int(qs.get("limit", [50])[0]),
                offset=int(qs.get("offset", [0])[0]),
            ))
        elif path == "/api/finance/invoices":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_invoices(
                schema,
                issued_to_id=qs.get("issuedToId", [None])[0],
                invoice_type=qs.get("invoiceType", [None])[0],
                limit=int(qs.get("limit", [20])[0]),
                offset=int(qs.get("offset", [0])[0]),
            ))
        elif path == "/api/finance/earnings":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            schema = "atlas_driver_offer_bpp"  # earnings only on BPP
            self._send_json(get_finance_earnings(
                schema,
                counterparty_id=qs.get("counterpartyId", [None])[0],
            ))
        elif path == "/api/finance/reference-types":
            from urllib.parse import parse_qs
            qs = parse_qs(parsed.query)
            side = qs.get("side", ["bpp"])[0]
            schema = "atlas_app" if side == "bap" else "atlas_driver_offer_bpp"
            self._send_json(get_finance_reference_types(schema))
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
    print(
        f"\n  \033[93m📋 Context API + CORS Proxy on http://localhost:{port}\033[0m")
    print(
        f"  DB: {DB_CONFIG['user']}@{DB_CONFIG['host']}:{DB_CONFIG['port']}/{DB_CONFIG['dbname']}")
    print(f"  Proxy: /proxy/rider/* → {RIDER_URL}")
    print(f"  Proxy: /proxy/driver/* → {DRIVER_URL}\n")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutdown.")
        server.server_close()


if __name__ == "__main__":
    main()
