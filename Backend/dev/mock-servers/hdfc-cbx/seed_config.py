#!/usr/bin/env python3
"""Point the local driver app's HDFC CBX payout config at the mock (and back).

  python3 dev/mock-servers/hdfc-cbx/seed_config.py                   # every MSIL city -> mock
  python3 dev/mock-servers/hdfc-cbx/seed_config.py --city <moc_id>   # just these cities
  python3 dev/mock-servers/hdfc-cbx/seed_config.py --restore         # put the backed-up rows back

Writes merchant_service_config `Payout_HdfcCbx` (url/tokenUrl = the mock, keys from the mock's kit,
secrets encrypted by passetto), routes the city's payouts to HdfcCbx in merchant_service_usage_config and
subscription_config (the PREPAID_SUBSCRIPTION wallet payout service), and keeps payout_order off KV. After
that, every payout triggered in the city (instant, adhoc or the scheduled sweep) goes to the mock.
The city's previous rows are first saved under ~/.hdfc-cbx-mock/config-backup (outside the repo: it holds the
real config's encrypted secrets and certificates), so --restore returns the
city to whatever it was (e.g. real UAT). The driver app and allocator read this config and build their
HTTP managers at startup: restart both afterwards.
"""

import argparse
import json
import os
import subprocess
import sys
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, ROOT)

from services import hdfc_cbx  # noqa: E402

SCHEMA = "atlas_driver_offer_bpp"
# Outside the repo: a backup holds the real config being replaced (encrypted secrets, certificates).
BACKUP_DIR = os.environ.get("HDFC_MOCK_BACKUP_DIR", os.path.expanduser("~/.hdfc-cbx-mock/config-backup"))

# Compressed so a whole plan runs in ~30 min: 6 checks 2 min apart, then 2 more 7 min apart.
TEST_PLAN = {
    "firstCheckDelayMinutes": 2,
    "burstChecks": 6,
    "burstGapMinutes": 2,
    "tailChecks": 2,
    "tailGapMinutes": 7,
    "maxCallsPerCheck": 2,
    "readCallDelaySeconds": 10,
    "errorRetryDelaySeconds": 60,
    "maxCallsPerDay": 12,
}


def psql(args, query):
    cmd = ["psql", "-h", args.pg_host, "-p", str(args.pg_port), "-d", args.pg_db, "-v", "ON_ERROR_STOP=1", "-tAc", query]
    out = subprocess.run(cmd, capture_output=True, text=True)
    if out.returncode:
        sys.exit("psql failed: " + out.stderr.strip())
    return out.stdout.strip()


def q(text):
    return "'" + text.replace("'", "''") + "'"


def qn(value):
    return "NULL" if value is None else q(value)


def encrypt(args, plain):
    # passetto wants the value as a Haskell `show`n Text: S"<json string>"
    body = json.dumps({"value": "S" + json.dumps(plain)}).encode()
    req = urllib.request.Request(f"{args.passetto}/encrypt", data=body, headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=20) as r:
        return json.load(r)["value"]


def cities(args):
    where = f"moc.id IN ({', '.join(q(c) for c in args.city)})" if args.city else "m.short_id ILIKE '%MSIL%'"
    rows = psql(
        args,
        f"SELECT moc.id, moc.city, moc.merchant_id FROM {SCHEMA}.merchant_operating_city moc "
        f"JOIN {SCHEMA}.merchant m ON m.id = moc.merchant_id WHERE {where} ORDER BY moc.city;",
    )
    found = [line.split("|") for line in rows.splitlines() if line]
    if not found:
        sys.exit("No matching operating cities -- seed the database first, or pass --city.")
    return found


def read_kit(name):
    with open(os.path.join(hdfc_cbx.KEYS_DIR, name)) as f:
        return f.read()


def clear_config_caches(args, city_ids):
    """The app caches merchant service configs in Redis, and a restart does not clear them: a cached old row
    keeps the old HTTP-manager key, and every HDFC call then fails with HttpManagerNotFound. Exact keys
    only, one per call (the cluster refuses a multi-key DEL across slots)."""
    keys = [
        "dynamic-offer-driver-app:ConfigPilot:MerchantServiceConfigDriver",
        "dynamic-offer-driver-app:ConfigPilot:MerchantServiceUsageConfigDriver",
        "dynamic-offer-driver-app:ConfigPilot:SubscriptionConfig",
    ]
    for c in city_ids:
        keys += [
            f"driver-offer:CachedQueries:MerchantServiceConfig:MerchantOperatingCityId-{c}",
            f"driver-offer:CachedQueries:MerchantServiceUsageConfig:MerchantOperatingCityId-{c}",
            f"CacheHash:DRIVER-CONFIG_SubscriptionConfig-MerchantOperatingCityId:{c}",
        ]
    for key in keys:
        out = subprocess.run(["redis-cli", "-c", "-p", str(args.redis_port), "del", key], capture_output=True, text=True)
        print(f"cache cleared: {key} ({out.stdout.strip() or out.stderr.strip()})")


def backup_path(city_id):
    return os.path.join(BACKUP_DIR, f"{city_id}.json")


def seed(args):
    hdfc_cbx.ensure_keys()
    client_key = read_kit("client_priv.pem")
    config = {
        "url": args.base,
        "tokenUrl": args.base,
        "consumerKey": encrypt(args, "mock-consumer-key"),
        "consumerSecret": encrypt(args, "mock-consumer-secret"),
        "scope": args.scope,
        "apiKey": encrypt(args, "mock-consumer-key"),
        "clientCode": args.client_code,
        "groupId": args.group_id,
        "userId": args.user_id,
        "signingPrivateKey": encrypt(args, client_key),
        "bankPublicKey": read_kit("bank_pub.pem"),
        "clientCertPem": read_kit("client.crt"),
        "caBundlePem": read_kit("ca.pem"),
        "clientKeyPem": encrypt(args, client_key),
        "maxItemsPerBatch": args.max_items,
        "bulkStatusCheckPlan": TEST_PLAN,
        "ownBankIfscPrefix": "HDFC",
    }
    payload = json.dumps(config)
    os.makedirs(BACKUP_DIR, exist_ok=True)
    for city_id, city_name, merchant_id in cities(args):
        # Saved even when the city has no HDFC config yet, so --restore can put its routing back.
        existing = psql(
            args,
            f"SELECT json_build_object("
            f"'config', (SELECT config_json FROM {SCHEMA}.merchant_service_config "
            f" WHERE service_name = 'Payout_HdfcCbx' AND merchant_operating_city_id = {q(city_id)}), "
            f"'usage', (SELECT json_build_object('create_payout_order', create_payout_order, 'payout_order_status', payout_order_status) "
            f" FROM {SCHEMA}.merchant_service_usage_config WHERE merchant_operating_city_id = {q(city_id)}), "
            f"'subscription', (SELECT payout_service_name FROM {SCHEMA}.subscription_config "
            f" WHERE merchant_operating_city_id = {q(city_id)} AND service_name = 'PREPAID_SUBSCRIPTION' LIMIT 1));",
        )
        # Never overwrite a backup with the mock's own config: a second seed would lose the original.
        if not os.path.exists(backup_path(city_id)) and hdfc_cbx_url(existing) != args.base:
            with open(backup_path(city_id), "w") as f:
                f.write(existing)
            print(f"backed up {city_name} ({city_id}) -> {backup_path(city_id)}")
        psql(
            args,
            f"INSERT INTO {SCHEMA}.merchant_service_config "
            f"(merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at) "
            f"VALUES ({q(merchant_id)}, {q(city_id)}, 'Payout_HdfcCbx', {q(payload)}::json, now(), now()) "
            f"ON CONFLICT (service_name, merchant_operating_city_id) "
            f"DO UPDATE SET config_json = EXCLUDED.config_json, updated_at = now();",
        )
        psql(
            args,
            f"UPDATE {SCHEMA}.merchant_service_usage_config SET create_payout_order = 'HdfcCbx', payout_order_status = 'HdfcCbx' "
            f"WHERE merchant_operating_city_id = {q(city_id)};",
        )
        psql(
            args,
            f"UPDATE {SCHEMA}.subscription_config SET payout_service_name = 'Payout_HdfcCbx' "
            f"WHERE merchant_operating_city_id = {q(city_id)} AND service_name = 'PREPAID_SUBSCRIPTION';",
        )
        print(f"{city_name} ({city_id}) -> {args.base}")
    keep_payout_order_off_kv(args)
    clear_config_caches(args, [c[0] for c in cities(args)])
    print("Restart dynamic-offer-driver-app and driver-offer-allocator: they build the HDFC HTTP managers at startup.")


def hdfc_cbx_url(existing_json):
    try:
        return json.loads(existing_json)["config"]["url"]
    except Exception:
        return None


def keep_payout_order_off_kv(args):
    """createPayoutService reads a new payout order back by its orderId, which KV can't serve, so with payout_order
    on KV every bulk payout is refused ("Payout not sent: request is PROCESSING"). Kept off KV until the app reads
    the order back by its id. kv_configs is read at startup: restart the apps."""
    changed = psql(
        args,
        f"WITH u AS (UPDATE {SCHEMA}.system_configs SET config_value = jsonb_set(config_value::jsonb, '{{disableForKV}}', "
        f"coalesce(config_value::jsonb -> 'disableForKV', '[]'::jsonb) || '[\"payout_order\"]'::jsonb)::text "
        f"WHERE id = 'kv_configs' AND NOT coalesce(config_value::jsonb -> 'disableForKV', '[]'::jsonb) ? 'payout_order' "
        f"RETURNING id) SELECT count(*) FROM u;",
    )
    print("payout_order taken off KV" if changed == "1" else "payout_order already off KV")


def restore(args):
    for city_id, city_name, _merchant_id in cities(args):
        path = backup_path(city_id)
        if not os.path.exists(path):
            print(f"{city_name} ({city_id}): no backup, left as is")
            continue
        with open(path) as f:
            saved = json.load(f)
        if saved.get("config") is None:
            # The city had no HDFC config before the seed.
            psql(
                args,
                f"DELETE FROM {SCHEMA}.merchant_service_config "
                f"WHERE service_name = 'Payout_HdfcCbx' AND merchant_operating_city_id = {q(city_id)};",
            )
        else:
            psql(
                args,
                f"UPDATE {SCHEMA}.merchant_service_config SET config_json = {q(json.dumps(saved['config']))}::json, updated_at = now() "
                f"WHERE service_name = 'Payout_HdfcCbx' AND merchant_operating_city_id = {q(city_id)};",
            )
        usage = saved.get("usage") or {}
        if usage:
            psql(
                args,
                f"UPDATE {SCHEMA}.merchant_service_usage_config SET create_payout_order = {qn(usage['create_payout_order'])}, "
                f"payout_order_status = {qn(usage['payout_order_status'])} WHERE merchant_operating_city_id = {q(city_id)};",
            )
        if "subscription" in saved:
            psql(
                args,
                f"UPDATE {SCHEMA}.subscription_config SET payout_service_name = {qn(saved['subscription'])} "
                f"WHERE merchant_operating_city_id = {q(city_id)} AND service_name = 'PREPAID_SUBSCRIPTION';",
            )
        os.remove(path)
        print(f"{city_name} ({city_id}) restored -> {(saved.get('config') or {}).get('url', 'no HDFC config')}")
    clear_config_caches(args, [c[0] for c in cities(args)])
    # payout_order stays off KV: any HDFC config, mock or UAT, needs that on main.
    print("Restart dynamic-offer-driver-app and driver-offer-allocator: they build the HDFC HTTP managers at startup.")


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--city", action="append", help="merchant_operating_city id (repeatable); default every MSIL city")
    p.add_argument("--restore", action="store_true", help="restore the configs saved by an earlier seed")
    p.add_argument("--base", default="http://localhost:8080/hdfc", help="mock base URL (url and tokenUrl)")
    # Made-up identifiers: the mock accepts any, and real CBX ones do not belong in a mock config.
    p.add_argument("--scope", default="MOCKSCOPE")
    p.add_argument("--group-id", default="MOCKGROUP")
    p.add_argument("--client-code", default="9999")
    p.add_argument("--user-id", default="MOCKUSER")
    p.add_argument("--max-items", type=int, default=100)
    p.add_argument("--passetto", default="http://127.0.0.1:8021")
    p.add_argument("--pg-host", default="localhost")
    p.add_argument("--pg-port", default=5434)
    p.add_argument("--pg-db", default="atlas_dev")
    p.add_argument("--redis-port", default=30001, help="any node of the local Redis cluster")
    args = p.parse_args()
    restore(args) if args.restore else seed(args)


if __name__ == "__main__":
    main()
