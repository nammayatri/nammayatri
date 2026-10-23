#!/usr/bin/env python3
"""
RC transfer: activation rules, error codes and the "vehicle linked to another account" notice.

Covers Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate.linkRCStatus and
SharedLogic.RCTransferNotice end to end through the real driver-app API (POST /ui/rc/setStatus,
GET /ui/driver/profile, POST /ui/driver/rcTransferNotice/ack).

Fixture shortcut: there is no RC verification locally, so each test RC and its driver links are
inserted straight into Postgres (what upload + review would produce). Ages, rides and flags of the
drivers are staged the same way.

KV: person, driver_information, vehicle, driver_rc_association and the RC are KV tables. A row the
app has written lives in Redis (key <table>_<pk>_<value>{shard-N}, value "JSON{...}") until its TTL
and is read from there; Postgres only catches up when the drainer flushes. So every staged change is
written to Postgres AND patched into the Redis copy (kv_set), and state is read back through the API.

The window under test is transporter_config.automatic_r_c_activation_cut_off of the city. It is
cached (in memory and in Redis), so the script never changes it: it moves the timestamps the rule
compares against instead ("old" = 2 x cut-off ago). Restart driver-app after editing the cut-off.

Prerequisites:
  - `, run-mobility-stack-dev` up (driver-app :8016, Postgres :5434, passetto :8021).
  - Config imported for NAMMA_YATRI_PARTNER / Bangalore, with unifiedOnboardingFlagsRecompute (or
    requiresOnboardingInspection / enableBotFlow) on, so the enabled / approved checks apply.
  - Feature migration 0075 applied (RC_TRANSFERRED_* overlays).

Usage:
  python3 -I rc-transfer-test.py
  DRIVER_APP_LOG=/path/to/driver-app.log PSQL=/path/to/psql python3 -I rc-transfer-test.py
"""
import hashlib
import json
import os
import random
import shutil
import string
import subprocess
import sys
import time
import urllib.error
import urllib.request
import uuid

BASE = os.environ.get("DRIVER_APP_URL", "http://localhost:8016") + "/ui"
PASSETTO = os.environ.get("PASSETTO_URL", "http://localhost:8021")
MERCHANT_ID = os.environ.get("MERCHANT_ID", "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f")
MERCHANT_SHORT = os.environ.get("MERCHANT_SHORT_ID", "NAMMA_YATRI_PARTNER")
CITY = os.environ.get("CITY", "Bangalore")
OTP = os.environ.get("LOGIN_OTP", "7891")
PSQL = os.environ.get("PSQL") or shutil.which("psql")
REDIS_CLI = os.environ.get("REDIS_CLI") or shutil.which("redis-cli")
REDIS_PORTS = os.environ.get("REDIS_CLUSTER_PORTS", "30001,30002,30003,30004,30005,30006").split(",")
PG = ["-h", os.environ.get("PG_HOST", "localhost"), "-p", os.environ.get("PG_PORT", "5434"),
      "-U", os.environ.get("PG_USER", "atlas_driver_offer_bpp_user"), os.environ.get("PG_DB", "atlas_dev")]
PG_PASSWORD = os.environ.get("PG_PASSWORD", "atlas")
LOG = os.environ.get("DRIVER_APP_LOG", "/tmp/dynamic-offer-driver-app.log")
S = "atlas_driver_offer_bpp"
# Dev encHashSalt (dhall-configs/dev/secrets/dynamic-offer-driver-app.dhall).
HASH_SALT = "How wonderful it is that nobody need wait a single moment before starting to improve the world"

results = []
RC_NUMBERS = {}  # rc_id -> registration number, for reading RC state back through the API


# ── plumbing ──────────────────────────────────────────────────────────────────

def check(case, name, ok, detail=""):
    results.append((f"{case}: {name}", ok))
    print(("  PASS " if ok else "  FAIL ") + name + ("" if ok or not detail else f"  [{detail}]"))


def sql(q):
    out = subprocess.run([PSQL, *PG, "-Atc", q], capture_output=True, text=True,
                         env=dict(os.environ, PGPASSWORD=PG_PASSWORD))
    if out.returncode != 0:
        raise RuntimeError(f"{out.stderr.strip()}\n  in: {q.strip()[:200]}")
    return out.stdout.strip()


def redis(port, *args):
    out = subprocess.run([REDIS_CLI, "-c", "-p", port, *args], capture_output=True, text=True)
    return out.stdout


def kv_set(key_glob, fields, where=None):
    """Patch the app's Redis copy of a KV row (if cached) so reads see `fields`; keeps the TTL.
    `where` filters rows by their JSON fields when the key glob is not unique."""
    keys = set()
    for port in REDIS_PORTS:
        keys.update(k for k in redis(port, "--scan", "--count", "10000", "--pattern", key_glob).split() if k)
    for key in keys:
        raw = redis(REDIS_PORTS[0], "get", key).rstrip("\n")
        if not raw.startswith("JSON{"):
            continue
        row = json.loads(raw[4:])
        if where and any(row.get(k) != v for k, v in where.items()):
            continue
        row.update(fields)
        redis(REDIS_PORTS[0], "set", key, "JSON" + json.dumps(row, separators=(",", ":")), "KEEPTTL")


def call(method, url, body=None, token=None):
    headers = {"Content-Type": "application/json"}
    if token:
        headers["token"] = token
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, headers=headers, method=method)
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            raw = r.read().decode()
            return r.status, json.loads(raw) if raw else {}
    except urllib.error.HTTPError as e:
        raw = e.read().decode()
        try:
            return e.code, json.loads(raw)
        except ValueError:
            return e.code, {"raw": raw}


def moc_id():
    return sql(f"select id from {S}.merchant_operating_city where merchant_id = '{MERCHANT_ID}' and city = '{CITY}'")


def cut_off_seconds():
    return int(sql(f"select automatic_r_c_activation_cut_off from {S}.transporter_config where merchant_operating_city_id = '{moc_id()}'"))


def log_lines(needle):
    try:
        with open(LOG, errors="ignore") as f:
            return [line for line in f if needle in line]
    except OSError:
        return None


# ── fixtures ──────────────────────────────────────────────────────────────────

def new_driver(enabled=True):
    mobile = "9" + "".join(random.choices(string.digits, k=9))
    s, auth = call("POST", f"{BASE}/auth", {"mobileNumber": mobile, "mobileCountryCode": "+91",
                                           "merchantId": MERCHANT_ID, "merchantOperatingCity": CITY})
    assert s == 200, f"auth {s} {auth}"
    s, ver = call("POST", f"{BASE}/auth/{auth['authId']}/verify", {"otp": OTP, "deviceToken": "rc-transfer-test"})
    assert s == 200, f"verify {s} {ver}"
    d = {"id": ver["person"]["id"], "token": ver["token"], "mobile": mobile}
    set_enabled(d, enabled)
    return d


def driver_info_set(d, sql_set, fields):
    sql(f"update {S}.driver_information set {sql_set} where driver_id = '{d['id']}'")
    kv_set(f"driverInformation_driverId_{d['id']}*", fields)


def set_enabled(d, enabled):
    v = "true" if enabled else "false"
    driver_info_set(d, f"enabled = {v}, verified = {v}", {"enabled": enabled, "verified": enabled})


def set_on_ride(d, on_ride):
    driver_info_set(d, f"on_ride = {'true' if on_ride else 'false'}", {"onRide": on_ride})


def go_online(d):
    driver_info_set(d, "active = true, mode = 'ONLINE'", {"active": True, "mode": "ONLINE"})


def age_account(d):
    """Make the driver's account older than the cut-off (the rule used when they have no rides)."""
    sql(f"update {S}.person set created_at = now() - interval '{2 * CUT_OFF} seconds' where id = '{d['id']}'")
    old = time.strftime("%Y-%m-%dT%H:%M:%S.000000Z", time.gmtime(time.time() - 2 * CUT_OFF))
    kv_set(f"*person_id_{d['id']}*", {"createdAt": old})


def rc_set(rc_id, sql_set, fields):
    sql(f"update {S}.vehicle_registration_certificate set {sql_set} where id = '{rc_id}'")
    kv_set(f"vehicleRegistrationCertificate_id_{rc_id}*", fields)


def deactivate_link(d, rc_id):
    """Leave the vehicle row but drop the active RC link (the vehicle-row-only state)."""
    sql(f"update {S}.driver_rc_association set is_rc_active = false where driver_id = '{d['id']}' and rc_id = '{rc_id}'")
    kv_set("driver[Rr][Cc]Association_*", {"isRcActive": False}, where={"driverId": d["id"], "rcId": rc_id})


def add_ride(d, ago_seconds, status="COMPLETED"):
    """A ride assigned to the driver `ago_seconds` ago (findLastRideAssigned orders by created_at).
    Rides are read back with their pickup location, so the ride gets a location mapping too."""
    ride_id, loc_id = str(uuid.uuid4()), str(uuid.uuid4())
    # merchant / city / trip category set on the ride itself, so reading it needs no booking row
    sql(f"""insert into {S}.ride (id, booking_id, driver_id, otp, short_id, status, tracking_url, merchant_id,
              merchant_operating_city_id, trip_category, created_at, updated_at)
            values ('{ride_id}', '{uuid.uuid4()}', '{d['id']}', '0000', '{uuid.uuid4().hex[:10]}', '{status}',
                    'http://localhost/track', '{MERCHANT_ID}', '{MOC}', 'OneWay_OneWayOnDemandDynamicOffer',
                    now() - interval '{ago_seconds} seconds', now())""")
    sql(f"insert into {S}.location (id, lat, lon, created_at, updated_at) values ('{loc_id}', 12.97, 77.59, now(), now())")
    sql(f"""insert into {S}.location_mapping (id, entity_id, location_id, "order", tag, version, created_at, updated_at)
            values ('{uuid.uuid4()}', '{ride_id}', '{loc_id}', 0, 'RIDE', 'LATEST', now(), now())""")


def encrypt(value):
    # The backend decrypts via /decrypt/obj and parses the result as JSON, so encrypt the quoted string.
    s, r = call("POST", f"{PASSETTO}/encrypt/obj", {"value": json.dumps(value)})
    assert s == 200, f"passetto encrypt {s} {r}"
    return r["value"]


def reg_no():
    return "KA01" + random.choice("ABCDEFGHJK") + random.choice("ABCDEFGHJK") + "".join(random.choices(string.digits, k=4))


def masked(reg):
    return reg[:3] + "..." + reg[-3:]  # Kernel.Utils.Text.maskText


def new_rc(linked_to, status="VALID", approved=True):
    """A reviewed RC linked (inactive) to each given driver. Returns its registration number and id."""
    reg, rc_id = reg_no(), str(uuid.uuid4())
    digest = hashlib.sha256((HASH_SALT + reg).encode()).hexdigest()
    sql(f"""insert into {S}.vehicle_registration_certificate
              (id, document_image_id, fitness_expiry, verification_status, verified, approved,
               certificate_number_hash, certificate_number_encrypted, failed_rules, vehicle_class,
               vehicle_manufacturer, vehicle_model, vehicle_variant, vehicle_color, vehicle_capacity,
               merchant_id, created_at, updated_at)
            values ('{rc_id}', '{uuid.uuid4()}', now() + interval '365 days', '{status}', true, {str(approved).lower()},
               decode('{digest}', 'hex'), '{encrypt(reg)}', array[]::text[], 'M1', 'Maruti', 'Swift Dzire',
               'SEDAN', 'White', 4, '{MERCHANT_ID}', now(), now())""")
    RC_NUMBERS[rc_id] = reg
    for d in linked_to:
        link(d, rc_id)
    return reg, rc_id


def link(d, rc_id):
    sql(f"""insert into {S}.driver_rc_association
              (id, driver_id, rc_id, associated_on, associated_till, consent, consent_timestamp, is_rc_active,
               merchant_id, merchant_operating_city_id)
            values ('{uuid.uuid4()}', '{d['id']}', '{rc_id}', now(), now() + interval '365 days', true, now(), false,
               '{MERCHANT_ID}', '{MOC}')""")


# ── API + state readers ───────────────────────────────────────────────────────

def activate(d, reg):
    return call("POST", f"{BASE}/rc/setStatus", {"rcNo": reg, "isActivate": True}, d["token"])


def profile(d):
    s, p = call("GET", f"{BASE}/driver/profile", token=d["token"])
    assert s == 200, f"profile {s} {p}"
    return p


def ack(d, transfer_id):
    return call("POST", f"{BASE}/driver/rcTransferNotice/ack", {"transferId": transfer_id}, d["token"])


def has_vehicle(d):
    return profile(d).get("linkedVehicle") is not None


def rc_active_for(d, rc_id):
    s, rcs = call("GET", f"{BASE}/rc/all", token=d["token"])
    assert s == 200, f"rc/all {s} {rcs}"
    reg = RC_NUMBERS[rc_id]
    return any(r.get("rcActive") and r["rcDetails"]["certificateNumber"] == reg for r in rcs)


def notices(d, wait_for=0):
    """Notice rows for the driver as previous holder; waits up to `wait_for` rows (KV drains to Postgres)."""
    def read():
        rows = sql(f"select id, rc_id, new_driver_id, has_other_vehicles, acknowledged from {S}.rc_transfer_notice "
                   f"where previous_driver_id = '{d['id']}' order by created_at")
        return [dict(zip(["id", "rc_id", "new", "has_other", "acked"], r.split("|"))) for r in rows.splitlines() if r]
    rows = read()
    for _ in range(60):
        if len(rows) >= wait_for:
            break
        time.sleep(1)
        rows = read()
    return rows


def overlay_row(key):
    r = sql(f"select title, description, ok_button_text, array_to_json(actions), coalesce(link, '') from {S}.merchant_overlay "
            f"where merchant_operating_city_id = '{MOC}' and overlay_key = '{key}' and language = 'ENGLISH' limit 1")
    if not r:
        return None
    title, desc, ok, actions, lnk = r.split("|")
    return {"title": title, "description": desc, "okButtonText": ok, "actions": json.loads(actions), "link": lnk or None}


# ── assertions shared by the cases ────────────────────────────────────────────

def expect_transfer(case, old, new, reg, rc_id, has_other):
    """`new` activates `reg`, taking it from `old`: one notice, one push attempt, copy for the right case."""
    go_online(old)  # so "forced OFFLINE" below is observed, not the default
    pushes_before = log_lines(old["id"])
    s, r = activate(new, reg)
    check(case, "activation by new driver succeeds", s == 200, f"{s} {r}")
    rows = [n for n in notices(old, wait_for=1) if n["rc_id"] == rc_id]
    check(case, "exactly one notice row for this transfer", len(rows) == 1, str(rows))
    if not rows:
        return None
    n = rows[-1]
    check(case, "notice names the new driver", n["new"] == new["id"])
    check(case, f"hasOtherVehicles = {has_other}", n["has_other"] == ("t" if has_other else "f"), n["has_other"])
    check(case, "notice starts unacknowledged", n["acked"] == "f")
    check(case, "old driver lost the vehicle", not has_vehicle(old) and not rc_active_for(old, rc_id))
    check(case, "new driver has the vehicle and active RC", has_vehicle(new) and rc_active_for(new, rc_id))

    p = profile(old)
    check(case, "old driver forced inactive / OFFLINE", p.get("active") is False and p.get("mode") == "OFFLINE",
          f"active={p.get('active')} mode={p.get('mode')}")
    notice = p.get("pendingRcTransferNotice")
    check(case, "profile returns pendingRcTransferNotice", notice is not None)
    if notice:
        check(case, "profile notice is this transfer", notice.get("transferId") == n["id"], notice.get("transferId"))
        check_overlay(case, notice.get("overlay") or {}, new, reg, n["id"],
                      "RC_TRANSFERRED_VEHICLE_REMOVED" if has_other else "RC_TRANSFERRED_NO_VEHICLE")

    time.sleep(2)  # push is sent in a fork after the notice is stored
    if pushes_before is not None:
        logged = [l for l in log_lines("RC_TRANSFER") if n["id"] in l]
        check(case, "one RC_TRANSFER audit log line", len(logged) == 1, len(logged))
        pushes = [l for l in log_lines(old["id"]) if l not in pushes_before and "FCM" in l and "recordAndNotifyRCTransfer" in l]
        check(case, "FCM push attempted for the old driver", len(pushes) >= 1, len(pushes))
    return n


def check_overlay(case, ov, new, reg, transfer_id, key):
    row = overlay_row(key)
    check(case, f"overlay config {key} exists", row is not None)
    if not row:
        return
    blob = json.dumps(ov)
    check(case, f"title is the {key} copy", ov.get("title") == row["title"], ov.get("title"))
    check(case, "description has the masked RC", ov.get("description") == row["description"].replace("{#rcNumber#}", masked(reg)),
          ov.get("description"))
    check(case, "full RC number never sent", reg not in blob)
    check(case, "no data about the new driver", new["id"] not in blob and new["mobile"] not in blob)
    check(case, "actions / link match config", ov.get("actions") == row["actions"] and ov.get("link") == row["link"],
          f"{ov.get('actions')} {ov.get('link')}")
    check(case, "ack endPoint is absolute", str(ov.get("endPoint", "")).startswith("http")
          and ov["endPoint"].endswith("/driver/rcTransferNotice/ack"), ov.get("endPoint"))
    check(case, "ack method POST, reqBody carries transferId",
          ov.get("method") == "POST" and (ov.get("reqBody") or {}).get("transferId") == transfer_id, f"{ov.get('method')} {ov.get('reqBody')}")
    check(case, "no secondary CALL_SUPPORT action", not ov.get("secondaryActions"), ov.get("secondaryActions"))


def expect_rejected(case, old, new, reg, rc_id, code):
    """`new` tries to take `reg`: rejected with `code`, and the current holder is left untouched."""
    s, r = activate(new, reg)
    check(case, f"rejected with {code}", s == 400 and r.get("errorCode") == code, f"{s} {r.get('errorCode')} {r.get('errorMessage')}")
    check(case, "error message is set", bool(r.get("errorMessage")))
    time.sleep(2)
    if old:
        check(case, "holder keeps vehicle and active RC", has_vehicle(old) and rc_active_for(old, rc_id))
        check(case, "no notice for the holder", notices(old) == [] and profile(old).get("pendingRcTransferNotice") is None)
    check(case, "new driver got nothing", not rc_active_for(new, rc_id))
    return r.get("errorCode")


def holder_with_rc():
    """A driver holding one active RC, plus the RC; the holder is old enough to lose it."""
    a = new_driver()
    reg, rc_id = new_rc([a])
    s, r = activate(a, reg)
    assert s == 200, f"holder activation {s} {r}"
    age_account(a)
    return a, reg, rc_id


# ── cases ─────────────────────────────────────────────────────────────────────

def t_transfer_no_other_vehicle():
    print("\n[T1] transfer; previous holder has no other vehicle")
    a, reg, rc_id = holder_with_rc()
    b = new_driver()
    link(b, rc_id)
    n = expect_transfer("T1", a, b, reg, rc_id, has_other=False)
    check("T1", "new driver has no notice", profile(b).get("pendingRcTransferNotice") is None)
    return a, b, reg, rc_id, n


def t_ack(a, b, n):
    print("\n[T2] acknowledging the notice")
    s, _ = ack(a, str(uuid.uuid4()))
    check("T2", "unknown transferId -> 400", s == 400, s)
    s, _ = ack(b, n["id"])
    check("T2", "another driver's transferId -> 400", s == 400, s)
    check("T2", "notice still pending after rejected acks", profile(a).get("pendingRcTransferNotice") is not None)
    s, r = ack(a, n["id"])
    check("T2", "own transferId -> 200 Success", s == 200 and r.get("result") == "Success", f"{s} {r}")
    s, r = ack(a, n["id"])
    check("T2", "repeat ack -> 200 (idempotent)", s == 200, f"{s} {r}")
    check("T2", "profile pendingRcTransferNotice is null", profile(a).get("pendingRcTransferNotice") is None)


def t_ping_pong(a, b, reg, rc_id):
    print("\n[T3] same RC moves back B -> A")
    age_account(b)
    n = expect_transfer("T3", b, a, reg, rc_id, has_other=False)
    check("T3", "A still has only its one earlier notice", len(notices(a)) == 1, len(notices(a)))
    check("T3", "A has nothing pending", profile(a).get("pendingRcTransferNotice") is None)
    return n


def t_transfer_with_other_vehicle():
    print("\n[T4] transfer; previous holder has another valid RC -> 'vehicle removed' copy")
    a, reg, rc_id = holder_with_rc()
    reg2, rc2 = new_rc([a])
    b = new_driver()
    link(b, rc_id)
    expect_transfer("T4", a, b, reg, rc_id, has_other=True)
    print("\n[T5] holder activates their other vehicle -> notice clears without an ack")
    s, r = activate(a, reg2)
    check("T5", "holder activates their other RC", s == 200, f"{s} {r}")
    check("T5", "profile pendingRcTransferNotice is null", profile(a).get("pendingRcTransferNotice") is None)
    rows = notices(a)
    for _ in range(10):
        if rows and all(x["acked"] == "t" for x in rows):
            break
        time.sleep(1)
        rows = notices(a)
    check("T5", "notice row marked acknowledged", bool(rows) and all(x["acked"] == "t" for x in rows), str(rows))


def t_other_rc_invalid():
    print("\n[T6] previous holder's other RC is INVALID -> still counts as no other vehicle")
    a, reg, rc_id = holder_with_rc()
    new_rc([a], status="INVALID")
    b = new_driver()
    link(b, rc_id)
    expect_transfer("T6", a, b, reg, rc_id, has_other=False)


def t_vehicle_row_only():
    print("\n[T7] holder has the vehicle but no active RC link (vehicle-row branch)")
    a, reg, rc_id = holder_with_rc()
    deactivate_link(a, rc_id)
    b = new_driver()
    link(b, rc_id)
    expect_transfer("T7", a, b, reg, rc_id, has_other=False)


def t_old_ride_allows():
    print("\n[T8] holder's last ride is older than the cut-off -> transfer allowed (ride rule, not account age)")
    a = new_driver()  # account is new: only the ride rule can allow this
    reg, rc_id = new_rc([a])
    assert activate(a, reg)[0] == 200
    add_ride(a, 2 * CUT_OFF)
    b = new_driver()
    link(b, rc_id)
    expect_transfer("T8", a, b, reg, rc_id, has_other=False)


def t_already_active():
    print("\n[T9] driver re-activates the RC they already hold")
    a, reg, rc_id = holder_with_rc()
    s, r = activate(a, reg)
    check("T9", "200, nothing changes", s == 200 and has_vehicle(a) and rc_active_for(a, rc_id), f"{s} {r}")
    time.sleep(2)
    check("T9", "no notice", notices(a) == [])


def t_no_other_holder():
    print("\n[T10] first activation of an RC nobody holds")
    b = new_driver()
    reg, rc_id = new_rc([b])
    s, r = activate(b, reg)
    check("T10", "200 and vehicle linked", s == 200 and has_vehicle(b) and rc_active_for(b, rc_id), f"{s} {r}")


def rejections():
    codes = {}

    print("\n[R1] holder is on a ride")
    a, reg, rc_id = holder_with_rc()
    b = new_driver()
    link(b, rc_id)
    set_on_ride(a, True)
    codes["R1"] = expect_rejected("R1", a, b, reg, rc_id, "RC_OTHER_DRIVER_ON_RIDE")
    set_on_ride(a, False)

    print("\n[R2] holder took a ride inside the cut-off")
    a, reg, rc_id = holder_with_rc()
    add_ride(a, 1)
    b = new_driver()
    link(b, rc_id)
    codes["R2"] = expect_rejected("R2", a, b, reg, rc_id, "RC_OTHER_DRIVER_RECENT_RIDE")

    print("\n[R3] holder has no rides and an account younger than the cut-off")
    a = new_driver()
    reg, rc_id = new_rc([a])
    assert activate(a, reg)[0] == 200
    b = new_driver()
    link(b, rc_id)
    codes["R3"] = expect_rejected("R3", a, b, reg, rc_id, "RC_OTHER_DRIVER_ACCOUNT_NEW")

    print("\n[R4] new driver is on a ride")
    a, reg, rc_id = holder_with_rc()
    b = new_driver()
    link(b, rc_id)
    set_on_ride(b, True)
    codes["R4"] = expect_rejected("R4", a, b, reg, rc_id, "RC_ACTIVATION_DRIVER_ON_RIDE")
    set_on_ride(b, False)

    print("\n[R5] new driver is not enabled")
    a, reg, rc_id = holder_with_rc()
    b = new_driver(enabled=False)
    link(b, rc_id)
    codes["R5"] = expect_rejected("R5", a, b, reg, rc_id, "RC_ACTIVATION_DRIVER_NOT_ENABLED")

    print("\n[R6] RC is not approved")
    a, reg, rc_id = holder_with_rc()
    rc_set(rc_id, "approved = false", {"approved": False})
    b = new_driver()
    link(b, rc_id)
    codes["R6"] = expect_rejected("R6", a, b, reg, rc_id, "RC_ACTIVATION_RC_NOT_APPROVED")

    print("\n[R7] RC is not verified (verificationStatus != VALID)")
    a, reg, rc_id = holder_with_rc()
    rc_set(rc_id, "verification_status = 'INVALID'", {"verificationStatus": "INVALID"})
    b = new_driver()
    link(b, rc_id)
    codes["R7"] = expect_rejected("R7", a, b, reg, rc_id, "RC_ACTIVATION_RC_NOT_VALID")

    print("\n[R8] RC is not linked to the new driver (and they have no fleet)")
    a, reg, rc_id = holder_with_rc()
    b = new_driver()
    codes["R8"] = expect_rejected("R8", a, b, reg, rc_id, "RC_ACTIVATION_RC_NOT_LINKED")

    print("\n[R9] precedence: new driver's own problem is reported before the holder's")
    a, reg, rc_id = holder_with_rc()
    set_on_ride(a, True)
    b = new_driver(enabled=False)
    link(b, rc_id)
    expect_rejected("R9", a, b, reg, rc_id, "RC_ACTIVATION_DRIVER_NOT_ENABLED")
    set_on_ride(a, False)

    print("\n[R10] every rejection reason has its own errorCode")
    distinct = len(set(codes.values())) == len(codes)
    check("R10", "error codes are distinct", distinct, json.dumps(codes))


def preflight():
    if not PSQL:
        sys.exit("psql not found: set PSQL=/path/to/psql")
    if not REDIS_CLI:
        sys.exit("redis-cli not found: set REDIS_CLI=/path/to/redis-cli")
    if not MOC:
        sys.exit(f"No merchant_operating_city for {MERCHANT_SHORT}/{CITY}: import config first (dev/config-sync).")
    if sql(f"select to_regclass('{S}.rc_transfer_notice')") == "":
        sys.exit(f"{S}.rc_transfer_notice is missing: driver-app migrations have not run.")
    for key in ("RC_TRANSFERRED_NO_VEHICLE", "RC_TRANSFERRED_VEHICLE_REMOVED"):
        if overlay_row(key) is None:
            sys.exit(f"merchant_overlay {key} missing for {CITY}: apply dev/feature-migrations/0075-*.sql.")
    if log_lines("") is None:
        print(f"note: {LOG} not readable, skipping log / push assertions (set DRIVER_APP_LOG)")
    print(f"{MERCHANT_SHORT}/{CITY}  cut-off {CUT_OFF}s")


def main():
    preflight()
    a, b, reg, rc_id, n = t_transfer_no_other_vehicle()
    if n:
        t_ack(a, b, n)
        t_ping_pong(a, b, reg, rc_id)
    t_transfer_with_other_vehicle()
    t_other_rc_invalid()
    t_vehicle_row_only()
    t_old_ride_allows()
    t_already_active()
    t_no_other_holder()
    rejections()

    failed = [name for name, ok in results if not ok]
    print(f"\n{len(results) - len(failed)}/{len(results)} checks passed")
    if failed:
        print("failed:", *failed, sep="\n  - ")
        sys.exit(1)


if __name__ == "__main__":
    MOC = moc_id() if PSQL else ""
    CUT_OFF = cut_off_seconds() if MOC else 0
    main()
