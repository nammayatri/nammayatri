#!/usr/bin/env python3
"""Why does `GET /ui/message/list` answer 500 as soon as it has a row?

Run it ON the VPS:

    python3 probe-agency-messages.py

── What this is for ────────────────────────────────────────────────────────
The driver app has a whole messagerie -- a list, a message, and `response`,
which is the **only route in the entire driver API that carries a driver's own
words back to the office**. The three tables exist. The agency has nothing that
writes to them, because upstream writes them from a dashboard we do not run.

Before building that sending screen into the admin console it is worth knowing
whether the driver would ever see the message. On 2026-08-24 the answer was no:
empty the route answers `200 []`, and with a single seeded row it answered
`500` with no detail. That measurement did not say *which* column was wrong,
which is the difference between "one INSERT away" and "needs a rebuild".

So this walks the row up one field at a time and reads the container's own log
after each attempt, rather than guessing.

── It puts the database back ───────────────────────────────────────────────
Every row it writes carries a `probe-` id and is deleted at the end, including
after a failure. The last check re-reads the route and asserts it is `200 []`
again, so a dirty database cannot be mistaken for a working one -- or left
behind for the next person.

Nothing here restarts a container, changes config, or writes to any table the
product uses. `message`, `message_translation` and `message_report` have been
empty since this stack existed.
"""
import json
import subprocess
import sys
import urllib.error
import urllib.request
import uuid

D = "http://localhost:8017"
MSG_ID = "probe-msg-" + uuid.uuid4().hex[:12]


def pg(sql):
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", "atlas_dev",
         "-At", "-c", sql], capture_output=True, text=True, timeout=60)
    if out.returncode != 0:
        return "ERR " + out.stderr.strip().splitlines()[0] if out.stderr.strip() else "ERR"
    return out.stdout.strip()


def call(url, method="GET", body=None, token=None):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, method=method)
    req.add_header("content-type", "application/json")
    if token:
        req.add_header("token", token)
    try:
        with urllib.request.urlopen(req, timeout=25) as r:
            return r.status, r.read().decode()
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode()
    except Exception as exc:
        return 0, str(exc)


def driver_log_tail(n=6):
    """What the binary said about the request we just made.

    A 500 from this backend is a bare `INTERNAL_ERROR` on the wire; the reason
    only ever exists in the container's own log.
    """
    out = subprocess.run(["docker", "logs", "--tail", str(n * 12), "ny-driver"],
                         capture_output=True, text=True, timeout=60)
    lines = (out.stdout + out.stderr).splitlines()
    keep = [l for l in lines
            if any(w in l for w in ("Error", "error", "Exception", "message", "Message"))]
    return keep[-n:] if keep else lines[-n:]


def cleanup():
    pg(f"DELETE FROM atlas_driver_offer_bpp.message_report WHERE message_id = '{MSG_ID}'")
    pg(f"DELETE FROM atlas_driver_offer_bpp.message_translation WHERE message_id = '{MSG_ID}'")
    pg(f"DELETE FROM atlas_driver_offer_bpp.message WHERE id = '{MSG_ID}'")


print("=" * 74)
print("AGENCY MESSAGES — what the list route actually chokes on")
print("=" * 74)

# ── the tables must be empty, or nothing below means anything ────────────────
before = pg("SELECT count(*) FROM atlas_driver_offer_bpp.message")
if before != "0":
    sys.exit(f"`message` already has {before} rows — refusing to run. "
             "This probe only reasons correctly from an empty table.")

# ── a driver to be, with his own OTP read from the backend's own table ───────
row = pg("SELECT p.id || '|' || p.unencrypted_mobile_number || '|' || "
         "coalesce(p.language::text,'NULL') || '|' || p.merchant_id "
         "FROM atlas_driver_offer_bpp.person p "
         "WHERE p.role = 'DRIVER' AND p.unencrypted_mobile_number IS NOT NULL "
         "ORDER BY p.created_at DESC LIMIT 1")
if not row or row.startswith("ERR"):
    sys.exit(f"no driver to test with: {row}")
DRIVER_ID, PHONE, LANG, MERCHANT = row.split("|")
print(f"\ndriver   {DRIVER_ID}")
print(f"phone    {PHONE}   language {LANG}")
print(f"merchant {MERCHANT}")

print("\n--- signing him in (straight at the backend, past the guard)")
code, raw = call(f"{D}/ui/auth", "POST",
                 {"mobileCountryCode": "+213", "mobileNumber": PHONE,
                  "merchantId": "favorit0-0000-0000-0000-00000favorit"})
try:
    auth_id = json.loads(raw)["authId"]
except Exception:
    sys.exit(f"auth failed {code}: {raw[:200]}")

otp = pg(f"SELECT auth_value_hash FROM atlas_driver_offer_bpp.registration_token "
         f"WHERE id = '{auth_id}'")
code, raw = call(f"{D}/ui/auth/{auth_id}/verify", "POST",
                 {"otp": otp, "deviceToken": "agency-message-probe"})
try:
    TOKEN = json.loads(raw)["token"]
except Exception:
    sys.exit(f"verify failed {code}: {raw[:200]}")
print(f"  ok, otp {otp}")

print("\n--- baseline: the route with nothing to return")
code, raw = call(f"{D}/ui/message/list?limit=20&offset=0", token=TOKEN)
print(f"  {code}  {raw[:100]}")
if code != 200:
    cleanup()
    sys.exit("the empty case is already broken — stop, this is a different bug")


def attempt(name, message_sql, report_sql, translation_sql=None):
    """Seed one shape, ask the route, print what happened, then clear it."""
    cleanup()
    for sql in (message_sql, translation_sql, report_sql):
        if sql:
            err = pg(sql)
            if err.startswith("ERR"):
                print(f"\n  {name}\n    INSERT refused: {err}")
                return None
    code, raw = call(f"{D}/ui/message/list?limit=20&offset=0", token=TOKEN)
    mark = "\033[1;32mok \033[0m" if code == 200 else "\033[1;31mBAD\033[0m"
    print(f"\n  {mark} {name}\n      {code}  {raw[:150]}")
    if code != 200:
        for line in driver_log_tail(4):
            print(f"      log| {line[:150]}")
    return code


MSG = ("INSERT INTO atlas_driver_offer_bpp.message "
       "(id, type, title, description, media_files, merchant_id, created_at, label) "
       "VALUES ('{id}', '{type}', 'Message de test', 'Ceci est un essai.', {media}, "
       "'{merchant}', now(), {label})")

REP = ("INSERT INTO atlas_driver_offer_bpp.message_report "
       "(message_id, driver_id, delivery_status, read_status, reply, "
       " message_dynamic_fields, created_at, updated_at) "
       "VALUES ('{id}', '{driver}', '{status}', false, NULL, {dyn}, now(), now())")

TRA = ("INSERT INTO atlas_driver_offer_bpp.message_translation "
       "(message_id, language, title, description, created_at, label) "
       "VALUES ('{id}', '{lang}', 'Message de test', 'Ceci est un essai.', now(), NULL)")


def msg(type_="Read", media="'{}'", label="NULL"):
    return MSG.format(id=MSG_ID, type=type_, media=media, merchant=MERCHANT, label=label)


def rep(status="Success", dyn="'{}'::json"):
    return REP.format(id=MSG_ID, driver=DRIVER_ID, status=status, dyn=dyn)


def tra(lang="FRENCH"):
    return TRA.format(id=MSG_ID, lang=lang)


print("\n--- one field at a time")
attempt("media_files '{}', type Read, status Success, no translation",
        msg(), rep())
attempt("media_files NULL", msg(media="NULL"), rep())
attempt("with a FRENCH translation", msg(), rep(), tra("FRENCH"))
attempt("with an ENGLISH translation", msg(), rep(), tra("ENGLISH"))
attempt("delivery_status Queued", msg(), rep(status="Queued"))
attempt("message_dynamic_fields NULL", msg(), rep(dyn="NULL"))
attempt("type Action", msg(type_='Action "Ouvrir"'), rep())
attempt("label set", msg(label="'ANNONCE'"), rep())

print("\n--- putting it back")
cleanup()
code, raw = call(f"{D}/ui/message/list?limit=20&offset=0", token=TOKEN)
left = pg("SELECT count(*) FROM atlas_driver_offer_bpp.message")
print(f"  route {code} {raw[:60]}   rows left in `message`: {left}")
if code != 200 or left != "0":
    sys.exit("DID NOT CLEAN UP — fix this before walking away")
print("  clean")
