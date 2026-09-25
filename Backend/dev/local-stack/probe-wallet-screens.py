#!/usr/bin/env python3
"""Does the wallet server answer what the driver screens actually read?

Run it ON the VPS:

    python3 probe-wallet-screens.py

`wallet.js` and `src/lib/wallet.ts` were written against each other, which
proves nothing: two files written in one sitting agree about a typo as readily
as about a contract. This asserts the field *names and types* the screens parse,
against the deployed server, with a real driver's token.

── The one invariant worth more than the rest ──────────────────────────────
`canWork` must equal `dayActive || balance >= dayPrice`. The client is
forbidden from recomputing it — see the header of `lib/wallet.ts` — precisely so
that the screen and the dispatch pool cannot form two opinions. That is only
safe while the server's answer really is that expression, so it is checked here
rather than trusted.

── It cleans up after itself ───────────────────────────────────────────────
The checkout it opens is deleted. Left behind, it would sit in that driver's own
*Historique* as a payment he never started. The session itself stays at Moosyl,
in test mode, costing nothing.
"""
import json
import subprocess
import sys
import urllib.error
import urllib.request

BASE = "https://api.169-58-139-65.sslip.io"

passed = 0
failed = 0


def check(name, ok, detail=""):
    global passed, failed
    if ok:
        passed += 1
        print(f"  PASS  {name}")
    else:
        failed += 1
        print(f"  FAIL  {name}   {detail}")


def psql(sql):
    r = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres",
         "-d", "atlas_dev", "-tAc", sql],
        capture_output=True, text=True, timeout=60)
    return r.stdout.strip()


def request(path, method="GET", token=None):
    q = urllib.request.Request(BASE + path, method=method)
    if token:
        q.add_header("token", token)
    q.add_header("content-type", "application/json")
    try:
        with urllib.request.urlopen(q, timeout=30) as r:
            return r.status, r.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode("utf-8", "replace")
    except Exception as e:                                   # noqa: BLE001
        return 0, str(e)


def body_of(text):
    try:
        return json.loads(text)
    except ValueError:
        return {}


# ── a driver who is really signed in ────────────────────────────────────────
print("== a real driver")
row = psql(
    "SELECT rt.token || '|' || rt.entity_id || '|' || coalesce(p.first_name,'?') "
    "  FROM atlas_driver_offer_bpp.registration_token rt "
    "  JOIN atlas_driver_offer_bpp.person p ON p.id = rt.entity_id "
    " WHERE p.role = 'DRIVER' AND rt.verified "
    " ORDER BY rt.created_at DESC LIMIT 1")
if not row or "|" not in row:
    print("   no verified driver token in the database - run ./setup.sh drivers")
    sys.exit(2)

token, driver_id, name = row.split("|", 2)
print(f"   {name}  {driver_id}")


# ── 1. /wallet/status, field by field ───────────────────────────────────────
print("\n== GET /wallet/status")
code, text = request("/wallet/status", token=token)
check("his token is accepted", code == 200, f"HTTP {code} {text[:120]}")
s = body_of(text)

# Every one of these is read by a screen. A missing field is not a crash in
# JavaScript -- it is a zero, silently, which is how a driver with 300 MRU is
# shown an empty wallet.
for field, kind in (("balance", int), ("currency", str), ("dayPrice", int),
                    ("minTopup", int), ("dayActive", bool), ("canWork", bool),
                    ("configured", bool)):
    got = s.get(field)
    # bool is a subclass of int in Python; check it first or every bool passes
    # as an int and the two real integers go unchecked.
    ok = isinstance(got, kind) and (kind is not int or not isinstance(got, bool))
    check(f"{field} is {kind.__name__}", ok, f"got {got!r}")

check("dayUntil is a string or null",
      s.get("dayUntil") is None or isinstance(s.get("dayUntil"), str),
      f"got {s.get('dayUntil')!r}")
check("currency is MRU", s.get("currency") == "MRU", f"got {s.get('currency')!r}")
check("the day price is a whole number of MRU",
      isinstance(s.get("dayPrice"), int) and s.get("dayPrice") > 0)
check("dayUntil is present exactly when a day is running",
      bool(s.get("dayActive")) == (s.get("dayUntil") is not None),
      f"dayActive={s.get('dayActive')} dayUntil={s.get('dayUntil')!r}")

# THE invariant. See the docstring.
expected = bool(s.get("dayActive")) or (s.get("balance", 0) >= s.get("dayPrice", 1))
check("canWork == dayActive OR balance >= dayPrice",
      s.get("canWork") == expected,
      f"server said {s.get('canWork')}, expression gives {expected}")

print(f"   solde {s.get('balance')} {s.get('currency')} - journee {s.get('dayActive')} "
      f"- peut travailler {s.get('canWork')} - passerelle {s.get('configured')}")


# ── 2. /wallet/history ──────────────────────────────────────────────────────
print("\n== GET /wallet/history")
code, text = request("/wallet/history", token=token)
check("history is served", code == 200, f"HTTP {code} {text[:120]}")
h = body_of(text)
check("entries is a list", isinstance(h.get("entries"), list), f"got {type(h.get('entries'))}")
check("the balance matches /wallet/status",
      h.get("balance") == s.get("balance"),
      f"history {h.get('balance')} vs status {s.get('balance')}")

entries = h.get("entries") or []
if entries:
    e = entries[0]
    for field in ("kind", "amount", "at"):
        check(f"an entry carries {field}", field in e, f"keys: {sorted(e)}")
    check("kind is one the screen draws",
          e.get("kind") in ("topup", "day", "adjustment"), f"got {e.get('kind')!r}")
    check("amount is signed the way the ledger says",
          isinstance(e.get("amount"), int) and not isinstance(e.get("amount"), bool),
          f"got {e.get('amount')!r}")
    # The screen adds these up in the reader's head against the figure on top.
    total = sum(x.get("amount", 0) for x in entries)
    check("the entries sum to the balance",
          total == h.get("balance"),
          f"sum {total} vs balance {h.get('balance')} "
          f"({len(entries)} entries, capped at 100 by the server)")
    print(f"   {len(entries)} mouvement(s), le plus recent: "
          f"{e.get('kind')} {e.get('amount')}")
else:
    print("   no movements yet for this driver - shape checks skipped")


# ── 3. the refusals the top-up screen has copy for ──────────────────────────
print("\n== the refusals")
code, text = request("/wallet/topup?amount=1", method="POST", token=token)
b = body_of(text)
check("1 MRU is refused as too small", code == 400, f"HTTP {code} {text[:120]}")
check("the refusal names the minimum",
      isinstance(b.get("minTopup"), int) and b.get("minTopup") == s.get("minTopup"),
      f"got {b.get('minTopup')!r}, status said {s.get('minTopup')!r}")

code, _ = request("/wallet/topup/movin-nosuch-0", token=token)
check("an unknown transaction is 404, not 500", code == 404, f"HTTP {code}")

code, _ = request("/wallet/status", token="not-a-real-token")
check("a bad token is refused", code == 401, f"HTTP {code}")


# ── 4. a real checkout, then cleaned up ─────────────────────────────────────
print("\n== POST /wallet/topup")
amount = s.get("dayPrice", 30)
code, text = request(f"/wallet/topup?amount={amount}", method="POST", token=token)
made = body_of(text)
check("a checkout is created", code == 200, f"HTTP {code} {text[:160]}")
if code == 200:
    for field in ("transactionId", "url", "amount", "currency"):
        check(f"it carries {field}", bool(made.get(field)), f"keys: {sorted(made)}")
    check("the amount echoes what was asked", made.get("amount") == amount,
          f"asked {amount}, got {made.get('amount')}")
    check("the URL is https",
          isinstance(made.get("url"), str) and made["url"].startswith("https://"),
          f"got {made.get('url')!r}")

    tid = made.get("transactionId")
    if tid:
        # The screen polls this the moment he comes back. An open session must
        # read as not-yet-credited, never as failed -- `unseen`, not `failed`.
        code, text = request(f"/wallet/topup/{tid}", token=token)
        p = body_of(text)
        check("the new checkout can be polled", code == 200, f"HTTP {code} {text[:120]}")
        check("it is not credited", p.get("credited") is False, f"got {p.get('credited')!r}")
        check("its status is pending, not failed",
              p.get("status") == "pending", f"got {p.get('status')!r}")
        check("polling it did not move the balance",
              p.get("balance") == s.get("balance"),
              f"{p.get('balance')} vs {s.get('balance')}")

        # `psql -tAc` on a DELETE ... RETURNING prints the returned row AND
        # the "DELETE 1" status line, so the whole output never equals the id.
        # Comparing them reported a failed cleanup while the row was in fact
        # gone -- a false alarm on the one check whose job is to notice a
        # phantom payment left sitting in a driver's own history.
        gone = psql("DELETE FROM movin.wallet_topup WHERE transaction_id = "
                    f"'{tid}' RETURNING transaction_id").splitlines()[0].strip()
        check("the test checkout is cleaned up", gone == tid, f"deleted {gone!r}")

        left = psql("SELECT count(*) FROM movin.wallet_entry WHERE topup_id = "
                    f"'{tid}'")
        check("it never created a ledger entry", left == "0", f"found {left}")


print(f"\n{passed} passed, {failed} failed")
sys.exit(1 if failed else 0)
