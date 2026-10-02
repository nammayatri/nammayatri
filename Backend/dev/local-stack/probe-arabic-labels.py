#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Two questions the app cannot answer for itself.

1. Does the place_id the BACKEND hands the phone match the place_id the shim
   knows? The label lookup is keyed on it. If the backend rewrites or caches
   ids -- it keeps its own `atlas_app.place_name_cache` -- every lookup asks
   for ids that do not exist and returns an empty map, which looks exactly like
   "the feature does not work" and nothing logs an error.

2. Does typing ARABIC find anything? `geo.place.search_norm` is built from the
   display name plus alt_names, so it should. Whether `geo.normalise` leaves
   Arabic intact is a different question, and only the server can answer it.

Signs in as the test rider. Never as a simulated driver.
"""
import json
import urllib.error
import urllib.request

BASE = "https://api.169-58-139-65.sslip.io"
RIDER, CC, OTP = "22778899", "+222", "111111"
NEAR = "18.0858,-15.9785"


def call(method, path, body=None, tok=None):
    data = json.dumps(body).encode() if body is not None else None
    q = urllib.request.Request(BASE + path, data=data, method=method)
    q.add_header("content-type", "application/json")
    if tok:
        q.add_header("token", tok)
    try:
        with urllib.request.urlopen(q, timeout=25) as r:
            return r.read().decode(), r.status
    except urllib.error.HTTPError as e:
        return e.read().decode(), e.code
    except Exception as e:
        return str(e), 0


raw, code = call("POST", "/v2/auth",
                 {"mobileNumber": RIDER, "mobileCountryCode": CC, "merchantId": "YATRI"})
assert code == 200, "auth -> %s %s" % (code, raw[:200])
raw, code = call("POST", "/v2/auth/%s/verify" % json.loads(raw)["authId"],
                 {"otp": OTP, "deviceToken": "arabic-label-probe"})
assert code == 200, "verify -> %s %s" % (code, raw[:200])
TOK = json.loads(raw)["token"]
print("signed in\n")


def suggest(text):
    raw, code = call("POST", "/v2/maps/autoComplete", {
        "input": text, "location": NEAR, "radius": 50000, "language": "ENGLISH",
    }, TOK)
    if code != 200:
        return None, "%s %s" % (code, raw[:160])
    return json.loads(raw).get("predictions", []), None


# ── 1. the ids ────────────────────────────────────────────────────────────
print("── the RAW shape of one prediction ──")
_raw, _ = call("POST", "/v2/maps/autoComplete", {
    "input": "Rue", "location": NEAR, "radius": 50000, "language": "ENGLISH",
}, TOK)
_p = json.loads(_raw).get("predictions", [])
print("  keys: %s" % (sorted(_p[0].keys()) if _p else "(none)"))
print("  first: %s" % json.dumps(_p[0], ensure_ascii=False) if _p else "")
print()

print("── what the BACKEND returns for 'Rue' ──")
preds, err = suggest("Rue")
if err:
    raise SystemExit("  autoComplete failed: %s" % err)
for p in preds[:5]:
    print("  %-46s %s" % (p.get("description", "")[:45], p.get("placeId") or p.get("place_id")))

ids = [p.get("placeId") or p.get("place_id") for p in preds]
ids = [i for i in ids if i]
print()
print("── the same ids, asked of the shim through the edge ──")
if not ids:
    print("  no ids to ask about")
else:
    q = ",".join(ids)
    raw, code = call("GET", "/place/labels/json?lang=ar&ids=" + urllib.parse.quote(q))
    print("  http %s" % code)
    body = json.loads(raw) if code == 200 else {}
    labels = body.get("labels", {})
    print("  asked %d, got %d back" % (len(ids), len(labels)))
    for i in ids[:5]:
        print("    %-22s %s" % (i, labels.get(i, "-- not found --")))

# ── 2. typing Arabic ──────────────────────────────────────────────────────
print()
print("── typing Arabic ──")
for text in ("شارع", "نواكشوط", "تفرغ"):
    preds, err = suggest(text)
    if err:
        print("  %-10s -> %s" % (text, err))
    else:
        first = preds[0]["description"] if preds else "(nothing)"
        print("  %-10s -> %d results, first: %s" % (text, len(preds), first))
