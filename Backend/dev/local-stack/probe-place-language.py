#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Can the app tell the geocoder which language the rider reads?

This decides the shape of the Arabic place-names work, and neither the source
tree nor the documentation settles it.

The chain is app -> rider-app -> maps-shim, and the only per-request channel is
what the backend forwards: `input`, `location`, `radius`, `language`. There is
no second text field to smuggle a translation back in -- the rider-app's
AutoCompleteResp carries a single `description`, and the app's own Suggestion
type is `{description, placeId}`.

So everything hangs on `language`. Read out of the DEPLOYED binary (2026-09-09,
`grep -a` because `strings` is not installed on the box), its Language enum is:

    ENGLISH  HINDI  KANNADA  TAMIL  MALAYALAM

No FRENCH, no ARABIC. But an enum in the binary is not proof the FIELD is typed
as that enum -- it may well be plain Text on this request, in which case any
string reaches the shim and the honest design is available. Rebuilding the
backend to add a constructor is 45 minutes plus new binaries and a re-proof of
the ride flow, which is what this fork exists to avoid.

Three values are tried, and the third is the one that matters:

  ENGLISH   the value the app sends today -- the control
  ARABIC    what we would like to send
  XYZZY     nonsense. If ARABIC is accepted and this is refused, the field is
            an enum wider than the binary showed. If BOTH are accepted, the
            field is free text and we can say what we mean.

Signs in as the test rider from memory (never as a simulated driver: that
revokes the daemon's session and the car goes quiet for an hour).
"""
import json
import urllib.error
import urllib.request

BASE = "https://api.169-58-139-65.sslip.io"
RIDER, CC, OTP = "22778899", "+222", "111111"


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
aid = json.loads(raw)["authId"]

raw, code = call("POST", "/v2/auth/%s/verify" % aid,
                 {"otp": OTP, "deviceToken": "place-language-probe"})
assert code == 200, "verify -> %s %s" % (code, raw[:200])
TOK = json.loads(raw)["token"]
print("signed in as +%s %s\n" % (CC.lstrip('+'), RIDER))

# Nouakchott centre. "hop" matches Route de l'Espoir, which is the row whose
# Arabic name (طريق الأمل) we already hold and would want back.
QUERY = "Route"
NEAR = "18.0858,-15.9785"

ok = True
for language in ("ENGLISH", "ARABIC", "XYZZY"):
    raw, code = call("POST", "/v2/maps/autoComplete", {
        "input": QUERY,
        "location": NEAR,
        "radius": 50000,
        "language": language,
    }, TOK)

    if code == 200:
        preds = json.loads(raw).get("predictions", [])
        first = preds[0]["description"] if preds else "(no predictions)"
        print("  %-8s -> 200, %d results, first: %s" % (language, len(preds), first))
    else:
        print("  %-8s -> %s  %s" % (language, code, raw[:160]))
        if language == "ENGLISH":
            ok = False

print()
print("Read it like this:")
print("  ARABIC 200 and XYZZY 200 -> free text. Send the real language.")
print("  ARABIC 200 and XYZZY 400 -> a wider enum than the binary showed.")
print("  ARABIC 400               -> the field is the 5-value enum. The")
print("                              language has to travel as one of those,")
print("                              or not through the backend at all.")
raise SystemExit(0 if ok else 1)
