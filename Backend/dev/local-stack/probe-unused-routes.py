#!/usr/bin/env python3
"""What can the rider API do that the app does not use yet?

"What else should we build" is usually answered from what other ride apps have.
That produces a list the backend cannot serve. This answers it from what THIS
binary can actually serve, which is a shorter and much more useful list.

Runs ON the VPS: the rider API is loopback 8013.

MEASURED 2026-08-16, against the deployed binary (MANIFEST ref 03a7531):

    41 rider-facing routes, 20 used by screens 1-14, 21 unused.

The five that changed the roadmap:

  * /v2/frontend/flowStatus      the server knows a rider is mid-ride, and says
                                 so in 0.27s. Recovering an interrupted ride is
                                 therefore a launch check, not a design problem.
                                 A rider lost a trip to this during testing.
  * /v2/savedLocation            Home/Work exist server-side -- but see
                                 probe-rider-extras.py, the address text is
                                 discarded and only tag/lat/lon survive.
  * /v2/serviceability/destination  we only ever check the ORIGIN. A rider can
                                 pick a destination outside Algeria and only
                                 find out at the price screen.
  * /v2/auth/logout              there is no sign-out in the app today.
  * /v2/support/sendIssue        present but broken; complaints reach nobody.

And the one that is NOT there: **no push route exists on the rider API at all.**
There is an `FCMConfigUpdateReq` schema with no endpoint behind it.

**Do not read that as "this backend cannot send push" -- that conclusion was
drawn here and it was wrong.** There is no route because the app does not need
one: `Kernel.External.FCM.Flow` is compiled into the running binary and has been
*trying* on every ride, failing on a placeholder key ("Bad RSA key!" in the rider
log). The config is a row on `atlas_app.merchant`, device tokens are already
collected, and nine message types already exist. See the push section of the
local-stack README.
"""
import json
import subprocess

o = subprocess.run(["curl", "-s", "-m", "30", "http://localhost:8013/openapi"],
                   capture_output=True, text=True, timeout=40).stdout
d = json.loads(o)
paths = d["paths"]

# Everything screens 1-14 already call.
USED = {
    "/v2/auth", "/v2/auth/{authId}/verify", "/v2/auth/otp/{authId}/resend",
    "/v2/profile",
    "/v2/serviceability/origin", "/v2/maps/autoComplete", "/v2/maps/getPlaceName",
    "/v2/maps/getPlaceDetails",
    "/v2/rideSearch", "/v2/rideSearch/{searchId}/results",
    "/v2/estimate/{estimateId}/select", "/v2/estimate/{estimateId}/quotes",
    "/v2/estimate/{estimateId}/cancel",
    "/v2/rideSearch/quotes/{quoteId}/confirm",
    "/v2/rideBooking/{rideBookingId}", "/v2/rideBooking/{rideBookingId}/cancel",
    "/v2/rideBooking/list",
    "/v2/ride/{rideId}/driver/location",
    "/v2/cancellationReason/list", "/v2/feedback/rateRide",
}

# Not ours to call: BECKN callbacks, staff tooling, health.
def skip(p):
    return (p.startswith("/cab/") or p.startswith("/dashboard/")
            or p.startswith("/v2/customerSupport") or p in ("/v2", "/openapi"))


rider = [p for p in sorted(paths) if not skip(p)]
unused = [p for p in rider if p not in USED]

print(f"rider-facing routes: {len(rider)}   used by the app: {len(rider)-len(unused)}"
      f"   unused: {len(unused)}\n")

print("=== what we are not using")
for p in unused:
    ms = ",".join(m.upper() for m in paths[p] if m in ("get", "post", "put", "delete"))
    print(f"  {ms:9} {p}")

print("\n=== does anything support saved places, scheduling, referrals or push?")
comps = d.get("components", {}).get("schemas", {})
for word in ("saved", "favourite", "favorite", "schedule", "later", "rental",
             "referral", "coupon", "promo", "notification", "fcm", "device",
             "emergency", "sos", "contact"):
    hits = [p for p in rider if word in p.lower()]
    schemas = [k for k in comps if word in k.lower()]
    if hits or schemas:
        print(f"  {word:14} routes={hits or '-'}  schemas={schemas[:4] or '-'}")

print("\n=== the fare product types this binary knows")
for k in comps:
    if "fareproduct" in k.lower() or k in ("FareProductType", "SearchReqLocation"):
        print(f"  {k}: {json.dumps(comps[k], default=str)[:220]}")
