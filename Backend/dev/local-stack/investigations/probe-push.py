#!/usr/bin/env python3
"""Send one push to a real phone, without needing a ride.

    ./probe-push.py                      # newest real device token
    ./probe-push.py <personId>           # a specific rider
    ./probe-push.py --type TRIP_STARTED  # a different event

── Why this exists ─────────────────────────────────────────────────────────
Testing push through the product means booking a ride, waiting for a driver to
be assigned, and hoping. That is a five-minute round trip for one bit of
information, and it confuses two questions that need separating:

    does the SERVER send?          -- answered by the rider log
    does the PHONE draw it?        -- answered only on the phone

This sends the **byte-identical payload** the backend sends, straight to a
device token, so the phone side can be tested on its own and repeatedly. Run it
with `adb logcat -s ReactNativeJS:V` open and the answer arrives in one second.

The payload below is copied from an actual send captured in the rider log on
2026-08-18, including the parts that look wrong and are: `channel_id: General`
is ignored by the app because these are data-only messages that the app draws
itself, and the icon URL points at `localhost:8080`, which no phone can reach.

Runs ON THE VPS: it needs the service-account key and the database.
"""
import argparse
import json
import subprocess
import sys
import time
import urllib.error
import urllib.request

import jwt  # pyjwt

SA_PATH = "/tmp/sa.json"
SCOPE = "https://www.googleapis.com/auth/firebase.messaging"


def pg(sql):
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres",
         "-d", "atlas_dev", "-At", "-c", sql],
        capture_output=True, text=True, timeout=60)
    return out.stdout.strip()


def access_token(sa):
    """Mint a JWT with the service-account key and trade it for a bearer token.

    Exactly what the backend does -- and the step that failed for the whole life
    of this deployment, because upstream's placeholder key is the string
    'xxxxxxx' and produced 'Bad RSA key!' on every send.
    """
    now = int(time.time())
    claim = {
        "iss": sa["client_email"],
        "scope": SCOPE,
        "aud": sa["token_uri"],
        "iat": now,
        "exp": now + 3600,
    }
    assertion = jwt.encode(claim, sa["private_key"], algorithm="RS256")
    body = urllib.parse.urlencode({
        "grant_type": "urn:ietf:params:oauth:grant-type:jwt-bearer",
        "assertion": assertion,
    }).encode()
    req = urllib.request.Request(sa["token_uri"], data=body, method="POST")
    req.add_header("content-type", "application/x-www-form-urlencoded")
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.loads(r.read().decode())["access_token"]


def main():
    p = argparse.ArgumentParser()
    p.add_argument("person", nargs="?", help="rider id; default is the newest real token")
    p.add_argument("--type", default="DRIVER_ASSIGNMENT",
                   help="notification_type to send (default DRIVER_ASSIGNMENT)")
    args = p.parse_args()

    sa = json.load(open(SA_PATH))

    # A real FCM token is long and contains a colon. The 32-character hex values
    # in this column are the placeholder ids lib/device.ts minted while push was
    # believed impossible, and Firebase rejects every one of them.
    where = (f"id = '{args.person}'" if args.person
             else "device_token like '%:%' and length(device_token) > 100")
    row = pg(f"select id, coalesce(first_name,'?'), device_token "
             f"from atlas_app.person where {where} "
             f"order by updated_at desc limit 1;")
    if not row:
        sys.exit("no rider with a real FCM token -- open the app and sign in first")

    pid, name, token = row.split("|")
    print(f"to      : {name} ({pid[:8]})")
    print(f"token   : {token[:24]}… ({len(token)} chars)")
    print(f"type    : {args.type}\n")

    bearer = access_token(sa)
    print("oauth   : got a bearer token from Google")

    # Copied from a real send. The app reads `notification_type` and writes its
    # own French words, so the English in notification_json is never displayed --
    # it is kept here only so this stays a faithful replica.
    inner = json.dumps({
        "title": "Driver assigned!",
        "body": "Karim will be your driver for this trip.",
        "icon": "http://localhost:8080/static/images/ride-success.png",
        "sound": "default",
        "tag": args.type,
        "channel_id": "General",
    })
    data = {
        "entity_type": "Product",
        "notification_json": inner,
        "entity_ids": "probe-push",
        "entity_data": "[]",
        "notification_type": args.type,
        "show_notification": "true",
    }
    message = {"message": {"token": token, "android": {"data": data}}}

    url = f"https://fcm.googleapis.com/v1/projects/{sa['project_id']}/messages:send"
    req = urllib.request.Request(url, data=json.dumps(message).encode(), method="POST")
    req.add_header("content-type", "application/json")
    req.add_header("authorization", f"Bearer {bearer}")
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            print(f"fcm     : {r.status} {r.read().decode()[:200]}")
            print("\nAccepted by Google. Anything after this is the phone's side:")
            print("  adb logcat -s ReactNativeJS:V   ->  look for [push]")
    except urllib.error.HTTPError as e:
        print(f"fcm     : {e.code} {e.read().decode()[:400]}")
        print("\nRejected. A 404 means fcm_url/project is wrong; INVALID_ARGUMENT")
        print("on message.token means that token is not a real FCM token.")


if __name__ == "__main__":
    import urllib.parse  # noqa: E402  (used by access_token)
    main()
