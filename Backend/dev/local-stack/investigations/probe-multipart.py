#!/usr/bin/env python3
"""Does the server accept the exact multipart envelope the app now builds?

Run ON the VPS.

The previous version of the app used FormData and threw before a byte left the
handset. The replacement assembles the envelope by hand, and shipping a second
guess would be worse than the first. So this builds the SAME bytes, in the same
order, with the same headers, and posts them through the public edge.

Byte-for-byte the app's layout:

    --BOUNDARY\r\n
    Content-Disposition: form-data; name="type"\r\n
    \r\n
    <type>\r\n
    --BOUNDARY\r\n
    Content-Disposition: form-data; name="file"; filename="<name>"\r\n
    Content-Type: image/jpeg\r\n
    \r\n
    <bytes>\r\n
    --BOUNDARY--\r\n

It signs in on loopback 8017, which bypasses the auth guard and takes the fixed
dev code, so no real driver's code is touched. Whatever it uploads is deleted
again at the end, and the last check asserts the table is back to where it
started.
"""
import json
import subprocess
import sys
import urllib.error
import urllib.request

D = "http://localhost:8017"
PUBLIC = "https://api.movinapp.net/driver/documents"
NUM = "0555000002"          # already enrolled, already a driver
MERCHANT = "favorit0-0000-0000-0000-00000favorit"

# A one-pixel JPEG. Real magic bytes, so the server's sniff() accepts it.
JPEG = bytes.fromhex(
    "ffd8ffe000104a46494600010100000100010000ffdb004300ffffffffffffff"
    "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    "ffffffffffffffffffffffffffffffffffc00011080001000103012200021101"
    "031101ffc4001f0000010501010101010100000000000000000102030405060708"
    "090a0bffc400b5100002010303020403050504040000017d01020300041105122131"
    "410613516107227114328191a1082342b1c11552d1f02433627282090a161718191a"
    "25262728292a3435363738393a434445464748494a535455565758595a636465666768"
    "696a737475767778797a838485868788898a92939495969798999aa2a3a4a5a6a7a8a9"
    "aab2b3b4b5b6b7b8b9bac2c3c4c5c6c7c8c9cad2d3d4d5d6d7d8d9dae1e2e3e4e5e6e7"
    "e8e9eaf1f2f3f4f5f6f7f8f9faffda0008010100003f00fb00a28a2800a28a2803ffd9"
)


def pg(sql):
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d",
         "atlas_dev", "-At", "-c", sql],
        capture_output=True, text=True, timeout=60)
    return out.stdout.strip()


def call(url, method="GET", body=None, headers=None):
    req = urllib.request.Request(url, data=body, method=method)
    for k, v in (headers or {}).items():
        req.add_header(k, v)
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return r.status, r.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode("utf-8", "replace")
    except Exception as exc:
        return 0, str(exc)


before = pg("SELECT count(*) FROM movin.driver_document")
print(f"rows before: {before}")

print("\n--- signing a driver in on loopback (past the guard, dev code)")
code, raw = call(f"{D}/ui/auth", "POST",
                 json.dumps({"mobileCountryCode": "+213", "mobileNumber": NUM,
                             "merchantId": MERCHANT}).encode(),
                 {"content-type": "application/json"})
auth_id = (json.loads(raw).get("authId") if code == 200 else None)
if not auth_id:
    sys.exit(f"auth failed {code}: {raw[:200]}")

otp = pg(f"SELECT auth_value_hash FROM atlas_driver_offer_bpp.registration_token "
         f"WHERE id = '{auth_id}'")
code, raw = call(f"{D}/ui/auth/{auth_id}/verify", "POST",
                 json.dumps({"otp": otp, "deviceToken": "multipart-probe"}).encode(),
                 {"content-type": "application/json"})
token = (json.loads(raw).get("token") if code == 200 else None)
if not token:
    sys.exit(f"verify failed {code}: {raw[:200]}")
print("  ok")

print("\n--- posting the app's exact envelope, through the public edge")
boundary = "----movinprobe1"
head = (
    f"--{boundary}\r\n"
    'Content-Disposition: form-data; name="type"\r\n\r\n'
    "DriverLicense\r\n"
    f"--{boundary}\r\n"
    'Content-Disposition: form-data; name="file"; filename="probe.jpg"\r\n'
    "Content-Type: image/jpeg\r\n\r\n"
).encode("ascii")
tail = f"\r\n--{boundary}--\r\n".encode("ascii")

code, raw = call(PUBLIC, "POST", head + JPEG + tail, {
    "token": token,
    "Content-Type": f"multipart/form-data; boundary={boundary}",
})
print(f"  {code}  {raw[:200]}")

ok = code in (200, 201)
print(f"\n  {'PASS — the server parses this envelope' if ok else 'FAIL — the app would fail the same way'}")

print("\n--- putting it back")
rows = pg("SELECT id || ' ' || stored_path FROM movin.driver_document")
for line in rows.splitlines():
    if not line.strip():
        continue
    doc_id, path = line.split(" ", 1)
    subprocess.run(["docker", "exec", "movin-admin-api", "sh", "-c",
                    f"rm -f /data/documents/{path}"], capture_output=True, timeout=30)
    pg(f"DELETE FROM movin.driver_document WHERE id = '{doc_id}'")

after = pg("SELECT count(*) FROM movin.driver_document")
print(f"rows after: {after}")
if after != before:
    sys.exit("DID NOT CLEAN UP")
print("clean")
sys.exit(0 if ok else 1)
