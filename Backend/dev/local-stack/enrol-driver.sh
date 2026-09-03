#!/usr/bin/env bash
# Give a driver the personal code he signs in with, or take it away.
#
# ── Why a code at all ───────────────────────────────────────────────────────
# There is no SMS gateway yet, so the backend runs with `useFakeSms = Some 7891`
# and accepts that one code from anybody. On the rider side of a pilot that is
# survivable. On the driver side, published on 443, it means whoever knows a
# driver's phone number can take his shift and his earnings.
#
# So the auth guard holds a code per approved number, checks it itself, and only
# then substitutes the fixed code the backend wants. This script is how a number
# gets into that list. Nothing else grants access: a number not listed here is
# refused at `POST /ui/auth`, before the backend can even create a driver record
# for it.
#
# ── What this does NOT do ───────────────────────────────────────────────────
# Enrolling lets a driver sign in. It does not enable him, does not verify his
# documents, and does not attach a vehicle -- those are office operations on
# /dashboard/, which stays unpublished. A freshly enrolled driver can sign in
# and see that he is waiting for approval, and that is all.
#
# ── The code is spoken, not sent ────────────────────────────────────────────
# It is printed once, here, and is not recoverable afterwards -- the file keeps
# a salted hash, not the code. That fits how the pilot actually onboards: the
# agency enrols a driver face to face and hands him the number on paper. When a
# gateway exists the guard will generate and send a code per sign-in instead,
# through the same substitution; only the source of the code changes.
#
# ── Usage ───────────────────────────────────────────────────────────────────
#   ./enrol-driver.sh 0551234567 "Karim Benali"     # enrol, print a new code
#   ./enrol-driver.sh --set 0551234567 482913       # set a chosen code
#   ./enrol-driver.sh --list                        # who is enrolled
#   ./enrol-driver.sh --revoke 0551234567           # remove
#
# Run it on the VPS, in this directory. The guard notices the file changing and
# reloads it within one request -- no restart, no dropped sign-in.
set -euo pipefail

cd "$(dirname "$0")"
FILE="${DRIVER_CODES:-auth-guard/driver-codes.json}"

python3 - "$FILE" "$@" <<'PY'
import json, os, secrets, sys, hashlib, stat

path, *args = sys.argv[1:]

# The guard keys on `mobileCountryCode + mobileNumber`, exactly as the app sends
# them. For Algeria that is "+213" and a local number that KEEPS ITS TRUNK ZERO
# -- 0551234567, not 551234567. The international form is what a European would
# write and it is what this backend rejects, so normalising it away here would
# produce a key no sign-in ever matches: enrolment appears to work, the driver
# is refused, and nothing says why.
CC = os.environ.get("COUNTRY_CODE", "+213")

def key(local):
    d = "".join(c for c in local if c.isdigit())
    if d.startswith("213"):
        d = d[3:]
    if not d.startswith("0"):
        d = "0" + d
    if len(d) != 10:
        sys.exit(f"'{local}' is not an Algerian mobile number (expected 10 digits like 0551234567)")
    return CC + d

def load():
    try:
        with open(path) as fh:
            doc = json.load(fh)
    except FileNotFoundError:
        return {"codes": {}}
    doc.setdefault("codes", {})
    return doc

def save(doc):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    tmp = path + ".new"
    with open(tmp, "w") as fh:
        json.dump(doc, fh, indent=2, sort_keys=True)
        fh.write("\n")
    # Hashes, not codes -- but a list of every driver's phone number is still
    # not a world-readable file.
    os.chmod(tmp, stat.S_IRUSR | stat.S_IWUSR)
    os.replace(tmp, path)

def put(doc, k, code, note):
    salt = secrets.token_hex(8)
    doc["codes"][k] = {
        "salt": salt,
        "hash": hashlib.sha256(f"{salt}:{k}:{code}".encode()).hexdigest(),
        "note": note,
    }

doc = load()

if not args or args[0] in ("-h", "--help"):
    sys.exit(__doc__ or "see the header of enrol-driver.sh")

if args[0] == "--list":
    if not doc["codes"]:
        print("nobody is enrolled -- every driver sign-in is refused")
    for k, v in sorted(doc["codes"].items()):
        print(f"  {k}  {v.get('note', '')}")
    sys.exit(0)

if args[0] == "--revoke":
    k = key(args[1])
    if doc["codes"].pop(k, None) is None:
        sys.exit(f"{k} was not enrolled")
    save(doc)
    print(f"{k} revoked. Any session he already holds stays valid until it expires;")
    print("to end it now, disable the driver from the office side as well.")
    sys.exit(0)

if args[0] == "--set":
    k, code = key(args[1]), args[2]
    if not (code.isdigit() and 4 <= len(code) <= 8):
        sys.exit("a code is 4 to 8 digits")
    note = doc["codes"].get(k, {}).get("note", "")
    put(doc, k, code, note)
    save(doc)
    print(f"{k}: code set.")
    sys.exit(0)

k = key(args[0])
note = args[1] if len(args) > 1 else ""
existing = k in doc["codes"]
# Six digits, not the backend's four: the guard allows three attempts before a
# fifteen-minute lock, and six digits makes guessing pointless rather than
# merely slow. The driver sign-in screen has to accept six -- the passenger one
# still takes four, and they are different screens.
code = "".join(secrets.choice("0123456789") for _ in range(6))
put(doc, k, code, note)
save(doc)

print()
print(f"  {k}" + (f"   {note}" if note else ""))
print(f"  code: {code}")
print()
print("  Write it down now -- it is stored hashed and cannot be read back.")
if existing:
    print("  This REPLACED an earlier code. He must be told the new one.")
print("  He can sign in with it. He still needs the office to enable him")
print("  and attach his vehicle before he can go online.")
PY
