#!/usr/bin/env bash
# CI assertion: every generated direct-dashboard handler that takes a DashboardUser
# together with a merchant ShortId and a city MUST call enforceMerchantCityAccessForUser
# as its handler-entry authorization (one-token-per-person: the token no longer carries
# merchant/city, so each handler must authorize the URL's merchant/city against
# merchant_access). A handler with those three args but no enforce call is an
# unauthorized cross-merchant hole. Fix in the generator (namma-dsl) + regenerate;
# src-read-only/ must not be hand-edited.
#
# Exit 0 = clean, 1 = violations found.
set -euo pipefail
ROOT="${1:-$(git rev-parse --show-toplevel)/Backend}"
python3 - "$ROOT" <<'PY'
import re, glob, os, sys
root = sys.argv[1]
dirs = [
  os.path.join(root, "app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/API/Action/DashboardAuth"),
  os.path.join(root, "app/rider-platform/rider-app/Main/src-read-only/API/Action/DashboardAuth"),
]
violations = []
checked = 0
for d in dirs:
  for f in glob.glob(d + "/**/*.hs", recursive=True):
    src = open(f).read()
    for m in re.finditer(r'^([a-z][A-Za-z0-9_]*) :: \((.*?)\)\n', src, re.M | re.S):
      name, sig = m.group(1), m.group(2)
      if 'DashboardUser' not in sig: continue
      if 'ShortId' not in sig or 'Merchant' not in sig: continue
      if 'Context.City' not in sig and 'Beckn.Context.City' not in sig: continue
      checked += 1
      body = re.search(r'^' + re.escape(name) + r' [a-zA-Z].*?(?=^\S|\Z)', src, re.M | re.S)
      body = body.group(0) if body else ""
      if 'enforceMerchantCityAccessForUser' not in body:
        violations.append((os.path.relpath(f, root), name))
print(f"[dashboard-enforce] handlers checked (DashboardUser+Merchant+City): {checked}")
print(f"[dashboard-enforce] violations (missing enforceMerchantCityAccessForUser): {len(violations)}")
for rel, name in violations:
  print(f"  VIOLATION: {name}  ({rel})")
sys.exit(1 if violations else 0)
PY
