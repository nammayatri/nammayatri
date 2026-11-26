"""Regenerates fixtures/app_attest.json for the App Attest unit tests.

Run from the repository root:  python3 Backend/lib/shared-services/test/AppAttestation/gen_fixtures.py

The objects are signed by the local test CA of the mock server (dev/mock-servers/services/appattest.py),
whose root certificate is written to the fixture as well.
"""

import base64
import datetime
import hashlib
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "..", "..", "dev", "mock-servers"))

from services import appattest  # noqa: E402

TEAM_ID = "TEAM123456"
BUNDLE_ID = "in.juspay.nammayatri"
KEY_CHALLENGE = "key-registration-challenge"
CANONICAL = "v1|NAMMA_YATRI|+91|9876543210|assertion-challenge"
ISSUED_AT = datetime.datetime(2026, 11, 17)


def b64(raw):
    return base64.b64encode(raw).decode()


def main():
    key_id, attestation, private_key = appattest.attest(
        TEAM_ID, BUNDLE_ID, "DEVELOPMENT", hashlib.sha256(KEY_CHALLENGE.encode()).digest(), now=ISSUED_AT
    )
    _, attestation_nonzero_counter, _ = appattest.attest(
        TEAM_ID, BUNDLE_ID, "DEVELOPMENT", hashlib.sha256(KEY_CHALLENGE.encode()).digest(),
        private_key=private_key, now=ISSUED_AT, sign_count=1,
    )
    client_data_hash = hashlib.sha256(CANONICAL.encode()).digest()
    fixture = {
        "rootPem": appattest.root_pem(),
        "teamId": TEAM_ID,
        "bundleId": BUNDLE_ID,
        "keyChallenge": KEY_CHALLENGE,
        "canonical": CANONICAL,
        "keyId": b64(key_id),
        "attestation": b64(attestation),
        "attestationNonZeroCounter": b64(attestation_nonzero_counter),
        "assertionCounter1": b64(appattest.assert_request(private_key, TEAM_ID, BUNDLE_ID, client_data_hash, 1)),
        "assertionOtherRequest": b64(
            appattest.assert_request(private_key, TEAM_ID, BUNDLE_ID, hashlib.sha256(b"other request").digest(), 2)
        ),
    }
    with open(os.path.join(HERE, "fixtures", "app_attest.json"), "w") as out:
        json.dump(fixture, out, indent=2)
        out.write("\n")


if __name__ == "__main__":
    main()
