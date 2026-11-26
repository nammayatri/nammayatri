"""Mock Apple App Attest device — stands in for an iPhone running the app, for local tests.

The backend verifies App Attest objects locally against a root CA; there is no Apple server
to mock. Instead this module plays the device: it creates Secure-Enclave-like P-256 keys,
issues credential certificates from a local test CA, and signs assertions. Point the city's
app_attestation_config.app_attest_root_cert_pem at GET /appattest/root.pem.

The CA keys are derived from fixed seeds, so the root certificate is stable across restarts.
They are test material only and trust nothing outside a local stack.

Routes:
  GET  /appattest/root.pem          — the test root CA (PEM) the backend should trust.
  POST /appattest/device/attest     — {teamId, bundleId, environment, clientDataHash(b64)}
                                      → {keyId(b64), attestation(b64)}
  POST /appattest/device/assert     — {keyId(b64), clientDataHash(b64), teamId, bundleId,
                                       counter?(int), tamper?("signature"|"rpId")}
                                      → {assertion(b64), counter}

clientDataHash is the raw 32-byte SHA-256 the app passes to attestKey / generateAssertion,
base64 encoded: SHA-256(challenge) for attestation, SHA-256(canonical request) for assertions.
"""

import base64
import datetime
import hashlib
import json
import struct
import threading

from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import ec
from cryptography.x509.oid import NameOID

NONCE_OID = x509.ObjectIdentifier("1.2.840.113635.100.8.2")

_P256_ORDER = 0xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551
_P384_ORDER = int(
    "ffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf"
    "581a0db248b0a77aecec196accc52973",
    16,
)

_LOCK = threading.Lock()
_KEYS = {}  # keyId(b64) -> {"private": key, "counter": int}


# ── CBOR (only the subset App Attest objects use) ──


def _cbor_head(major, length):
    if length < 24:
        return bytes([(major << 5) | length])
    if length < 0x100:
        return bytes([(major << 5) | 24, length])
    if length < 0x10000:
        return bytes([(major << 5) | 25]) + struct.pack(">H", length)
    return bytes([(major << 5) | 26]) + struct.pack(">I", length)


def cbor_encode(value):
    if isinstance(value, bytes):
        return _cbor_head(2, len(value)) + value
    if isinstance(value, str):
        raw = value.encode()
        return _cbor_head(3, len(raw)) + raw
    if isinstance(value, list):
        return _cbor_head(4, len(value)) + b"".join(cbor_encode(v) for v in value)
    if isinstance(value, dict):
        return _cbor_head(5, len(value)) + b"".join(cbor_encode(k) + cbor_encode(v) for k, v in value.items())
    if isinstance(value, int) and value >= 0:
        return _cbor_head(0, value)
    raise TypeError(f"cbor_encode: unsupported {type(value)}")


# ── Test CA ──


def _derived_key(seed, curve, order):
    digest = hashlib.sha512(seed.encode()).digest()
    return ec.derive_private_key(int.from_bytes(digest, "big") % (order - 1) + 1, curve)


def _name(common_name):
    return x509.Name([
        x509.NameAttribute(NameOID.COMMON_NAME, common_name),
        x509.NameAttribute(NameOID.ORGANIZATION_NAME, "Namma Yatri Local Test"),
    ])


def _ca_cert(subject, issuer, public_key, signing_key, serial):
    return (
        x509.CertificateBuilder()
        .subject_name(subject)
        .issuer_name(issuer)
        .public_key(public_key)
        .serial_number(serial)
        .not_valid_before(datetime.datetime(2020, 1, 1))
        .not_valid_after(datetime.datetime(2045, 1, 1))
        .add_extension(x509.BasicConstraints(ca=True, path_length=None), critical=True)
        .add_extension(x509.KeyUsage(False, False, False, False, False, True, True, False, False), critical=True)
        .sign(signing_key, hashes.SHA384())
    )


ROOT_KEY = _derived_key("nammayatri-local-app-attest-root", ec.SECP384R1(), _P384_ORDER)
INTERMEDIATE_KEY = _derived_key("nammayatri-local-app-attest-intermediate", ec.SECP384R1(), _P384_ORDER)
ROOT_NAME = _name("Local Test App Attestation Root CA")
INTERMEDIATE_NAME = _name("Local Test App Attestation CA 1")
ROOT_CERT = _ca_cert(ROOT_NAME, ROOT_NAME, ROOT_KEY.public_key(), ROOT_KEY, 1)
INTERMEDIATE_CERT = _ca_cert(INTERMEDIATE_NAME, ROOT_NAME, INTERMEDIATE_KEY.public_key(), ROOT_KEY, 2)


def root_pem():
    return ROOT_CERT.public_bytes(serialization.Encoding.PEM).decode()


# ── Device ──


def _uncompressed_point(public_key):
    return public_key.public_bytes(serialization.Encoding.X962, serialization.PublicFormat.UncompressedPoint)


def app_id_hash(team_id, bundle_id):
    return hashlib.sha256(f"{team_id}.{bundle_id}".encode()).digest()


def aaguid(environment):
    return b"appattestdevelop" if environment == "DEVELOPMENT" else b"appattest" + b"\x00" * 7


def attest(team_id, bundle_id, environment, client_data_hash, private_key=None, now=None,
           sign_count=0, aaguid_override=None):
    """Returns (keyId bytes, attestation object bytes, private key)."""
    private_key = private_key or ec.generate_private_key(ec.SECP256R1())
    point = _uncompressed_point(private_key.public_key())
    key_id = hashlib.sha256(point).digest()
    auth_data = (
        app_id_hash(team_id, bundle_id)
        + bytes([0x40])
        + struct.pack(">I", sign_count)
        + (aaguid_override or aaguid(environment))
        + struct.pack(">H", len(key_id))
        + key_id
        + cbor_encode({"kty": 2})
    )
    nonce = hashlib.sha256(auth_data + client_data_hash).digest()
    nonce_extension = bytes([0x30, 0x24, 0xA1, 0x22, 0x04, 0x20]) + nonce
    now = now or datetime.datetime.utcnow()
    credential_cert = (
        x509.CertificateBuilder()
        .subject_name(_name(key_id.hex()))
        .issuer_name(INTERMEDIATE_NAME)
        .public_key(private_key.public_key())
        .serial_number(int.from_bytes(key_id[:8], "big"))
        .not_valid_before(now - datetime.timedelta(days=1))
        .not_valid_after(now + datetime.timedelta(days=365))
        .add_extension(x509.UnrecognizedExtension(NONCE_OID, nonce_extension), critical=False)
        .sign(INTERMEDIATE_KEY, hashes.SHA256())
    )
    attestation = cbor_encode({
        "fmt": "apple-appattest",
        "attStmt": {
            "x5c": [
                credential_cert.public_bytes(serialization.Encoding.DER),
                INTERMEDIATE_CERT.public_bytes(serialization.Encoding.DER),
            ],
            "receipt": b"local-test-receipt",
        },
        "authData": auth_data,
    })
    return key_id, attestation, private_key


def assert_request(private_key, team_id, bundle_id, client_data_hash, counter):
    authenticator_data = app_id_hash(team_id, bundle_id) + bytes([0x40]) + struct.pack(">I", counter)
    nonce = hashlib.sha256(authenticator_data + client_data_hash).digest()
    signature = private_key.sign(nonce, ec.ECDSA(hashes.SHA256()))
    return cbor_encode({"signature": signature, "authenticatorData": authenticator_data})


# ── HTTP ──


def _b64(raw):
    return base64.b64encode(raw).decode()


def _unb64(text):
    return base64.b64decode(text + "=" * (-len(text) % 4))


def handle(handler, path, body):
    if path.endswith("/root.pem") and handler.command == "GET":
        return handler._raw(200, "application/x-pem-file", root_pem())

    try:
        req = json.loads(body.decode() if isinstance(body, bytes) else (body or "{}")) or {}
    except (ValueError, UnicodeDecodeError):
        return handler._json({"error": "body must be JSON"}, status=400)

    if path.endswith("/device/attest") and handler.command == "POST":
        key_id, attestation, private_key = attest(
            req["teamId"], req["bundleId"], req.get("environment", "DEVELOPMENT"), _unb64(req["clientDataHash"]),
        )
        with _LOCK:
            _KEYS[_b64(key_id)] = {"private": private_key, "counter": 0}
        return handler._json({"keyId": _b64(key_id), "attestation": _b64(attestation)})

    if path.endswith("/device/assert") and handler.command == "POST":
        with _LOCK:
            key = _KEYS.get(req["keyId"])
            if key is None:
                return handler._json({"error": "unknown keyId"}, status=404)
            counter = req.get("counter") or key["counter"] + 1
            key["counter"] = max(key["counter"], counter)
        team_id, bundle_id = req["teamId"], req["bundleId"]
        if req.get("tamper") == "rpId":
            bundle_id = bundle_id + ".tampered"
        assertion = assert_request(key["private"], team_id, bundle_id, _unb64(req["clientDataHash"]), counter)
        if req.get("tamper") == "signature":
            assertion = assert_request(key["private"], team_id, bundle_id, hashlib.sha256(b"other").digest(), counter)
        return handler._json({"assertion": _b64(assertion), "counter": counter})

    return handler._json({"error": f"unknown appattest route {path}"}, status=404)
