"""Mock Google Play Integrity — the OAuth token endpoint and decodeIntegrityToken.

A mock integrity token is "mockpi." + base64url(JSON) where the JSON is the verdict that
decodeIntegrityToken returns as tokenPayloadExternal. Tests build tokens through
/playintegrity/device/token, which plays the Android app: it fills a passing verdict for the
given package and requestHash and applies any overrides.

Routes:
  POST /playintegrity/token                                  — service-account JWT grant → access token
  POST /playintegrity/v1/<package>:decodeIntegrityToken      — {integrity_token} → {tokenPayloadExternal}
  POST /playintegrity/device/token                           — {packageName, requestHash, certDigest,
                                                                 overrides?} → {token}
  GET  /playintegrity/service-account                        — a throwaway service account JSON whose
                                                                 token_uri is this mock

Special tokens: "mockpi-outage" answers 503, "mockpi-slow" answers after 5 s, anything that is
not a mock token answers 400 like Google does for an undecodable token.
"""

import base64
import json
import threading
import time

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric import rsa

ACCESS_TOKEN = "mock-play-integrity-access-token"

_LOCK = threading.Lock()
_SIGNING_KEY = []


def _signing_key_pem():
    with _LOCK:
        if not _SIGNING_KEY:
            key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
            _SIGNING_KEY.append(key.private_bytes(
                serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption(),
            ).decode())
        return _SIGNING_KEY[0]


def service_account(base_url):
    return {
        "type": "service_account",
        "project_id": "local-play-integrity",
        "private_key_id": "local-test-key",
        "private_key": _signing_key_pem(),
        "client_email": "play-integrity@local-play-integrity.iam.gserviceaccount.com",
        "client_id": "000000000000000000000",
        "auth_uri": f"{base_url}/playintegrity/auth",
        "token_uri": f"{base_url}/playintegrity/token",
        "auth_provider_x509_cert_url": f"{base_url}/playintegrity/certs",
        "client_x509_cert_url": f"{base_url}/playintegrity/certs/client",
    }


def _deep_merge(base, extra):
    for key, value in (extra or {}).items():
        if isinstance(value, dict) and isinstance(base.get(key), dict):
            _deep_merge(base[key], value)
        else:
            base[key] = value
    return base


def device_token(package_name, request_hash, cert_digest, overrides=None):
    payload = {
        "requestDetails": {
            "requestPackageName": package_name,
            "requestHash": request_hash,
            "timestampMillis": str(int(time.time() * 1000)),
        },
        "appIntegrity": {
            "appRecognitionVerdict": "PLAY_RECOGNIZED",
            "packageName": package_name,
            "certificateSha256Digest": [cert_digest],
            "versionCode": "100",
        },
        "deviceIntegrity": {"deviceRecognitionVerdict": ["MEETS_DEVICE_INTEGRITY"]},
        "accountDetails": {"appLicensingVerdict": "LICENSED"},
    }
    _deep_merge(payload, overrides)
    encoded = base64.urlsafe_b64encode(json.dumps(payload).encode()).decode().rstrip("=")
    return "mockpi." + encoded


def _decode(token):
    if not token.startswith("mockpi."):
        return None
    raw = token[len("mockpi."):]
    try:
        return json.loads(base64.urlsafe_b64decode(raw + "=" * (-len(raw) % 4)))
    except ValueError:
        return None


def handle(handler, path, body):
    try:
        req = json.loads(body.decode() if isinstance(body, bytes) else (body or "{}")) or {}
    except (ValueError, UnicodeDecodeError):
        req = {}

    if path.endswith("/service-account"):
        host = handler.headers.get("Host") or "localhost:8080"
        return handler._json(service_account(f"http://{host}"))

    if path.endswith("/token") and not path.endswith("/device/token"):
        return handler._json({"access_token": ACCESS_TOKEN, "expires_in": 3600, "token_type": "Bearer"})

    if path.endswith("/device/token"):
        token = device_token(req["packageName"], req["requestHash"], req["certDigest"], req.get("overrides"))
        return handler._json({"token": token})

    if ":decodeIntegrityToken" in path:
        if handler.headers.get("Authorization") != f"Bearer {ACCESS_TOKEN}":
            return handler._json({"error": {"code": 401, "message": "Request had invalid authentication credentials."}}, status=401)
        token = req.get("integrity_token", "")
        if token == "mockpi-outage":
            return handler._json({"error": {"code": 503, "message": "The service is currently unavailable."}}, status=503)
        if token == "mockpi-slow":
            time.sleep(5)
        payload = _decode(token)
        if payload is None:
            return handler._json({"error": {"code": 400, "message": "Integrity token cannot be decoded."}}, status=400)
        return handler._json({"tokenPayloadExternal": payload})

    return handler._json({"error": f"unknown playintegrity route {path}"}, status=404)
