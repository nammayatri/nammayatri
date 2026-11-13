import base64
import hashlib
import json
import logging
import os
import secrets
import struct
import threading

log = logging.getLogger("kms")

_LOCK = threading.Lock()
_BLOB_MAGIC = b"MOCKKMS1"
_OBJECT_MAGIC = bytes([0x00, 0x4E, 0x59, 0x44, 0x56, 0x01])
_ACCOUNT_ARN = "arn:aws:kms:ap-south-1:000000000000:key/"
_PRIVATE_KEY_PATH = os.path.join(os.path.dirname(__file__), "..", "kms", "dev-archive-private-key.pem")
_PUBLIC_KEY_PATH = os.path.join(os.path.dirname(__file__), "..", "kms", "dev-archive-public-key.pem")
_KEY_STATES = ("Enabled", "Disabled", "PendingDeletion", "Deleted")

_STATE = {"autoProvision": False, "aliases": {}, "keys": {}, "keyDetails": {}}


def _key_id_for(alias):
    return "mock-" + hashlib.sha256(alias.encode()).hexdigest()[:32]


def _parse(body):
    if not body:
        return {}
    try:
        return json.loads(body.decode("utf-8") if isinstance(body, bytes) else body)
    except (json.JSONDecodeError, ValueError):
        return {}


def _error(handler, code, message, status=400):
    return handler._raw(
        status,
        "application/x-amz-json-1.1",
        json.dumps({"__type": code, "message": message}),
        {"x-amzn-ErrorType": code},
    )


def _ok(handler, data):
    return handler._raw(200, "application/x-amz-json-1.1", json.dumps(data))


def _key_state_error(handler, key_id, state):
    if state == "Disabled":
        return _error(handler, "DisabledException", f"{_ACCOUNT_ARN}{key_id} is disabled.")
    if state == "PendingDeletion":
        return _error(handler, "KMSInvalidStateException", f"{_ACCOUNT_ARN}{key_id} is pending deletion.")
    return _error(handler, "NotFoundException", f"Key '{_ACCOUNT_ARN}{key_id}' does not exist")


def _resolve(key_ref):
    with _LOCK:
        if key_ref.startswith("alias/"):
            key_id = _STATE["aliases"].get(key_ref)
            if key_id is None:
                if not _STATE["autoProvision"]:
                    return None, None
                key_id = _key_id_for(key_ref)
                _STATE["aliases"][key_ref] = key_id
                _STATE["keys"].setdefault(key_id, "Enabled")
            return key_id, _STATE["keys"].get(key_id, "Deleted")
        key_id = key_ref.rsplit("/", 1)[-1]
        if key_id in _STATE["keys"]:
            return key_id, _STATE["keys"][key_id]
        return (key_id, "Enabled") if _STATE["autoProvision"] else (None, None)


def _generate_data_key(handler, req):
    key_ref = req.get("KeyId") or ""
    key_id, state = _resolve(key_ref)
    if key_id is None:
        return _error(handler, "NotFoundException", f"Alias {key_ref} is not found.")
    if state != "Enabled":
        return _key_state_error(handler, key_id, state)
    size = req.get("NumberOfBytes") or (16 if req.get("KeySpec") == "AES_128" else 32)
    dek = secrets.token_bytes(size)
    blob = _BLOB_MAGIC + json.dumps(
        {"k": key_id, "c": req.get("EncryptionContext") or {}, "d": base64.b64encode(dek).decode()},
        sort_keys=True,
    ).encode()
    return _ok(handler, {
        "CiphertextBlob": base64.b64encode(blob).decode(),
        "Plaintext": base64.b64encode(dek).decode(),
        "KeyId": _ACCOUNT_ARN + key_id,
    })


def _decrypt(handler, req):
    try:
        blob = base64.b64decode(req.get("CiphertextBlob") or "")
        if not blob.startswith(_BLOB_MAGIC):
            raise ValueError("not a mock blob")
        sealed = json.loads(blob[len(_BLOB_MAGIC):])
    except (ValueError, json.JSONDecodeError):
        return _error(handler, "InvalidCiphertextException", "The ciphertext is invalid.")
    key_id, state = _resolve(sealed["k"])
    if key_id is None:
        return _error(handler, "NotFoundException", f"Key '{_ACCOUNT_ARN}{sealed['k']}' does not exist")
    if state != "Enabled":
        return _key_state_error(handler, key_id, state)
    if (req.get("EncryptionContext") or {}) != sealed["c"]:
        return _error(handler, "InvalidCiphertextException", "The encryption context does not match.")
    return _ok(handler, {
        "KeyId": _ACCOUNT_ARN + key_id,
        "Plaintext": sealed["d"],
        "EncryptionAlgorithm": "SYMMETRIC_DEFAULT",
    })


def _get_public_key(handler, req):
    key_ref = req.get("KeyId") or ""
    with open(_PUBLIC_KEY_PATH) as key_file:
        pem_lines = [line for line in key_file.read().splitlines() if line and not line.startswith("-----")]
    return _ok(handler, {
        "KeyId": key_ref,
        "PublicKey": "".join(pem_lines),
        "KeySpec": "RSA_4096",
        "KeyUsage": "ENCRYPT_DECRYPT",
        "EncryptionAlgorithms": ["RSAES_OAEP_SHA_256"],
    })


def _create_key(handler, req):
    key_id = "mock-" + secrets.token_hex(16)
    try:
        policy = json.loads(req.get("Policy") or "{}")
    except (json.JSONDecodeError, ValueError):
        return _error(handler, "MalformedPolicyDocumentException", "The key policy is not valid JSON.")
    with _LOCK:
        _STATE["keys"][key_id] = "Enabled"
        _STATE["keyDetails"][key_id] = {
            "policy": policy,
            "tags": req.get("Tags") or [],
            "description": req.get("Description"),
            "bypassPolicyLockoutSafetyCheck": bool(req.get("BypassPolicyLockoutSafetyCheck")),
        }
    return _ok(handler, {"KeyMetadata": {"KeyId": key_id, "Arn": _ACCOUNT_ARN + key_id, "KeyState": "Enabled", "Enabled": True}})


def _create_alias(handler, req):
    alias = req.get("AliasName") or ""
    target = (req.get("TargetKeyId") or "").rsplit("/", 1)[-1]
    with _LOCK:
        if alias in _STATE["aliases"]:
            return _error(handler, "AlreadyExistsException", f"An alias with the name {alias} already exists")
        if target not in _STATE["keys"]:
            return _error(handler, "NotFoundException", f"Key '{_ACCOUNT_ARN}{target}' does not exist")
        _STATE["aliases"][alias] = target
    return _ok(handler, {})


_OPERATIONS = {
    "GenerateDataKey": _generate_data_key,
    "Decrypt": _decrypt,
    "GetPublicKey": _get_public_key,
    "CreateKey": _create_key,
    "CreateAlias": _create_alias,
}


def handle(handler, path, body):
    target = handler.headers.get("X-Amz-Target", "")
    operation = target.split(".", 1)[-1]
    forced, _ = handler._get_override("kms")
    if forced:
        return _error(handler, forced, f"Forced by mock override for {operation}.")
    action = _OPERATIONS.get(operation)
    if action is None:
        return _error(handler, "UnsupportedOperationException", f"Mock KMS does not implement {operation}.")
    return action(handler, _parse(body))


def _take_length_prefixed(data, offset):
    (size,) = struct.unpack_from(">H", data, offset)
    start = offset + 2
    field = data[start:start + size]
    if len(field) != size:
        raise ValueError("truncated field")
    return field, start + size


def open_archive(sealed_bytes):
    from cryptography.hazmat.primitives import hashes, serialization
    from cryptography.hazmat.primitives.asymmetric import padding
    from cryptography.hazmat.primitives.ciphers.aead import AESGCM

    if not sealed_bytes.startswith(_OBJECT_MAGIC):
        raise ValueError("not a sealed document")
    archive_encrypted_data_key, offset = _take_length_prefixed(sealed_bytes, len(_OBJECT_MAGIC))
    document_id, offset = _take_length_prefixed(sealed_bytes, offset)
    nonce = sealed_bytes[offset:offset + 12]
    tag = sealed_bytes[offset + 12:offset + 28]
    ciphertext = sealed_bytes[offset + 28:]
    header = sealed_bytes[:offset + 12]
    with open(_PRIVATE_KEY_PATH, "rb") as key_file:
        private_key = serialization.load_pem_private_key(key_file.read(), password=None)
    dek = private_key.decrypt(
        archive_encrypted_data_key,
        padding.OAEP(mgf=padding.MGF1(algorithm=hashes.SHA256()), algorithm=hashes.SHA256(), label=None),
    )
    document = AESGCM(dek).decrypt(nonce, ciphertext + tag, header)
    return document_id.decode(), document


def handle_admin(handler, path, raw_body):
    body = _parse(raw_body)
    action = path[len("/mock/kms/"):].strip("/")

    if action == "state" and handler.command == "GET":
        with _LOCK:
            return handler._json(_STATE)

    if handler.command != "POST":
        return handler._json({"error": "method not allowed"}, status=405)

    if action == "reset":
        with _LOCK:
            _STATE.update({"autoProvision": False, "aliases": {}, "keys": {}, "keyDetails": {}})
            return handler._json(_STATE)

    if action == "config":
        with _LOCK:
            if "autoProvision" in body:
                _STATE["autoProvision"] = bool(body["autoProvision"])
            return handler._json(_STATE)

    if action == "alias":
        alias = body.get("alias") or ""
        state = body.get("state") or "Enabled"
        if not alias.startswith("alias/") or state not in _KEY_STATES:
            return handler._json({"error": f"need alias/... and state in {list(_KEY_STATES)}"}, status=400)
        with _LOCK:
            key_id = _STATE["aliases"].setdefault(alias, _key_id_for(alias))
            _STATE["keys"][key_id] = state
            return handler._json({"alias": alias, "keyId": key_id, "state": state})

    if action == "open-archive":
        try:
            if body.get("path"):
                with open(body["path"], "rb") as sealed_file:
                    sealed_bytes = sealed_file.read()
            else:
                sealed_bytes = base64.b64decode(body.get("object") or "")
            document_id, document = open_archive(sealed_bytes)
        except ImportError:
            return handler._json({"error": "python 'cryptography' package is not available"}, status=501)
        except Exception as exc:
            return handler._json({"error": f"{type(exc).__name__}: {exc}"}, status=400)
        return handler._json({
            "documentId": document_id,
            "size": len(document),
            "document": base64.b64encode(document).decode(),
        })

    return handler._json({"error": f"unknown kms admin action {action}"}, status=404)
