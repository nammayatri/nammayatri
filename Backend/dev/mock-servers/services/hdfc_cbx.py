"""HDFC CBX (nodal bulk payout) mock — token, bulk payment, bulk inquiry, batch-number recovery.

Mirrors what the shared-kernel adapter (Kernel.External.Payout.Interface.HdfcCbx) expects, so the
real adapter runs end to end against it: only the merchant_service_config URLs change.

  POST /hdfc/auth/oauth/v1/token?grant_type=client_credentials&scope=..   Basic auth, plain JSON
  POST /hdfc/api/v1/cbx-nodal-bulkPayment                                 JOSE in, JOSE out
  POST /hdfc/api/v1/cbx-nodal-bulkPaymentInq                              JOSE in, JOSE out
  POST /hdfc/api/v2/cbx-getBatchNo                                        JOSE in, JOSE out

Envelope (Kernel.Utils.Jose): a request is a JWS (RS256, client key) inside a JWE (RSA-OAEP-256 +
A256GCM, to the bank key); a response is the reverse. Replies outside the envelope are bare
application/problem+json {"errors":[{"code","reason"}]} -- the "enquire again" 202, refusals, 5xx.
Plain HTTP only: point url/tokenUrl at http://localhost:8080/hdfc. Keys live in
dev/mock-servers/hdfc-cbx/test-keys: a TEST-ONLY kit generated on first use and gitignored.

Inquiry, per batch, as HDFC answers it: the first hit is accepted and answered 202 "We have accepted your
request. Please enquire again after sometime" (the batch is now being processed); the next hit, once
`readyAfterSeconds` have passed, gets the actual rows -- every item advanced one step -- and the batch goes
back to idle, so the next check starts again with "enquire again". A hit before it is ready is told to
enquire again. Only real answers advance items; injected faults do not touch this state.

Item ladders (one step per answered inquiry, `stepsPerInquiry` to go faster):
  NEFT/RTGS/IMPS   P -> C -> E -> E/TXSIP -> E/TXSETT (refno = UTR, RRN for IMPS)
  intra-bank       P -> C -> E (bankrefno = FT number, no rbistatus)
                   intra-bank = cdflag I, or IFSC starting with `ownBankPrefix` (default HDFC);
                   the sent cdflag is echoed unless `echoConvertedCdflag` is set.
  rejected at once (R): name > 40 chars on NEFT/RTGS (HDFC allow 200 elsewhere), cdflag I with a non-HDFC IFSC,
                   malformed IFSC.

Beneficiary account-number suffixes:
  ...9001  R "Invalid beneficiary account"     ...9005  stuck at P (never approved)
  ...9002  E then TXREJE (return after debit)   ...9006  unknown codstatus X
  ...9003  TXDSETT (deemed settled)             ...9007  E with unknown rbistatus TXFOO
  ...9004  stuck at C

Control API (plain JSON):
  GET  /hdfc/mock/state                        tokens, batches, items, config, pending faults
  POST /hdfc/mock/reset                        clear batches/tokens/faults (config kept)
  POST /hdfc/mock/faults/clear                 drop queued faults only (batches in flight untouched)
  POST /hdfc/mock/config                       merge into config, e.g. {"autoApprove": false}
  POST /hdfc/mock/fault                        queue a one-shot fault for the next call of an API:
       {"api": "token|payment|inquiry|batchnum", "kind": .., "status": n, "code": "..", "reason": "..",
        "process": bool, "seconds": n, "match": {"batchnum": ".."}}   match: only a call whose request has these fields
                                                                       ({"accno": ".."}: a payment file with that account)
       any api:   5xx | 4xx | 401 | problem   answer with that problem+json; "process": true acts
                                              on the request first (payment: the file IS accepted)
                  timeout                     sleep `seconds` (65) then act normally;
                                              "process": false answers 504 without acting
       payment:   duplicate | accept-no-batchnum
                  reject                      codstatus 1 (not a duplicate), nothing stored; "reason" sets txtstatus
                  unknown-ack                 a codstatus that is neither 0 nor 1 ("codstatus", default "2"; "" sends
                                              none); "process": true stores the file first (it did land)
       inquiry:   notready | ndf
       batchnum:  norecord
                  unknown                     neither a batch number nor "no record" (codstatus "9" or "code")
  POST /hdfc/mock/batch/<batchnum>/approve     release items held at P (autoApprove=false)
  POST /hdfc/mock/item/<custrefno>             force a row: {codstatus, rbistatus, refno, ...}
"""

import base64
import hashlib
import json
import logging
import os
import re
import threading
import time
import uuid
from datetime import datetime, timedelta, timezone
from urllib.parse import parse_qs, urlparse

from status_store import register_body_decoder

log = logging.getLogger("mock-server.hdfc_cbx")

_HERE = os.path.dirname(os.path.abspath(__file__))
_ROOT = os.path.dirname(_HERE)
# A TEST-ONLY kit, generated on first use (ensure_keys) and gitignored. The config seed_config.py writes
# carries the client key passetto-encrypted, so it must come from this same kit. Never real keys.
KEYS_DIR = os.environ.get("HDFC_MOCK_KEYS_DIR", os.path.join(_ROOT, "hdfc-cbx", "test-keys"))
STATE_FILE = os.environ.get("HDFC_MOCK_STATE_FILE", os.path.join(_ROOT, ".state", "hdfc_cbx.json"))

DEFAULT_CONFIG = {
    # first hit of a check: 202 "enquire again"; the next hit, readyAfterSeconds later, gets the rows
    "enquireAgainFirst": True,
    "readyAfterSeconds": 5,
    # false: items sit at P until POST /hdfc/mock/batch/<batchnum>/approve
    "autoApprove": True,
    # an intra-bank item converted from N/R/M echoes "I" instead of the sent cdflag
    "echoConvertedCdflag": False,
    "ownBankPrefix": "HDFC",
    "stepsPerInquiry": 1,
    "latencyMs": 0,
    # empty = accept any; set to pin the credentials the mock accepts
    "consumerKey": "",
    "consumerSecret": "",
    "apiKey": "",
    "maxItems": 500,
}

_lock = threading.RLock()
_state = {
    "config": dict(DEFAULT_CONFIG),
    "tokens": {},  # token -> issued epoch
    "batches": {},  # batchnum -> batch
    "byFile": {},  # "filerefno|dd/mm/yyyy" -> batchnum
    "faults": {},  # api -> [fault, ...]
    "seq": 0,
}

_IST = timezone(timedelta(hours=5, minutes=30))
_ENQUIRE_AGAIN = "We have accepted your request. Please enquire again after sometime"


# ── crypto (lazy: the mock server must still boot on an interpreter without `cryptography`) ──

_keys = None


def _crypto():
    from cryptography.hazmat.primitives import hashes, serialization
    from cryptography.hazmat.primitives.asymmetric import padding, rsa
    from cryptography.hazmat.primitives.ciphers.aead import AESGCM

    return hashes, serialization, padding, rsa, AESGCM


def ensure_keys(keys_dir=KEYS_DIR):
    """Generate the test key kit if missing. Returns the directory.

    bank_priv.pem / bank_pub.pem      the "bank" pair: decrypts requests, signs responses
    client_priv.pem / client_pub.pem  our pair: signingPrivateKey (also clientKeyPem)
    ca.pem / client.crt               self-signed CA and a client leaf for the mTLS manager; plain
                                      HTTP never handshakes, but the manager must still parse them
    The CA's private key is not kept: nothing needs to sign with it again.
    """
    if os.path.exists(os.path.join(keys_dir, "client.crt")):
        return keys_dir
    hashes, serialization, _padding, rsa, _ = _crypto()
    from cryptography import x509
    from cryptography.x509.oid import NameOID

    os.makedirs(keys_dir, exist_ok=True)

    def new_key():
        return rsa.generate_private_key(public_exponent=65537, key_size=2048)

    def write(name, data):
        with open(os.path.join(keys_dir, name), "wb") as f:
            f.write(data)

    def priv_pem(k):
        return k.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())

    def pub_pem(k):
        return k.public_key().public_bytes(serialization.Encoding.PEM, serialization.PublicFormat.SubjectPublicKeyInfo)

    bank, client, ca_key = new_key(), new_key(), new_key()
    now = datetime.now(timezone.utc)
    ca_name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "HDFC CBX Mock CA")])
    ca_cert = (
        x509.CertificateBuilder()
        .subject_name(ca_name)
        .issuer_name(ca_name)
        .public_key(ca_key.public_key())
        .serial_number(x509.random_serial_number())
        .not_valid_before(now - timedelta(days=1))
        .not_valid_after(now + timedelta(days=3650))
        .add_extension(x509.BasicConstraints(ca=True, path_length=None), critical=True)
        .sign(ca_key, hashes.SHA256())
    )
    leaf = (
        x509.CertificateBuilder()
        .subject_name(x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "hdfc-cbx-mock-client")]))
        .issuer_name(ca_name)
        .public_key(client.public_key())
        .serial_number(x509.random_serial_number())
        .not_valid_before(now - timedelta(days=1))
        .not_valid_after(now + timedelta(days=3650))
        .add_extension(x509.SubjectAlternativeName([x509.DNSName("localhost")]), critical=False)
        .sign(ca_key, hashes.SHA256())
    )
    write("bank_priv.pem", priv_pem(bank))
    write("bank_pub.pem", pub_pem(bank))
    write("client_priv.pem", priv_pem(client))
    write("client_pub.pem", pub_pem(client))
    write("ca.pem", ca_cert.public_bytes(serialization.Encoding.PEM))
    write("client.crt", leaf.public_bytes(serialization.Encoding.PEM))
    log.info(f"HDFC mock: generated test key kit in {keys_dir}")
    return keys_dir


def _load_keys():
    global _keys
    if _keys is None:
        _hashes, serialization, _padding, _rsa, _ = _crypto()
        ensure_keys()

        def read(name):
            with open(os.path.join(KEYS_DIR, name), "rb") as f:
                return f.read()

        _keys = {
            "bank_priv": serialization.load_pem_private_key(read("bank_priv.pem"), password=None),
            "client_pub": serialization.load_pem_public_key(read("client_pub.pem")),
        }
    return _keys


def _b64u(b):
    return base64.urlsafe_b64encode(b).rstrip(b"=").decode()


def _unb64u(s):
    s = s.encode() if isinstance(s, str) else s
    return base64.urlsafe_b64decode(s + b"=" * (-len(s) % 4))


def kid_of(public_key):
    """base64url(SHA-256(SubjectPublicKeyInfo DER)) -- Jose.kidOf."""
    _h, serialization, _p, _r, _ = _crypto()
    der = public_key.public_bytes(serialization.Encoding.DER, serialization.PublicFormat.SubjectPublicKeyInfo)
    return _b64u(hashlib.sha256(der).digest())


def _compact_json(obj):
    return json.dumps(obj, separators=(",", ":")).encode()


def sign_jws(priv, payload):
    hashes, _s, padding, _r, _ = _crypto()
    header = _b64u(_compact_json({"typ": "JWT", "alg": "RS256", "kid": kid_of(priv.public_key())}))
    signing_input = f"{header}.{_b64u(payload)}".encode()
    sig = priv.sign(signing_input, padding.PKCS1v15(), hashes.SHA256())
    return f"{signing_input.decode()}.{_b64u(sig)}"


def verify_jws(pub, compact):
    hashes, _s, padding, _r, _ = _crypto()
    parts = compact.split(".")
    if len(parts) != 3:
        raise ValueError("JWS: expected 3 parts")
    header = json.loads(_unb64u(parts[0]))
    if header.get("alg") != "RS256":
        raise ValueError(f"JWS: unsupported alg {header.get('alg')}")
    pub.verify(_unb64u(parts[2]), f"{parts[0]}.{parts[1]}".encode(), padding.PKCS1v15(), hashes.SHA256())
    return _unb64u(parts[1])


def encrypt_jwe(pub, plaintext):
    hashes, _s, padding, _r, AESGCM = _crypto()
    header = _b64u(_compact_json({"alg": "RSA-OAEP-256", "enc": "A256GCM", "kid": kid_of(pub)}))
    cek, iv = os.urandom(32), os.urandom(12)
    wrapped = pub.encrypt(cek, padding.OAEP(mgf=padding.MGF1(hashes.SHA256()), algorithm=hashes.SHA256(), label=None))
    ct_tag = AESGCM(cek).encrypt(iv, plaintext, header.encode())
    return ".".join([header, _b64u(wrapped), _b64u(iv), _b64u(ct_tag[:-16]), _b64u(ct_tag[-16:])])


def decrypt_jwe(priv, compact):
    hashes, _s, padding, _r, AESGCM = _crypto()
    parts = compact.strip().split(".")
    if len(parts) != 5:
        raise ValueError("JWE: expected 5 parts")
    header = json.loads(_unb64u(parts[0]))
    if header.get("alg") != "RSA-OAEP-256" or header.get("enc") != "A256GCM":
        raise ValueError(f"JWE: unsupported alg/enc {header.get('alg')}/{header.get('enc')}")
    cek = priv.decrypt(_unb64u(parts[1]), padding.OAEP(mgf=padding.MGF1(hashes.SHA256()), algorithm=hashes.SHA256(), label=None))
    return AESGCM(cek).decrypt(_unb64u(parts[2]), _unb64u(parts[3]) + _unb64u(parts[4]), parts[0].encode())


def open_request(body):
    """JWE(bank) -> JWS(client) -> dict."""
    keys = _load_keys()
    text = body.decode() if isinstance(body, bytes) else body
    inner = decrypt_jwe(keys["bank_priv"], text).decode()
    return json.loads(verify_jws(keys["client_pub"], inner))


def seal_response(obj):
    """dict -> JWS(bank) -> JWE(client)."""
    keys = _load_keys()
    signed = sign_jws(keys["bank_priv"], _compact_json(obj))
    return encrypt_jwe(keys["client_pub"], signed.encode())


def _decode_for_overrides(body):
    try:
        return open_request(body)
    except Exception:
        return None


register_body_decoder("hdfc_cbx", _decode_for_overrides)


# ── persistence ──


def _load_state():
    try:
        with open(STATE_FILE) as f:
            saved = json.load(f)
        for k in ("tokens", "batches", "byFile", "faults", "seq"):
            if k in saved:
                _state[k] = saved[k]
        _state["config"] = {**DEFAULT_CONFIG, **{k: v for k, v in saved.get("config", {}).items() if k in DEFAULT_CONFIG}}
        log.info(f"HDFC mock: restored {len(_state['batches'])} batches from {STATE_FILE}")
    except FileNotFoundError:
        pass
    except Exception as e:
        log.warning(f"HDFC mock: could not restore state from {STATE_FILE}: {e}")


def _save_state():
    try:
        os.makedirs(os.path.dirname(STATE_FILE), exist_ok=True)
        tmp = STATE_FILE + ".tmp"
        with open(tmp, "w") as f:
            json.dump(_state, f, indent=1)
        os.replace(tmp, STATE_FILE)
    except Exception as e:
        log.warning(f"HDFC mock: could not persist state: {e}")


_load_state()


# ── replies ──
#
# Every API computes a reply rather than sending one, so a fault can replace the answer after the
# request has been acted on (a 5xx after the file was accepted is exactly the case that matters).


def Sealed(obj):
    return ("sealed", obj)


def Problem(status, code, reason):
    return ("problem", status, code, reason)


def _send(handler, reply, plaintext_req=None):
    if reply[0] == "problem":
        _, status, code, reason = reply
        body = {"title": "Accepted" if status == 202 else "Error", "status": status, "errors": [{"code": code, "reason": reason}]}
        handler._raw(status, "application/problem+json", json.dumps(body))
    elif reply[0] == "json":
        handler._json(reply[1], reply[2] if len(reply) > 2 else 200)
    else:
        handler._raw(200, "application/jose", seal_response(reply[1]))
        # Show plaintext in the test dashboard's hit log instead of ciphertext.
        handler._captured_response_body = reply[1]
    if plaintext_req is not None:
        handler._hit_request_body = json.dumps(plaintext_req).encode()


# Faults any API can take. `process: true` acts on the request first and then answers with the
# fault -- e.g. a payment 5xx after the file was accepted, which the caller must recover by getBatchNo.
GENERIC_FAULTS = {
    "5xx": (500, "TH99500", "Backend Service Provided Unexpected Response"),
    "4xx": (400, "TH99400", "Bad Request"),
    "401": (401, "TH99401", "Invalid API Key"),
    "problem": (400, "", ""),
}
API_FAULTS = {
    "token": set(),
    "payment": {"duplicate", "accept-no-batchnum", "reject", "unknown-ack"},
    "inquiry": {"notready", "ndf"},
    "batchnum": {"norecord", "unknown"},
}


def _fault_reply(fault):
    status, code, reason = GENERIC_FAULTS[fault["kind"]]
    return Problem(int(fault.get("status", status)), fault.get("code", code), fault.get("reason", reason))


def _matches(req, key, value):
    """A request field equals `value`. `accno` instead matches a file with any item for that account, so a
    payment fault can target one driver's file while other files are being sent."""
    req = req or {}
    if key == "accno" and "accno" not in req:
        return any(str(t.get("accno")) == str(value) for t in req.get("trans") or [])
    return str(req.get(key)) == str(value)


def _take_fault(api, req=None):
    """The first queued fault for `api` whose optional `match` ({field: value} on the decrypted request,
    e.g. {"batchnum": ..}, or {"accno": ..} for a payment file with that account) fits this call. Scoped
    faults let several batches run different scenarios at once."""
    with _lock:
        queue = _state["faults"].get(api) or []
        for i, fault in enumerate(queue):
            match = fault.get("match") or {}
            if all(_matches(req, k, v) for k, v in match.items()):
                queue.pop(i)
                _save_state()
                log.info(f"HDFC mock: injecting fault on {api}: {fault}")
                return fault
        return None


def _with_fault(api, fault, compute):
    """Run `compute(fault)` under a one-shot fault and return the reply to send.

    timeout   sleep `seconds` (default 65, beyond the client's 60 s), then act normally unless
              `process: false`, which answers 504 without acting
    generic   answer with the fault; with `process: true`, act on the request first
    specific  handled inside `compute`
    """
    kind = fault.get("kind") if fault else None
    if kind == "timeout":
        time.sleep(float(fault.get("seconds", 65)))
        if not fault.get("process", True):
            return Problem(504, "TH99504", "Gateway Timeout")
        return compute(None)
    if kind in GENERIC_FAULTS:
        if fault.get("process"):
            compute(None)
        return _fault_reply(fault)
    return compute(fault)


# ── entry point ──


def handle(handler, path, body):
    sub = path[path.index("/hdfc") + len("/hdfc"):] or "/"
    try:
        _crypto()
    except ImportError:
        return _send(handler, Problem(500, "MOCK", "hdfc mock needs the python `cryptography` package (see nix/services/nammayatri.nix mock-server)"))

    if sub.startswith("/mock/"):
        return _control(handler, sub, body)

    latency = _state["config"].get("latencyMs") or 0
    if latency:
        time.sleep(latency / 1000.0)

    if sub == "/auth/oauth/v1/token":
        if handler.command != "POST":
            return _send(handler, Problem(405, "TH99405", "Method Not Allowed"))
        return _send(handler, _with_fault("token", _take_fault("token"), lambda _f: _token(handler)))

    api = {
        "/api/v1/cbx-nodal-bulkPayment": "payment",
        "/api/v1/cbx-nodal-bulkPaymentInq": "inquiry",
        "/api/v2/cbx-getBatchNo": "batchnum",
    }.get(sub)
    if api is None:
        return _send(handler, Problem(404, "TH99404", f"Resource not found: {sub}"))
    if handler.command != "POST":
        return _send(handler, Problem(405, "TH99405", "Method Not Allowed"))

    denied = _authorise(handler)
    if denied:
        return _send(handler, denied)
    try:
        req = open_request(body)
    except Exception as e:
        log.warning(f"HDFC mock: envelope rejected on {api}: {e}")
        return _send(handler, Problem(401, "TH99401", f"Unable to decrypt/verify request: {e}"))
    log.info(f"HDFC mock {api} request: {json.dumps(req)[:2000]}")

    compute = {"payment": _payment, "inquiry": _inquiry, "batchnum": _batchnum}[api]
    reply = _with_fault(api, _take_fault(api, req), lambda f: compute(req, f))
    _send(handler, reply, req)


def _token(handler):
    cfg = _state["config"]
    query = parse_qs(urlparse(handler.path).query)
    # UAT answers this when the form content type is absent, even though the body is empty
    if "application/x-www-form-urlencoded" not in handler.headers.get("Content-Type", ""):
        return Problem(400, "invalid_request", "Missing or duplicate parameters")
    if query.get("grant_type", [""])[0] != "client_credentials" or not query.get("scope", [""])[0]:
        return Problem(400, "invalid_request", "Missing or duplicate parameters")
    auth = handler.headers.get("Authorization", "")
    try:
        user, _, password = base64.b64decode(auth[6:]).decode().partition(":") if auth.startswith("Basic ") else ("", "", "")
    except Exception:
        user, password = "", ""
    if not user or (cfg["consumerKey"] and user != cfg["consumerKey"]) or (cfg["consumerSecret"] and password != cfg["consumerSecret"]):
        return Problem(401, "invalid_client", "Client authentication failed")
    token = uuid.uuid4().hex
    with _lock:
        now = time.time()
        _state["tokens"] = {t: ts for t, ts in _state["tokens"].items() if now - ts < 900}
        _state["tokens"][token] = now
        _save_state()
    # expires_in as a JSON string, as UAT sends it
    return ("json", {"access_token": token, "token_type": "Bearer", "expires_in": "899", "scope": query["scope"][0]})


def _authorise(handler):
    """A refusal, or None when the call may proceed."""
    cfg = _state["config"]
    apikey = handler.headers.get("apikey", "")
    if not apikey or (cfg["apiKey"] and apikey != cfg["apiKey"]):
        return Problem(401, "TH99401", "Invalid API Key")
    if not handler.headers.get("Scope"):
        return Problem(401, "TH99401", "Missing Scope")
    if not handler.headers.get("transactionId"):
        return Problem(400, "TH99400", "Missing transactionId")
    auth = handler.headers.get("Authorization", "")
    issued = _state["tokens"].get(auth[7:] if auth.startswith("Bearer ") else "")
    if issued is None or time.time() - issued > 900:
        return Problem(401, "TH99401", "Invalid or expired access token")
    return None


# ── bulk payment ──

_IFSC_RE = re.compile(r"^[A-Z]{4}0[A-Z0-9]{6}$")
_AMOUNT_RE = re.compile(r"^\d+(\.\d{1,2})?$")


def _validate_file(req, cfg):
    trans = req.get("trans") or []
    try:
        nooftran = int(str(req.get("nooftran", "")))
    except ValueError:
        nooftran = -1
    if not req.get("clientcode") or not req.get("groupid") or not req.get("iduser"):
        return "clientcode, groupid and iduser are mandatory"
    if not re.fullmatch(r"\d{6}", str(req.get("filerefno", ""))):
        return "File reference number should be 6 numeric characters"
    if not trans:
        return "No transactions in request"
    if nooftran != len(trans):
        return f"Number of transactions mismatch: nooftran {req.get('nooftran')} vs {len(trans)} records"
    if len(trans) > int(cfg["maxItems"]):
        return f"Maximum {cfg['maxItems']} transactions allowed per file"
    refs = set()
    for i, t in enumerate(trans, 1):
        ref = t.get("custrefno") or ""
        if t.get("cdflag") not in ("I", "N", "R", "M"):
            return f"Record {i}: invalid cdflag {t.get('cdflag')!r}"
        if not _AMOUNT_RE.fullmatch(str(t.get("amount", ""))):
            return f"Record {i}: invalid amount {t.get('amount')!r}"
        if not ref:
            return f"Record {i}: custrefno is mandatory"
        if len(ref) > 20:
            return f"Record {i}: custrefno Length should not be more then 20"
        if ref in refs:
            return f"Record {i}: duplicate custrefno {ref} in file"
        if not t.get("accno"):
            return f"Record {i}: accno is mandatory"
        if not re.fullmatch(r"\d{2}/\d{2}/\d{4}", t.get("reqdexctndt") or ""):
            return f"Record {i}: reqdexctndt should be dd/mm/yyyy"
        refs.add(ref)
    return None


def _payment(req, fault):
    cfg = _state["config"]
    trans = req.get("trans") or []
    filerefno = str(req.get("filerefno", ""))
    date = trans[0].get("reqdexctndt", "") if trans else ""
    kind = fault.get("kind") if fault else None

    def ack(codstatus, txtstatus, batchnum=None):
        return Sealed(
            {
                "clientcode": req.get("clientcode"),
                "groupid": req.get("groupid"),
                "iduser": req.get("iduser"),
                "filerefno": filerefno,
                "nooftran": str(len(trans)),
                "codstatus": codstatus,
                "txtstatus": txtstatus,
                "batchnum": batchnum,
            }
        )

    if kind == "duplicate":
        return ack("1", "Duplicate File Reference Number")
    if kind == "reject":
        return ack("1", fault.get("reason") or "File rejected: invalid debit account")
    unknown_code = fault.get("codstatus", "2") if kind == "unknown-ack" else None
    unknown_reason = (fault or {}).get("reason") or "Request under review"
    if kind == "unknown-ack" and not fault.get("process"):
        return ack(unknown_code or None, unknown_reason)
    error = _validate_file(req, cfg)
    if error:
        return ack("1", error)

    with _lock:
        key = f"{filerefno}|{date}"
        if key in _state["byFile"]:
            return ack("1", f"Duplicate File Reference Number {filerefno}", _state["byFile"][key])
        _state["seq"] += 1
        # 30 characters like a real batch number, but a prefix no real CBX domain uses
        batchnum = f"MOCKCBX{datetime.now(_IST):%Y%m%d}{_state['seq']:015d}"
        _state["batches"][batchnum] = {
            "batchnum": batchnum,
            "filerefno": filerefno,
            "reqdexctndt": date,
            "clientcode": req.get("clientcode"),
            "groupid": req.get("groupid"),
            "iduser": req.get("iduser"),
            "submittedAt": time.time(),
            "inquiryCalls": 0,
            "approved": bool(cfg["autoApprove"]),
            "items": [_new_item(t, cfg) for t in trans],
        }
        _state["byFile"][key] = batchnum
        _save_state()
    log.info(f"HDFC mock: accepted file {filerefno} ({date}) as {batchnum} with {len(trans)} items")
    if kind == "accept-no-batchnum":
        return ack("0", "Success")
    if kind == "unknown-ack":
        return ack(unknown_code or None, unknown_reason)
    return ack("0", "Success", batchnum)


def _new_item(t, cfg):
    ifsc = (t.get("ifsc") or "").upper()
    sent = t.get("cdflag")
    own = (cfg.get("ownBankPrefix") or "").upper()
    intra = sent == "I" or (own and ifsc.startswith(own))
    accno = t.get("accno") or ""
    name = t.get("name") or ""

    reject = None
    if len(name) > 40 and sent in ("N", "R"):
        reject = "Beneficiary name length should not be more than 40"
    elif not _IFSC_RE.fullmatch(ifsc):
        reject = "Invalid IFSC code"
    elif sent == "I" and own and not ifsc.startswith(own):
        reject = "Invalid IFSC for internal fund transfer"

    scenario = "normal"
    # TODO: let a test pick an item's outcome through the server's POST /mock/override (as the idfy and cmrl
    #  mocks do, keyed on accno or custrefno), instead of only these hard-coded account-number suffixes.
    for suffix, name_ in (("9001", "reject"), ("9002", "return"), ("9003", "deemed"), ("9004", "stuckC"), ("9005", "stuckP"), ("9006", "unknownCod"), ("9007", "unknownRbi")):
        if accno.endswith(suffix):
            scenario = name_
    if reject:
        scenario = "invalid"

    return {
        "custrefno": t.get("custrefno"),
        "cdflag": sent,
        "echoCdflag": "I" if intra and cfg.get("echoConvertedCdflag") else sent,
        "accno": accno,
        "name": name,
        "amount": t.get("amount"),
        "ifsc": ifsc,
        "reqdexctndt": t.get("reqdexctndt"),
        "intra": bool(intra),
        "scenario": scenario,
        "rejectReason": reject,
        "step": -1,  # nothing shown before the first answered inquiry
        "forced": None,
        "refno": "",
        "bankrefno": "",
    }


# ── item ladder ──


def _ladder(item):
    """Successive (codstatus, rbistatus, txtreason, rbireason) states for an item."""
    s = item["scenario"]
    if s == "invalid":
        return [("R", None, item["rejectReason"], None)]
    if s == "reject":
        return [("P", None, "Pending Approval", None), ("R", None, "Invalid beneficiary account", None)]
    if s == "stuckP":
        return [("P", None, "Pending Approval", None)]
    if s == "stuckC":
        return [("P", None, "Pending Approval", None), ("C", None, "In Process", None)]
    if s == "unknownCod":
        return [("P", None, "Pending Approval", None), ("X", None, "Unmapped status", None)]
    if s == "unknownRbi":
        return [("P", None, "Pending Approval", None), ("C", None, "In Process", None), ("E", "TXFOO", "Completed", None)]
    if item["intra"]:
        return [("P", None, "Pending Approval", None), ("C", None, "In Process", None), ("E", None, "Completed", None)]
    final = {
        "return": ("E", "TXREJE", "Completed", "Beneficiary account closed"),
        "deemed": ("E", "TXDSETT", "Completed", None),
    }.get(s, ("E", "TXSETT", "Completed", None))
    return [
        ("P", None, "Pending Approval", None),
        ("C", None, "In Process", None),
        ("E", None, "Completed", None),
        ("E", "TXSIP", "Completed", None),
        final,
    ]


def _advance(batch, cfg):
    for item in batch["items"]:
        if item["forced"]:
            continue
        ladder = _ladder(item)
        cur = item["step"]
        if cur >= 0 and ladder[cur][0] == "P" and not batch["approved"]:
            continue  # held in the checker's queue
        nxt = cur + max(1, int(cfg.get("stepsPerInquiry") or 1))
        if not batch["approved"]:
            # never run past P while unapproved
            first_p = next((i for i, st in enumerate(ladder) if st[0] == "P"), None)
            if first_p is not None:
                nxt = min(nxt, first_p)
        item["step"] = min(nxt, len(ladder) - 1)
        _assign_refs(item, ladder[item["step"]])


def _assign_refs(item, st):
    codstatus, rbistatus = st[0], st[1]
    if codstatus == "E" and item["intra"] and not item["bankrefno"]:
        item["bankrefno"] = "FT" + str(uuid.uuid4().int)[:14]
    if rbistatus in ("TXSETT", "TXDSETT", "TXREJE") and not item["intra"] and not item["refno"]:
        digits = str(uuid.uuid4().int)
        if item["cdflag"] == "M":
            item["refno"] = digits[:12]  # IMPS RRN
        elif item["cdflag"] == "R":
            item["refno"] = "HDFCR5" + datetime.now(_IST).strftime("%Y%m%d") + digits[:8]
        else:
            item["refno"] = "HDFCN5" + digits[:10]


def _row(item):
    if item["forced"]:
        f = item["forced"]
        codstatus, rbistatus, txtreason, rbireason = f.get("codstatus"), f.get("rbistatus"), f.get("txtreason", ""), f.get("rbireason")
        refno, bankrefno = f.get("refno", item["refno"]), f.get("bankrefno", item["bankrefno"])
    else:
        codstatus, rbistatus, txtreason, rbireason = _ladder(item)[item["step"]]
        refno, bankrefno = item["refno"], item["bankrefno"]
    return {
        "cdflag": item["echoCdflag"],
        "custrefno": item["custrefno"],
        "accno": item["accno"],
        "name": item["name"],
        "amount": item["amount"],
        "ifsc": item["ifsc"],
        "micr": "",
        "reqdexctndt": item["reqdexctndt"],
        "codstatus": codstatus,
        "txtreason": txtreason or "",
        "refno": refno or "",
        "bankrefno": bankrefno or "",
        "rbistatus": rbistatus,
        "rbireason": rbireason,
    }


# ── inquiry ──


def _inquiry(req, fault):
    cfg = _state["config"]
    kind = fault.get("kind") if fault else None
    batchnum = req.get("batchnum", "")
    if kind == "notready":
        return Problem(202, "0", _ENQUIRE_AGAIN)

    ndf = Sealed({"gcif": req.get("gcif"), "iduser": req.get("iduser"), "batchnum": batchnum, "codstatus": "NDF", "message": "No data found", "trans": None})
    if kind == "ndf":
        return ndf
    with _lock:
        batch = _state["batches"].get(batchnum)
        if batch is None or (req.get("filerefno") and req["filerefno"] != batch["filerefno"]):
            return ndf
        batch["inquiryCalls"] += 1
        if cfg["enquireAgainFirst"]:
            now = time.time()
            since = batch.get("processingSince")
            if since is None:
                # first hit: HDFC accepts the inquiry and starts working on it
                batch["processingSince"] = now
                _save_state()
                return Problem(202, "0", _ENQUIRE_AGAIN)
            if now - since < float(cfg["readyAfterSeconds"]):
                # asked again too soon: still being processed
                _save_state()
                return Problem(202, "0", _ENQUIRE_AGAIN)
            # the answer is ready: give it, and the next check starts over
            batch["processingSince"] = None
        _advance(batch, cfg)
        rows = [_row(i) for i in batch["items"]]
        _save_state()
    return Sealed(
        {
            "gcif": batch["groupid"],
            "iduser": batch["iduser"],
            "batchnum": batchnum,
            "filerefno": batch["filerefno"],
            "nooftran": str(len(rows)),
            "codstatus": "0",
            "message": "Success",
            "trans": rows,
        }
    )


# ── batch-number recovery ──


def _batchnum(req, fault):
    base = {"gcif": req.get("gcif"), "iduser": req.get("iduser"), "filerefno": req.get("filerefno")}
    batchnum = _state["byFile"].get(f"{req.get('filerefno')}|{req.get('reqdexctndt')}")
    if fault and fault.get("kind") == "unknown":
        return Sealed({**base, "batchnum": None, "codstatus": fault.get("code") or "9", "message": fault.get("reason") or "Unable to process request"})
    if (fault and fault.get("kind") == "norecord") or not batchnum:
        return Sealed({**base, "batchnum": None, "codstatus": "1", "message": "No record found"})
    return Sealed({**base, "batchnum": batchnum, "codstatus": "0", "message": "Success"})


# ── control API ──


def _control(handler, sub, body):
    try:
        payload = json.loads(body) if body else {}
    except ValueError:
        return handler._json({"error": "body must be JSON"}, 400)

    if sub == "/mock/state":
        with _lock:
            return handler._json(_state)
    if sub == "/mock/reset" and handler.command == "POST":
        with _lock:
            _state.update({"tokens": {}, "batches": {}, "byFile": {}, "faults": {}})
            _save_state()
        return handler._json({"ok": True})
    if sub == "/mock/config":
        if handler.command == "POST":
            unknown = sorted(set(payload) - set(DEFAULT_CONFIG))
            if unknown:
                return handler._json({"error": f"unknown config keys {unknown}", "known": sorted(DEFAULT_CONFIG)}, 400)
            with _lock:
                _state["config"].update(payload)
                _save_state()
        return handler._json(_state["config"])
    if sub == "/mock/faults/clear" and handler.command == "POST":
        with _lock:
            _state["faults"] = {}
            _save_state()
        return handler._json({"ok": True})
    if sub == "/mock/fault" and handler.command == "POST":
        api, kind = payload.get("api"), payload.get("kind")
        if api not in API_FAULTS:
            return handler._json({"error": f"api must be one of {sorted(API_FAULTS)}"}, 400)
        allowed = {"timeout", *GENERIC_FAULTS, *API_FAULTS[api]}
        if kind not in allowed:
            return handler._json({"error": f"kind for {api} must be one of {sorted(allowed)}"}, 400)
        with _lock:
            _state["faults"].setdefault(api, []).append({k: v for k, v in payload.items() if k != "api"})
            _save_state()
        return handler._json({"ok": True, "pending": _state["faults"][api]})

    m = re.fullmatch(r"/mock/batch/([^/]+)/approve", sub)
    if m and handler.command == "POST":
        with _lock:
            batch = _state["batches"].get(m.group(1))
            if batch is None:
                return handler._json({"error": "unknown batchnum"}, 404)
            batch["approved"] = True
            _save_state()
        return handler._json({"ok": True})

    m = re.fullmatch(r"/mock/item/([^/]+)", sub)
    if m and handler.command == "POST":
        with _lock:
            for batch in _state["batches"].values():
                for item in batch["items"]:
                    if item["custrefno"] == m.group(1):
                        item["forced"] = payload or None
                        _save_state()
                        return handler._json({"ok": True, "batchnum": batch["batchnum"], "row": _row(item) if item["step"] >= 0 or item["forced"] else None})
        return handler._json({"error": "unknown custrefno"}, 404)

    return handler._json({"error": f"unknown control path {sub}"}, 404)
