"""Mock ONDC buyer app (BAP) for the workbench ride flow (integration-tests/collections/OndcRideFlow).

The collection sends search/select/init/confirm to the local BPP with
bap_uri = <mock-server>/workbench-bap, so every on_* callback lands here:

  POST /workbench-bap/<on_action>                    store the callback, ACK
  GET  /workbench-bap/callbacks?txn=<id>&action=<a>  latest <a> callback for txn (404 until it arrives)
  GET  /workbench-bap/callbacks?txn=<id>             {action: [callbacks...]} for txn

Callbacks are kept in memory (last _MAX_TXNS transactions).
"""

import collections
import json
import threading
from urllib.parse import parse_qs, urlparse

_MAX_TXNS = 200
_CALLBACKS = collections.OrderedDict()  # txn -> {action: [payload, ...]}
_LOCK = threading.Lock()


def _store(payload, path):
    ctx = payload.get("context") or {}
    txn = ctx.get("transaction_id")
    action = ctx.get("action") or path.rstrip("/").rsplit("/", 1)[-1]
    with _LOCK:
        by_action = _CALLBACKS.pop(txn, {})
        by_action.setdefault(action, []).append(payload)
        _CALLBACKS[txn] = by_action
        while len(_CALLBACKS) > _MAX_TXNS:
            _CALLBACKS.popitem(last=False)


def handle(handler, path, body):
    if handler.command == "POST":
        raw = body.decode("utf-8") if isinstance(body, bytes) else body
        try:
            payload = json.loads(raw or "{}")
        except ValueError:
            return handler._json({"error": "invalid json"}, status=400)
        _store(payload, path)
        return handler._json({"message": {"ack": {"status": "ACK"}}})

    if path.rstrip("/").endswith("/callbacks"):
        q = parse_qs(urlparse(handler.path).query)
        txn, action = q.get("txn", [None])[0], q.get("action", [None])[0]
        with _LOCK:
            by_action = _CALLBACKS.get(txn, {})
            if action:
                hits = by_action.get(action)
                if not hits:
                    return handler._json({"error": "no %s yet for %s" % (action, txn)}, status=404)
                return handler._json(hits[-1])
            return handler._json(by_action)

    return handler._json({"status": "UP", "service": "workbench-bap"})
