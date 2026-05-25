"""CAC (Superposition) remote-config mock.

Returns a flat config object that NammaYatri consumer/provider apps fetch
at boot to override baked-in defaults. Routes:

  /cac/consumer  → keys read by the rider app (consumerBaseUrl, ...)
  /cac/provider  → keys read by the driver app (grpc_config, rr_grpc_config, ...)
  /cac           → both merged (catch-all)

Override per request via /mock/override with service="cac" if you need to
flip a value during a test; otherwise the static defaults below are returned.
"""

from status_store import deep_merge

from ._env import RIDER_APP_PORT
from ._env import _port

NOTIFICATION_SERVICE_GRPC_PORT = _port("NOTIFICATION_SERVICE_GRPC_PORT", 50051)


CONSUMER_CONFIG = {
    "consumerBaseUrl": f"http://localhost:{RIDER_APP_PORT}/v2",
    "adConfigUrl": "",
    "enabled": True,
}

PROVIDER_CONFIG = {
    "grpc_config": {
        "enabled": True,
        "address": "localhost",
        "port": NOTIFICATION_SERVICE_GRPC_PORT,
        "restart_time_out": 10000,
    },
    "rr_grpc_config": {
        "enabled": True,
        "address": "localhost",
        "port": NOTIFICATION_SERVICE_GRPC_PORT,
        "restart_time_out": 10000,
    },
    "enabled": True,
}


def handle(handler, path, body):
    if "/cac/consumer" in path:
        base = dict(CONSUMER_CONFIG)
    elif "/cac/provider" in path:
        base = dict(PROVIDER_CONFIG)
    else:
        base = {**CONSUMER_CONFIG, **PROVIDER_CONFIG}

    override = getattr(handler, "_request_override", {}) or {}
    if override:
        base = deep_merge(base, override)

    handler._json(base)
