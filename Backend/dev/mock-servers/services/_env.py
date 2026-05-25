"""Shared port-from-env helpers for mock-server services.

The unified Python mock-server runs alongside the backend processes spawned
by `, run-mobility-stack-dev`. Under per-user port remapping
(Backend/nix/services/resolve-ports.sh), the parent nix-shell exports
RIDER_APP_PORT / DRIVER_APP_PORT / MOCK_SERVER_PORT so callback URLs
written back into mock responses point at the user's remapped ports
instead of the defaults.

Default fallbacks keep `python3 mock-servers/server.py --port 8080`
working when run outside nix shell.
"""

import os


def _port(name: str, default: int) -> int:
    try:
        return int(os.environ.get(name, str(default)))
    except ValueError:
        return default


RIDER_APP_PORT = _port("RIDER_APP_PORT", 8013)
DRIVER_APP_PORT = _port("DRIVER_APP_PORT", 8016)
MOCK_SERVER_PORT = _port("MOCK_SERVER_PORT", 8080)
