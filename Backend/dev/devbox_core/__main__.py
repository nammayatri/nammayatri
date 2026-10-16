"""Command line: python3 -m devbox_core <command> ...  (run from Backend/dev)."""
from __future__ import annotations

import argparse
import json
import subprocess
import sys

from . import ssh


def _target(args: argparse.Namespace) -> ssh.Target:
    return ssh.Target(user=args.user, host=args.host, port=args.port,
                      identity=args.key or ssh.find_key())


def cmd_ping(args: argparse.Namespace) -> int:
    t = _target(args)
    out = {"tcp22": ssh.tcp_open(t.host, t.port), "ssh": False}
    if out["tcp22"]:
        try:
            r = ssh.run(t, "true", timeout=15)
            out["ssh"] = r.returncode == 0
            if not out["ssh"]:
                out["error"] = (r.stderr or "").strip() or f"exit {r.returncode}"
        except subprocess.TimeoutExpired:
            out["error"] = "ssh timed out"
    print(json.dumps(out))
    return 0 if out["ssh"] else 1


def main(argv: list | None = None) -> int:
    p = argparse.ArgumentParser(prog="devbox_core")
    sub = p.add_subparsers(dest="command", required=True)

    ping = sub.add_parser("ping", help="check the devbox answers on SSH")
    ping.add_argument("--host", required=True)
    ping.add_argument("--user", required=True)
    ping.add_argument("--port", type=int, default=22)
    ping.add_argument("--key", help="private key file (default: DEVBOX_SSH_KEY_FILE or ~/.ssh/id_*)")
    ping.set_defaults(func=cmd_ping)

    args = p.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
