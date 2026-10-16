"""Command line: python3 -m devbox_core <command> ...  (run from Backend/dev)."""
from __future__ import annotations

import argparse
import json
import subprocess
import sys

from . import fleet, ssh


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


def _summary(m: dict) -> dict:
    return {"name": m.get("name", ""), "host": m.get("bestIp", ""),
            "user": m.get("user", ""), "route": m.get("route", ""),
            "ram_pct": fleet.ram_pct(m.get("usage"))}


def _devboxes(args: argparse.Namespace) -> tuple[list, str | None]:
    res = fleet.fetch_machines(args.base_host, args.base_port)
    return fleet.devboxes(res.get("machines", [])), res.get("error")


def cmd_machines(args: argparse.Namespace) -> int:
    boxes, err = _devboxes(args)
    if err:
        print(json.dumps({"error": err}))
        return 1
    print(json.dumps([_summary(m) for m in boxes], indent=2))
    return 0


def cmd_pick(args: argparse.Namespace) -> int:
    boxes, err = _devboxes(args)
    if err:
        print(json.dumps({"error": err}))
        return 1
    m = fleet.pick(boxes)
    if m is None:
        print(json.dumps({"error": "no reachable dev-box"}))
        return 1
    print(json.dumps(_summary(m)))
    return 0


def main(argv: list | None = None) -> int:
    p = argparse.ArgumentParser(prog="devbox_core")
    sub = p.add_subparsers(dest="command", required=True)

    ping = sub.add_parser("ping", help="check the devbox answers on SSH")
    ping.add_argument("--host", required=True)
    ping.add_argument("--user", required=True)
    ping.add_argument("--port", type=int, default=22)
    ping.add_argument("--key", help="private key file (default: DEVBOX_SSH_KEY_FILE or ~/.ssh/id_*)")
    ping.set_defaults(func=cmd_ping)

    base_host, base_port = fleet.base_api()
    for name, func, help_ in (("machines", cmd_machines, "list the dev-boxes and their route from here"),
                              ("pick", cmd_pick, "the least-loaded reachable dev-box")):
        c = sub.add_parser(name, help=help_)
        c.add_argument("--base-host", default=base_host, help="service discovery host (env BASE_API_HOST)")
        c.add_argument("--base-port", default=base_port, help="service discovery port (env BASE_API_PORT)")
        c.set_defaults(func=func)

    args = p.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
