"""Command line: python3 -m devbox_core <command> ...  (run from Backend/dev)."""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from typing import Callable

from . import cc, data, deploy, fleet, flow, ssh, stack, workspace


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
            "office_ip": m.get("localIp", ""), "overlay_ip": m.get("overlayIp", ""),
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
    """The dev-box a workspace would use: the least-loaded reachable one with a
    front desk (or --host, checked). --github-env also hands it to the next
    workflow steps as DEVBOX_HOST / DEVBOX_USER / DEVBOX_NAME, and the address
    its URLs are opened on (office network) as DEVBOX_URL_HOST."""
    os.environ["BASE_API_HOST"], os.environ["BASE_API_PORT"] = args.base_host, args.base_port
    try:
        t, name, url_host = flow.pick_target(args.user, args.host, None)
    except RuntimeError as e:
        print(json.dumps({"error": str(e)}))
        return 1
    if args.github_env:
        path = os.environ.get("GITHUB_ENV")
        if not path:
            print(json.dumps({"error": "--github-env needs GITHUB_ENV (only set inside GitHub Actions)"}))
            return 1
        with open(path, "a") as f:
            f.write(f"DEVBOX_HOST={t.host}\nDEVBOX_USER={t.user}\nDEVBOX_NAME={name}\n"
                    f"DEVBOX_URL_HOST={url_host}\n")
    print(json.dumps({"name": name, "host": t.host, "user": t.user, "url_host": url_host}))
    return 0


def cmd_registry(args: argparse.Namespace) -> int:
    try:
        reg = workspace.read_registry(_target(args))
    except RuntimeError as e:
        print(json.dumps({"error": str(e)}))
        return 1
    print(json.dumps(reg, indent=2))
    return 0


def _owner(args: argparse.Namespace) -> dict | None:
    if args.owner_user or args.owner_release:
        return {"by": args.owner_by, "user": args.owner_user, "release": args.owner_release}
    return None


def cmd_register(args: argparse.Namespace) -> int:
    try:
        entry = workspace.register(_target(args), args.name, _owner(args))
    except (ValueError, RuntimeError) as e:
        print(json.dumps({"error": str(e)}))
        return 1
    print(json.dumps(entry))
    return 0


def cmd_release(args: argparse.Namespace) -> int:
    try:
        removed = workspace.release(_target(args), args.name, _owner(args))
    except (ValueError, RuntimeError) as e:
        print(json.dumps({"error": str(e)}))
        return 1
    print(json.dumps({"released": removed} if removed else {"released": False, "note": "already gone"}))
    return 0


def cmd_sync(args: argparse.Namespace) -> int:
    try:
        res = deploy.sync(args.src, _target(args), args.name, _owner(args), dry_run=args.dry_run)
    except (ValueError, RuntimeError) as e:
        print(json.dumps({"error": str(e)}))
        return 1
    print(json.dumps(res))
    return 0


def _stack_cmd(fn: Callable[[argparse.Namespace, ssh.Target, str], dict]):
    def run(args: argparse.Namespace) -> int:
        try:
            res = fn(args, _target(args), workspace.workspace_dir(args.name))
        except (ValueError, RuntimeError) as e:
            print(json.dumps({"error": str(e)}))
            return 1
        print(json.dumps(res))
        return 0
    return run


def _stack_status(args, t, wdir) -> dict:
    res = stack.status(t, wdir)
    if args.tail:
        print(stack.tail(t, wdir, args.tail), end="")
    return res


def cmd_versions(args: argparse.Namespace) -> int:
    versions, err = data.published_versions(_target(args), args.source)
    if err:
        print(json.dumps({"error": err}))
        return 1
    pick = data.pick_version(versions)
    print(json.dumps({"source": args.source, "versions": versions,
                      "recommended": f"v{pick['version']}" if pick else None}, indent=2))
    return 0


def cmd_data(args: argparse.Namespace) -> int:
    t = _target(args)
    version = args.version
    try:
        if not version:
            versions, err = data.published_versions(t, args.source)
            if err:
                raise RuntimeError(err)
            pick = data.pick_version(versions)
            if not pick:
                raise RuntimeError(f"no usable {args.source} version published")
            version = f"v{pick['version']}"
            print(f"[data] latest safe {args.source}: {version}")
        ws = f"http://{t.host}:{stack.FRONTDESK_PORT}/{args.name}"
        res = data.sync_and_wait(ws, args.source, version, force=args.force)
    except (ValueError, RuntimeError) as e:
        print(json.dumps({"error": str(e)}))
        return 1
    print(json.dumps(res))
    return 0


def _cc_result(res: dict) -> int:
    if res.get("state") == "running":
        res.pop("log", None)   # only worth reading when something is wrong
    print(json.dumps(res))
    return 1 if res.get("error") else 0


def cmd_cc_start(args: argparse.Namespace) -> int:
    t, wdir = _target(args), workspace.workspace_dir(args.name)
    p = stack.ports(t, wdir)
    rider, driver = p.get("rider-app"), p.get("dynamic-offer-driver-app")
    if not (rider and driver):
        print(json.dumps({"error": "workspace ports not known yet — start the stack first"}))
        return 1
    info = {"branch": args.cc_branch, "commit": args.cc_commit}
    res = cc.start(t, wdir, rider, driver, info)
    if args.wait and not res.get("error"):
        try:
            res = cc.wait_running(t, wdir, timeout=args.timeout)
        except RuntimeError as e:
            res = {"error": str(e)}
    return _cc_result(res)


def cmd_result(args: argparse.Namespace) -> int:
    from . import output
    try:
        fields = flow.result_fields(_target(args), args.name, args.devbox, args.url_host)
    except RuntimeError as e:
        print(json.dumps({"error": str(e)}))
        return 1
    output.stage("ready", "done")
    output.result(fields)
    return 0


def cmd_cc_status(args: argparse.Namespace) -> int:
    return _cc_result(cc.status(_target(args), workspace.workspace_dir(args.name)))


def cmd_cc_stop(args: argparse.Namespace) -> int:
    return _cc_result(cc.stop(_target(args), workspace.workspace_dir(args.name)))


def cmd_spawn(args: argparse.Namespace) -> int:
    try:
        flow.spawn(args.name, args.src, _owner(args), host=args.host, user=args.user,
                   key=args.key, source=args.source, version=args.version,
                   cc_info={"branch": args.cc_branch, "commit": args.cc_commit},
                   ready_timeout=args.ready_timeout)
    except Exception as e:  # noqa: BLE001 — one clear line for the workflow log
        print(json.dumps({"error": str(e)}))
        return 1
    return 0


def cmd_stop(args: argparse.Namespace) -> int:
    try:
        res = flow.stop(_target(args), args.name, _owner(args))
    except (ValueError, RuntimeError) as e:
        print(json.dumps({"error": str(e)}))
        return 1
    print(json.dumps(res))
    return 0


def _add_owner_args(p: argparse.ArgumentParser) -> None:
    p.add_argument("--name", required=True, help="workspace name")
    p.add_argument("--owner-by", default="scc")
    p.add_argument("--owner-user", default="")
    p.add_argument("--owner-release", default="")


def _add_target_args(p: argparse.ArgumentParser) -> None:
    p.add_argument("--host", required=True)
    p.add_argument("--user", required=True)
    p.add_argument("--port", type=int, default=22)
    p.add_argument("--key", help="private key file (default: DEVBOX_SSH_KEY_FILE or ~/.ssh/id_*)")


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
                              ("pick", cmd_pick, "the least-loaded reachable dev-box with a front desk")):
        c = sub.add_parser(name, help=help_)
        c.add_argument("--base-host", default=base_host, help="service discovery host (env BASE_API_HOST)")
        c.add_argument("--base-port", default=base_port, help="service discovery port (env BASE_API_PORT)")
        if name == "pick":
            c.add_argument("--host", help="use this dev-box (its front desk is still checked)")
            c.add_argument("--user", help="SSH user for --host")
            c.add_argument("--github-env", action="store_true",
                           help="also write DEVBOX_HOST/USER/NAME to $GITHUB_ENV for the next steps")
        c.set_defaults(func=func)

    reg = sub.add_parser("registry", help="show the devbox's workspace registry")
    _add_target_args(reg)
    reg.set_defaults(func=cmd_registry)

    rg = sub.add_parser("register", help="add/update one workspace in the registry (locked)")
    _add_target_args(rg)
    _add_owner_args(rg)
    rg.set_defaults(func=cmd_register)

    rl = sub.add_parser("release", help="remove one workspace from the registry (owner must match)")
    _add_target_args(rl)
    _add_owner_args(rl)
    rl.set_defaults(func=cmd_release)

    sy = sub.add_parser("sync", help="register + rsync a checkout into the workspace + git init")
    _add_target_args(sy)
    _add_owner_args(sy)
    sy.add_argument("--src", required=True, help="nammayatri checkout to copy (its root folder)")
    sy.add_argument("--dry-run", action="store_true", help="list what would be copied; copy nothing")
    sy.set_defaults(func=cmd_sync)

    for name, help_, fn in (
        ("stack-start", "start the workspace's stack in the background",
         lambda a, t, w: stack.start(t, w)),
        ("stack-status", "running? which stage? (--tail N shows the last log lines)", _stack_status),
        ("stack-wait", "follow the stack until rider-app and driver-app answer",
         lambda a, t, w: stack.wait_ready(t, a.name, w, timeout=a.timeout, until=a.until)),
        ("stack-stop", "stop the stack and everything running in the workspace folder",
         lambda a, t, w: stack.stop(t, w)),
    ):
        c = sub.add_parser(name, help=help_)
        _add_target_args(c)
        c.add_argument("--name", required=True, help="workspace name")
        if name == "stack-status":
            c.add_argument("--tail", type=int, default=0)
        if name == "stack-wait":
            c.add_argument("--timeout", type=int, default=3600, help="seconds (default 3600)")
            c.add_argument("--until", choices=("services", "ready"), default="ready",
                           help="services = the build is over and services are starting; ready = they answer (default)")
        c.set_defaults(func=_stack_cmd(fn))

    vs = sub.add_parser("versions", help="published data versions (read through the devbox)")
    _add_target_args(vs)
    vs.add_argument("--from", dest="source", required=True, choices=data.SOURCES)
    vs.set_defaults(func=cmd_versions)

    dt = sub.add_parser("data", help="load published data into the workspace and wait")
    _add_target_args(dt)
    dt.add_argument("--name", required=True, help="workspace name")
    dt.add_argument("--from", dest="source", required=True, choices=data.SOURCES)
    dt.add_argument("--version", help="e.g. v3 (default: latest safe)")
    dt.add_argument("--force", action="store_true", help="load even if the workspace already has it")
    dt.set_defaults(func=cmd_data)

    for name, help_, func in (
        ("cc-start", "start the control centre next to the workspace (port 5300-5319)", cmd_cc_start),
        ("cc-status", "control centre state + URL (+ last log lines unless running)", cmd_cc_status),
        ("cc-stop", "stop the control centre", cmd_cc_stop),
    ):
        c = sub.add_parser(name, help=help_)
        _add_target_args(c)
        c.add_argument("--name", required=True, help="workspace name")
        if name == "cc-start":
            c.add_argument("--cc-branch", default="", help="control-center branch (shown in status)")
            c.add_argument("--cc-commit", default="", help="control-center commit (shown in status)")
            c.add_argument("--wait", action="store_true", help="wait until it answers")
            c.add_argument("--timeout", type=int, default=900, help="seconds for --wait (default 900)")
        c.set_defaults(func=func)

    rs = sub.add_parser("result", help="print DEVBOX_RESULT for a workspace (its URLs, data, control centre)")
    _add_target_args(rs)
    rs.add_argument("--name", required=True, help="workspace name")
    rs.add_argument("--devbox", default="", help="dev-box name to report (default: the host)")
    rs.add_argument("--url-host", default="",
                    help="address the URLs use, e.g. the office IP when --host is the overlay one (default: --host)")
    rs.set_defaults(func=cmd_result)

    sp = sub.add_parser("spawn", help="bring a whole workspace up: pick, sync, build, start, data, control centre")
    _add_owner_args(sp)
    sp.add_argument("--src", required=True, help="nammayatri checkout to copy (control-center/ inside it is used too)")
    sp.add_argument("--host", help="devbox address (default: pick the least-loaded reachable one)")
    sp.add_argument("--user", help="devbox SSH user (needed with --host)")
    sp.add_argument("--key", help="private key file (default: DEVBOX_SSH_KEY_FILE or ~/.ssh/id_*)")
    sp.add_argument("--from", dest="source", choices=data.SOURCES, help="data to load (default: none)")
    sp.add_argument("--version", help="data version, e.g. v14 (default: latest safe)")
    sp.add_argument("--cc-branch", default="")
    sp.add_argument("--cc-commit", default="")
    sp.add_argument("--ready-timeout", type=int, default=3600, help="seconds for build + start (default 3600)")
    sp.set_defaults(func=cmd_spawn)

    st = sub.add_parser("stop", help="remove a whole workspace: control centre, stack, folder, registry entry")
    _add_target_args(st)
    _add_owner_args(st)
    st.set_defaults(func=cmd_stop)

    args = p.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
