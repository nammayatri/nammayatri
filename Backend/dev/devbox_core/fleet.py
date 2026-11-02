"""Fleet helpers: list the machines service discovery knows, and pick a devbox.

The machines come from hopper's service discovery (GET <gateway>/node/list):
every node whose hopper-agent reported in the last 30s. A dev-box shows up
only when its hopper-agent was installed with node type "dev-box"
(HOPPER_NODE_TYPE in /etc/hopper-agent.env).

Moved from test-tool/local-api/server.py so the devbox workflow can choose a
devbox the same way the 7070 dashboard does.
"""
from __future__ import annotations

import ipaddress
import json
import re
import subprocess
import sys
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from typing import Callable

from . import ssh, stack


def _port_open(ip: str, port: int = 22) -> bool:
    return ssh.tcp_open(ip, port, timeout=1.5)


def _my_networks() -> list:
    """[(ip, network)] for this machine's own interfaces (loopback skipped)."""
    my_networks: list = []
    try:
        if sys.platform == "darwin":
            out = subprocess.check_output(["ifconfig"], text=True, timeout=3)
            # "inet <ip> netmask <hex>", plus the point-to-point form tunnels use:
            # "inet 10.4.1.13 --> 10.4.1.13 netmask 0xffffffe0". Skipping the
            # peer field lost every VPN interface, which made VPN addresses look
            # like a different network however wide the tunnel's mask was.
            for m in re.finditer(
                    r"inet (\d+\.\d+\.\d+\.\d+)(?: --> \d+\.\d+\.\d+\.\d+)? netmask (0x[0-9a-fA-F]+)",
                    out):
                ip_str, mask_hex = m.group(1), m.group(2)
                if ip_str == "127.0.0.1":
                    continue
                mask_int = int(mask_hex, 16)
                prefix = bin(mask_int).count("1")
                net = ipaddress.ip_network(f"{ip_str}/{prefix}", strict=False)
                my_networks.append((ip_str, net))
        else:
            out = subprocess.check_output(["hostname", "-I"], text=True, timeout=3).strip()
            for ip_str in out.split():
                ip_str = ip_str.strip()
                if ip_str:
                    # Default /24 for Linux (hostname -I doesn't give netmask)
                    net = ipaddress.ip_network(f"{ip_str}/24", strict=False)
                    my_networks.append((ip_str, net))
    except Exception:
        pass
    return my_networks


def _pct(value) -> str:
    """hopper's 14.2 -> "(14%)", the form ram_pct() and the dashboard read. "" when unknown."""
    try:
        return f"({float(value):.0f}%)"
    except (TypeError, ValueError):
        return ""


def fetch_machines(gateway: str = stack.GATEWAY_URL,
                   reachable: Callable[[str], bool] = _port_open) -> dict:
    """The machines hopper's service discovery knows, as MachineInfo dicts.

    Calls  GET <gateway>/node/list
    which returns the nodes whose hopper-agent reported in the last 30s:
      [{magic_dns_name, node_type, username, local_ip, overlay_ip,
        cpu_usage, mem_usage, disk_usage, updated_at}, …]

    Transforms that into:
      { "machines": [MachineInfo, …], "myIps": [...], "myNetworks": [...] }
    where MachineInfo = {name, role, localIp, overlayIp, bestIp, user, type, usage, …}

    `reachable(ip)` decides whether an address answers; callers that poll pass
    a cached version."""
    url = f"{gateway.rstrip('/')}/node/list"
    try:
        with urllib.request.urlopen(url, timeout=10) as r:
            nodes = json.loads(r.read().decode())
        if not isinstance(nodes, list):
            raise ValueError("not a list")
    except Exception as e:
        return {"machines": [], "myIps": [], "error": f"service discovery unreachable at {url}: {e}"}

    # Collect this machine's IPs + networks so we can pick the best route.
    my_networks = _my_networks()
    my_ips = [ip for ip, _ in my_networks]
    # Our own interfaces as CIDRs — the left-hand side of every same-subnet
    # comparison below, published so the mask a verdict used is on record.
    my_networks_cidr = [f"{ip}/{net.prefixlen}" for ip, net in my_networks]

    def _shared_subnet(ip: str) -> str:
        """CIDR of one of our own interfaces that contains ip, else "".

        This is the same-subnet rule — (mine AND mask) == (theirs AND mask) —
        evaluated through *our* mask, which is the only mask we know: discovery
        reports addresses without prefixes. A remote host configured with a wider
        mask can therefore consider us local while we consider it remote, so this
        answers "can I deliver directly?" and nothing more."""
        if not ip:
            return ""
        try:
            target = ipaddress.ip_address(ip)
        except ValueError:
            return ""
        for _, net in my_networks:
            if target in net:
                return str(net)
        return ""

    def _shape_machine(n: dict) -> dict:
        local_ip = n.get("local_ip") or ""
        overlay_ip = n.get("overlay_ip") or ""
        lan_subnet = _shared_subnet(local_ip)
        overlay_ok = bool(overlay_ip) and reachable(overlay_ip)
        # Skip the office probe when the overlay address already answered — same box.
        lan_ok = bool(local_ip) and (False if overlay_ok else reachable(local_ip))
        # The overlay address wins when it answers: it does not change when the
        # office IPs do, and dev-box keys may accept the runner only on it. Then
        # the local address. None → no route from here; the addresses are still
        # reported (for display) but bestIp/route stay empty so callers can
        # refuse to assign it.
        if overlay_ok:
            best, route = overlay_ip, "overlay"
        elif lan_ok:
            best, route = local_ip, "local"
        else:
            best, route = "", ""
        return {
            "name":        n.get("magic_dns_name") or "",
            # Service discovery lists only agents, never the base station.
            "role":        "worker",
            "localIp":     local_ip,
            "awsIp":       "",
            "overlayIp":   overlay_ip,
            "bestIp":      best,
            "localSubnet": lan_subnet,
            "vpnSubnet":   "",
            "route":       route,
            "overlayReachable": overlay_ok,
            "localReachable": lan_ok,
            "vpnReachable": False,
            "user":        n.get("username") or "ubuntu",
            "type":        n.get("node_type") or "",
            "resources":   {},
            "usage":       {"cpu": _pct(n.get("cpu_usage")), "ram": _pct(n.get("mem_usage")),
                            "storage": _pct(n.get("disk_usage"))},
        }

    machines: list = []
    raw_nodes = [n for n in nodes if isinstance(n, dict) and n.get("magic_dns_name")]

    # Probe the fleet concurrently: serially this would be
    # (machines × addresses × timeout) of dead air on every poll.
    if raw_nodes:
        with ThreadPoolExecutor(max_workers=min(8, len(raw_nodes) * 2)) as pool:
            machines = list(pool.map(_shape_machine, raw_nodes))

    return {"machines": machines, "myIps": my_ips,
            "myNetworks": my_networks_cidr}


def ram_pct(usage: dict) -> float:
    """'27.5Gi (88%)' -> 88.0. Unknown/missing -> 100.0 (least preferred)."""
    try:
        m = re.search(r"\((\d+(?:\.\d+)?)%\)", str((usage or {}).get("ram", "")))
        return float(m.group(1)) if m else 100.0
    except Exception:
        return 100.0


def devboxes(machines: list) -> list:
    """Only the dev-box workers (not the base station, not builders)."""
    return [m for m in machines
            if m.get("type") == "dev-box" and m.get("role") == "worker"]


def pick(machines: list) -> dict | None:
    """The least-loaded (lowest RAM %) machine that has a route from here, or None.

    A machine with no bestIp answered on neither address, so it is never picked
    however idle it is."""
    assignable = [m for m in machines if m.get("bestIp")]
    if not assignable:
        return None
    return min(assignable, key=lambda m: ram_pct(m.get("usage")))
