"""Fleet helpers: list the machines service discovery knows, and pick a devbox.

Moved from test-tool/local-api/server.py so the devbox workflow can choose a
devbox the same way the 7070 dashboard does.
"""
from __future__ import annotations

import ipaddress
import json
import os
import re
import subprocess
import sys
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from typing import Callable

from . import ssh

DEFAULT_BASE_API_HOST = "service-discovery.devenv.movingtech.net"
DEFAULT_BASE_API_PORT = "8787"


def base_api() -> tuple[str, str]:
    """(host, port) of the service discovery API, from env or the defaults."""
    return (os.environ.get("BASE_API_HOST", DEFAULT_BASE_API_HOST),
            os.environ.get("BASE_API_PORT", DEFAULT_BASE_API_PORT))


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


def _tailnet_ips() -> dict:
    """{hostname (lower case): overlay IPv4} for the tailnet peers this machine
    sees, from its own tailscale. Service discovery reports only office and AWS
    addresses; the overlay one is the one that survives a power cut (the office
    router hands out new IPs) and works from home. {} without tailscale."""
    try:
        out = subprocess.check_output(["tailscale", "status", "--json"], text=True,
                                      timeout=5, stderr=subprocess.DEVNULL)
        st = json.loads(out)
    except Exception:
        return {}
    ips: dict = {}
    for peer in (st.get("Peer") or {}).values():
        v4 = next((ip for ip in peer.get("TailscaleIPs") or [] if ":" not in ip), "")
        if v4 and peer.get("HostName"):
            ips[peer["HostName"].lower()] = v4
    return ips


def fetch_machines(base_host: str, base_port: str,
                   reachable: Callable[[str], bool] = _port_open) -> dict:
    """Fetch ServiceDiscovery from the base station HTTP API and return
    a list of MachineInfo dicts.

    Calls  GET http://<base_host>:<base_port>/api/status
    which returns:
      { "base": {localIp, awsIp, name, username, resources, usage},
        "workers": [{localIp, awsIp, name, username, type, resources, usage}, …] }

    Transforms that into:
      { "machines": [MachineInfo, …], "myIps": [...], "myNetworks": [...] }
    where MachineInfo = {name, role, localIp, awsIp, overlayIp, bestIp, user, type, resources}

    `reachable(ip)` decides whether an address answers; callers that poll pass
    a cached version."""
    if not base_host:
        return {"machines": [], "myIps": [],
                "error": "BASE_API_HOST not set — cannot reach base station API"}

    url = f"http://{base_host}:{base_port}/api/status"
    try:
        with urllib.request.urlopen(url, timeout=5) as r:
            data = json.loads(r.read().decode())
    except Exception as e:
        return {"machines": [], "myIps": [], "error": f"base station API unreachable: {e}"}

    # Collect this machine's IPs + networks so we can pick the best route.
    my_networks = _my_networks()
    my_ips = [ip for ip, _ in my_networks]
    # Our own interfaces as CIDRs — the left-hand side of every same-subnet
    # comparison below, published so the mask a verdict used is on record.
    my_networks_cidr = [f"{ip}/{net.prefixlen}" for ip, net in my_networks]
    tailnet = _tailnet_ips()

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

    def _shape_machine(m: dict, role: str, default_name: str) -> dict:
        local_ip = m.get("localIp", "")
        aws_ip = m.get("awsIp", "")
        overlay_ip = tailnet.get(m.get("name", default_name).lower(), "")
        lan_subnet = _shared_subnet(local_ip)
        vpn_subnet = _shared_subnet(aws_ip)
        overlay_ok = bool(overlay_ip) and reachable(overlay_ip)
        lan_ok = bool(local_ip) and reachable(local_ip)
        # Skip the VPN probe when the local address already answered — same box.
        vpn_ok = bool(aws_ip) and (False if lan_ok else reachable(aws_ip))
        # The overlay address wins when it answers: it does not change when the
        # office IPs do, and dev-box keys may accept the runner only on it. Then
        # the local address (no tunnel hop). None → no route from here; the
        # addresses are still reported (for display) but bestIp/route stay
        # empty so callers can refuse to assign it.
        if overlay_ok:
            best, route = overlay_ip, "overlay"
        elif lan_ok:
            best, route = local_ip, "local"
        elif vpn_ok:
            best, route = aws_ip, "vpn"
        else:
            best, route = "", ""
        return {
            "name":        m.get("name", default_name),
            "role":        role,
            "localIp":     local_ip,
            "awsIp":       aws_ip,
            "overlayIp":   overlay_ip,
            "bestIp":      best,
            "localSubnet": lan_subnet,
            "vpnSubnet":   vpn_subnet,
            "route":       route,
            "overlayReachable": overlay_ok,
            "localReachable": lan_ok,
            "vpnReachable": vpn_ok,
            "user":        m.get("username", "ubuntu"),
            "type":        m.get("type", ""),
            "resources":   m.get("resources", {}),
            "usage":       m.get("usage", {}),
        }

    machines: list = []
    raw_machines = [(data.get("base") or {}, "base", "base")] if data.get("base") else []
    raw_machines += [(w, "worker", "") for w in data.get("workers", [])]

    # Probe the fleet concurrently: serially this would be
    # (machines × addresses × timeout) of dead air on every poll.
    if raw_machines:
        with ThreadPoolExecutor(max_workers=min(8, len(raw_machines) * 2)) as pool:
            machines = list(pool.map(lambda a: _shape_machine(*a), raw_machines))

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
