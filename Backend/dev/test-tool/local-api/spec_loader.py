"""Launcher spec parsing + templating.

Reads YAML files from Backend/dev/test-tool/specs/*.yaml. Each spec describes
how to run an external repo as a launcher (ports, inputs, stages, workflows,
log sources, hooks). Templating substitutes ${ports.X}, ${inputs.X},
${source.ref}, ${destDir}, ${repoRoot} into shell commands and env values
*after* user inputs are supplied.
"""
from __future__ import annotations

import json
import os
import re
import threading
from pathlib import Path
from typing import Any, Dict, List, Optional

try:
    import yaml
except ImportError as _e:
    yaml = None  # type: ignore[assignment]
    _YAML_IMPORT_ERROR: Optional[Exception] = _e
else:
    _YAML_IMPORT_ERROR = None


SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent.parent.parent  # nammayatri/
SPECS_DIR = SCRIPT_DIR.parent / "specs"

_TEMPLATE_RE = re.compile(r"\$\{([^}]+)\}")
_ESCAPE_SENTINEL = "\x00__TT_DOLLAR_BRACE__\x00"


class SpecError(Exception):
    """Raised when a spec is invalid or templating fails."""


def _ensure_yaml() -> None:
    if yaml is None:
        raise SpecError(
            "PyYAML not available — run inside the nix shell that installs "
            f"pyyaml. import error: {_YAML_IMPORT_ERROR!r}"
        )


def _coerce_ports(raw: Any) -> List[dict]:
    out: List[dict] = []
    for p in raw or []:
        if not isinstance(p, dict) or "name" not in p or "port" not in p:
            raise SpecError(f"port entry must have name+port: {p!r}")
        out.append({
            "name": str(p["name"]),
            "port": int(p["port"]),
            "protocol": p.get("protocol", "http"),
            "allowOverride": bool(p.get("allowOverride", False)),
            "openInBrowser": bool(p.get("openInBrowser", False)),
        })
    return out


def _coerce_inputs(raw: Any) -> List[dict]:
    out: List[dict] = []
    for i in raw or []:
        if not isinstance(i, dict) or "key" not in i or "type" not in i:
            raise SpecError(f"input entry must have key+type: {i!r}")
        out.append({
            "key": str(i["key"]),
            "label": str(i.get("label") or i["key"]),
            "type": str(i["type"]),
            "accept": list(i.get("accept") or []),
            "options": list(i.get("options") or []),
            "default": i.get("default"),
            "storeAs": str(i.get("storeAs") or ("path" if i["type"] == "file" else "value")),
            "required": bool(i.get("required", False)),
        })
    return out


def _coerce_stages(raw: Any) -> List[dict]:
    out: List[dict] = []
    for s in raw or []:
        if not isinstance(s, dict) or "id" not in s:
            raise SpecError(f"stage entry must have id: {s!r}")
        builtin = s.get("builtin")
        if not builtin and not s.get("run"):
            raise SpecError(f"stage {s['id']!r} must have either `run:` or `builtin:`")
        out.append({
            "id": str(s["id"]),
            "builtin": builtin,
            "needs": list(s.get("needs") or []),
            "cwd": s.get("cwd"),
            "run": s.get("run"),
            "lifecycle": s.get("lifecycle", "one-shot"),
            "invalidatedBy": list(s.get("invalidatedBy") or []),
            "readyProbe": s.get("readyProbe"),
        })
    return out


def _coerce_logs(raw: Any) -> List[dict]:
    out: List[dict] = []
    for l in raw or []:
        if not isinstance(l, dict) or "name" not in l or "kind" not in l:
            raise SpecError(f"log entry must have name+kind: {l!r}")
        out.append({
            "name": str(l["name"]),
            "kind": str(l["kind"]),
            "stage": l.get("stage"),
            "path": l.get("path"),
            "follow": bool(l.get("follow", True)),
            "cmd": l.get("cmd"),
            "url": l.get("url"),
        })
    return out


def _coerce_workflows(raw: Any) -> dict:
    """Accept either:
        workflows:
          first-run: [a, b, c]                       # legacy: just stages
          fresh-emulator:                            # new: object with description
            description: "Wipe and cold-boot…"
            stages: [reset-emulator, a, b, c]
    Returns {'workflows': {name: [stages]}, 'workflowDescriptions': {name: desc}}.
    """
    flows: dict = {}
    descs: dict = {}
    for name, value in (raw or {}).items():
        if isinstance(value, list):
            flows[name] = [str(s) for s in value]
        elif isinstance(value, dict):
            stages = value.get("stages") or []
            if not isinstance(stages, list):
                raise SpecError(f"workflow {name!r}: 'stages' must be a list")
            flows[name] = [str(s) for s in stages]
            desc = value.get("description")
            if desc:
                descs[name] = str(desc)
        else:
            raise SpecError(f"workflow {name!r}: must be a list of stages or an object with stages+description")
    return {"workflows": flows, "workflowDescriptions": descs}


def load_spec(path: Path) -> dict:
    """Parse a single YAML spec file. Validates required fields and shapes."""
    _ensure_yaml()
    raw = yaml.safe_load(path.read_text())
    if not isinstance(raw, dict):
        raise SpecError(f"{path}: top-level must be a mapping")
    if "name" not in raw or "title" not in raw:
        raise SpecError(f"{path}: missing required name/title")

    spec: dict = {
        "name": str(raw["name"]),
        "title": str(raw["title"]),
        "icon": raw.get("icon"),
        "category": raw.get("category", "other"),
        "tags": list(raw.get("tags") or []),
        "source": dict(raw.get("source") or {}),
        "ports": _coerce_ports(raw.get("ports")),
        "adbReverse": list(raw.get("adbReverse") or []),
        "tools": list(raw.get("tools") or []),
        "domains": list(raw.get("domains") or []),
        "inputs": _coerce_inputs(raw.get("inputs")),
        "env": dict(raw.get("env") or {}),
        "stages": _coerce_stages(raw.get("stages")),
        **_coerce_workflows(raw.get("workflows")),
        "logs": _coerce_logs(raw.get("logs")),
        "hooks": dict(raw.get("hooks") or {}),
        "_path": str(path),
    }

    # Cross-checks: every ${ports.X} reference must resolve.
    port_names = {p["name"] for p in spec["ports"]}
    input_keys = {i["key"] for i in spec["inputs"]}
    for ref in _all_template_refs(spec):
        bucket, _, key = ref.partition(".")
        if bucket == "ports" and key not in port_names:
            raise SpecError(f"{spec['name']}: ${{ports.{key}}} not declared in ports[]")
        if bucket == "inputs" and key not in input_keys:
            raise SpecError(f"{spec['name']}: ${{inputs.{key}}} not declared in inputs[]")

    # Workflow stage refs must exist.
    stage_ids = {s["id"] for s in spec["stages"]}
    for wf, ids in spec["workflows"].items():
        for sid in ids:
            if sid not in stage_ids:
                raise SpecError(f"{spec['name']}: workflow {wf!r} references unknown stage {sid!r}")

    # adbReverse names must reference declared ports.
    for n in spec["adbReverse"]:
        if n not in port_names:
            raise SpecError(f"{spec['name']}: adbReverse refers to unknown port {n!r}")

    return spec


def _all_template_refs(spec: dict) -> List[str]:
    refs: List[str] = []
    def collect(s: Any) -> None:
        if isinstance(s, str):
            refs.extend(_TEMPLATE_RE.findall(s))
        elif isinstance(s, dict):
            for v in s.values():
                collect(v)
        elif isinstance(s, list):
            for v in s:
                collect(v)
    collect(spec.get("env"))
    collect(spec.get("stages"))
    collect(spec.get("logs"))
    collect(spec.get("hooks"))
    return refs


def discover_specs(specs_dir: Path = SPECS_DIR) -> List[dict]:
    """Load every *.yaml in specs/ that is not prefixed with _ (schema files)."""
    if not specs_dir.is_dir():
        return []
    out: List[dict] = []
    errors: List[str] = []
    for p in sorted(specs_dir.glob("*.yaml")):
        if p.name.startswith("_"):
            continue
        try:
            out.append(load_spec(p))
        except Exception as e:  # noqa: BLE001
            errors.append(f"{p.name}: {e}")
            print(f"  [spec_loader] skipped {p.name}: {e}")
    return out


_KNOWN_BUCKETS = frozenset({"ports", "inputs", "source", "env"})
_KNOWN_SCALARS = frozenset({"destDir", "repoRoot", "host"})


def template(value: Any, ctx: Dict[str, Any]) -> Any:
    """Recursively substitute ${a.b} references in strings using ctx.

    ctx shape: {"ports": {"metro": 8082}, "inputs": {"VARIANT": "Lynx"}, ...}
    Only references in a known namespace (ports.*, inputs.*, source.*, env.*)
    or a known top-level scalar (destDir, repoRoot) are substituted. Anything
    else — e.g. `${ANDROID_HOME}`, `${PATH}` — is passed through unchanged so
    the shell can expand it at runtime. `$${...}` still works as an explicit
    escape for legacy specs.
    """
    if isinstance(value, str):
        def repl(m: re.Match) -> str:
            ref = m.group(1)
            bucket, sep, key = ref.partition(".")
            if not sep:
                if ref in _KNOWN_SCALARS:
                    return str(ctx.get(ref, ""))
                return m.group(0)
            if bucket not in _KNOWN_BUCKETS:
                return m.group(0)
            container = ctx.get(bucket)
            if isinstance(container, dict):
                v = container.get(key)
                return "" if v is None else str(v)
            return m.group(0)
        protected = value.replace("$${", _ESCAPE_SENTINEL)
        substituted = _TEMPLATE_RE.sub(repl, protected)
        return substituted.replace(_ESCAPE_SENTINEL, "${")
    if isinstance(value, dict):
        return {k: template(v, ctx) for k, v in value.items()}
    if isinstance(value, list):
        return [template(v, ctx) for v in value]
    return value


_HOST_FILE = PROJECT_ROOT / "data" / "devbox-host"
_PORTS_FILE = PROJECT_ROOT / "data" / "devbox-ports.json"


def set_active_host(host: str) -> None:
    """Called by the server when a remote-stack connection is established."""
    h = host.strip() or "localhost"
    _HOST_FILE.parent.mkdir(parents=True, exist_ok=True)
    _HOST_FILE.write_text(h)


def get_active_host() -> str:
    """Read the persisted devbox host, defaulting to localhost."""
    try:
        return _HOST_FILE.read_text().strip() or "localhost"
    except OSError:
        return "localhost"


def set_registry_ports(ports: dict) -> None:
    """Persist resolved ports from the devbox registry."""
    _PORTS_FILE.parent.mkdir(parents=True, exist_ok=True)
    _PORTS_FILE.write_text(json.dumps(ports))


def get_registry_ports() -> Dict[str, int]:
    """Read persisted devbox registry ports."""
    try:
        data = json.loads(_PORTS_FILE.read_text())
        return data if isinstance(data, dict) else {}
    except (OSError, json.JSONDecodeError):
        return {}


def build_ctx(spec: dict, inputs: Dict[str, Any], port_overrides: Dict[str, int]) -> Dict[str, Any]:
    """Build the template context from a spec + current input/port state."""
    # Resolve ports: spec default → registry override (via registryKey) → per-slug override
    registry_ports = get_registry_ports()
    ports = {}
    for p in spec["ports"]:
        name = p["name"]
        reg_key = p.get("registryKey")
        base = registry_ports.get(reg_key, p["port"]) if reg_key else p["port"]
        ports[name] = port_overrides.get(name, base)
    dest_dir = (spec.get("source") or {}).get("destDir") or ""
    abs_dest = str((PROJECT_ROOT / dest_dir).resolve()) if dest_dir else str(PROJECT_ROOT)
    merged_inputs: Dict[str, Any] = {}
    for i in spec.get("inputs") or []:
        if "default" in i and i["default"] is not None:
            merged_inputs[i["key"]] = i["default"]
    if inputs:
        for k, v in inputs.items():
            if v is not None and v != "":
                merged_inputs[k] = v
    import os as _os
    return {
        "ports": ports,
        "inputs": merged_inputs,
        "source": {
            "ref": (spec.get("source") or {}).get("ref", ""),
            "url": (spec.get("source") or {}).get("url", ""),
        },
        # Lets specs reach host env vars in `env:` blocks or stage `run:`
        # strings via ${env.FOO}. Unset values resolve to empty string.
        "env": dict(_os.environ),
        "destDir": abs_dest,
        "repoRoot": str(PROJECT_ROOT),
        "host": get_active_host(),
    }


# ── Cache so the HTTP handler doesn't reparse on every request ──
_cache_lock = threading.Lock()
_cache: Dict[str, dict] = {}
_cache_mtime: Dict[str, float] = {}


def get_specs() -> List[dict]:
    """Return cached specs; reload entries whose YAML file mtime changed."""
    with _cache_lock:
        result: List[dict] = []
        seen_names: set = set()
        files = sorted(SPECS_DIR.glob("*.yaml")) if SPECS_DIR.is_dir() else []
        for p in files:
            if p.name.startswith("_"):
                continue
            mtime = p.stat().st_mtime
            cached = _cache.get(p.name)
            if cached is None or _cache_mtime.get(p.name) != mtime:
                try:
                    spec = load_spec(p)
                except Exception as e:  # noqa: BLE001
                    print(f"  [spec_loader] error reloading {p.name}: {e}")
                    if cached is not None:
                        result.append(cached)
                        seen_names.add(cached["name"])
                    continue
                _cache[p.name] = spec
                _cache_mtime[p.name] = mtime
                result.append(spec)
                seen_names.add(spec["name"])
            else:
                result.append(cached)
                seen_names.add(cached["name"])
        # Drop entries whose files disappeared.
        for fname in list(_cache.keys()):
            if not (SPECS_DIR / fname).exists():
                _cache.pop(fname, None)
                _cache_mtime.pop(fname, None)
        return result


def get_spec(name: str) -> Optional[dict]:
    for s in get_specs():
        if s["name"] == name:
            return s
    return None
