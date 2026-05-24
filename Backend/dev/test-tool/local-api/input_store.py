"""Persistent storage for launcher user-supplied inputs (text + files).

Layout under data/launcher-state/<slug>/:
  inputs.json   — {key: stringValue} for text/select/number/boolean/path-of-file
  files/<key>/<basename>  — uploaded file payloads (one per file input)

For inputs whose type is `file`:
  - storeAs=path       → inputs.json[KEY] = absolute path to the stored file
  - storeAs=env-inline → inputs.json[KEY] = utf-8 contents of the file
  - storeAs=value      → same as path (treated as text path)

For text/select/number/boolean inputs:
  - inputs.json[KEY] = stringified user value
"""
from __future__ import annotations

import base64
import json
import shutil
import threading
from pathlib import Path
from typing import Any, Dict, List, Optional

from spec_loader import PROJECT_ROOT

STATE_ROOT = PROJECT_ROOT / "data" / "launcher-state"
_lock = threading.Lock()


def _slug_dir(slug: str) -> Path:
    d = STATE_ROOT / slug
    d.mkdir(parents=True, exist_ok=True)
    (d / "files").mkdir(parents=True, exist_ok=True)
    return d


def _inputs_path(slug: str) -> Path:
    return _slug_dir(slug) / "inputs.json"


def load(slug: str) -> Dict[str, Any]:
    p = _inputs_path(slug)
    if not p.is_file():
        return {}
    try:
        return json.loads(p.read_text())
    except Exception:  # noqa: BLE001
        return {}


def _save_atomic(slug: str, data: Dict[str, Any]) -> None:
    p = _inputs_path(slug)
    tmp = p.with_suffix(".tmp")
    tmp.write_text(json.dumps(data, indent=2, default=str))
    tmp.replace(p)


def set_value(slug: str, key: str, value: Any) -> Dict[str, Any]:
    with _lock:
        data = load(slug)
        data[key] = value
        _save_atomic(slug, data)
        return data


def set_many(slug: str, values: Dict[str, Any]) -> Dict[str, Any]:
    with _lock:
        data = load(slug)
        data.update(values)
        _save_atomic(slug, data)
        return data


def store_file(slug: str, input_def: dict, filename: str, content_b64: str) -> str:
    """Save an uploaded file under data/launcher-state/<slug>/files/<KEY>/<filename>.

    Returns the value to record in inputs.json depending on storeAs.
    """
    key = input_def["key"]
    store_as = input_def.get("storeAs", "path")
    raw = base64.b64decode(content_b64, validate=True)
    bucket = _slug_dir(slug) / "files" / key
    # Replace previous contents so the value points at exactly what the user uploaded.
    if bucket.exists():
        shutil.rmtree(bucket)
    bucket.mkdir(parents=True, exist_ok=True)
    target = bucket / Path(filename).name
    target.write_bytes(raw)
    if store_as == "env-inline":
        try:
            return raw.decode("utf-8")
        except UnicodeDecodeError as e:
            raise ValueError(
                f"input {key!r} has storeAs=env-inline but file isn't UTF-8: {e}"
            ) from None
    return str(target)


def upsert_input(slug: str, input_def: dict, payload: dict) -> Any:
    """Update one input from a typed payload (file vs scalar)."""
    if input_def["type"] == "file":
        filename = (payload.get("filename") or "").strip()
        content_b64 = payload.get("content_base64")
        if not filename or not content_b64:
            raise ValueError(f"input {input_def['key']!r} requires filename+content_base64")
        value = store_file(slug, input_def, filename, content_b64)
    else:
        value = payload.get("value")
        if value is None:
            value = ""
    set_value(slug, input_def["key"], value)
    return value


def ensure_defaults(slug: str, spec: dict) -> None:
    """Seed default values for inputs that aren't already in the store.
    Also trims whitespace from existing string values."""
    with _lock:
        data = load(slug)
        changed = False
        for i in spec.get("inputs", []):
            key = i["key"]
            if key not in data and i.get("default") is not None and i.get("type") != "file":
                raw = str(i["default"])
                data[key] = raw if i.get("type") == "textarea" else raw.strip()
                changed = True
            elif key in data and isinstance(data[key], str) and i.get("type") not in ("file", "textarea"):
                trimmed = data[key].strip()
                if trimmed != data[key]:
                    data[key] = trimmed
                    changed = True
        if changed:
            _save_atomic(slug, data)


def public_view(slug: str, spec: dict) -> Dict[str, Any]:
    """Return inputs safe to ship to the dashboard.

    Secrets and env-inline file contents are summarized (length only) rather
    than echoed back to the client.
    """
    data = load(slug)
    out: Dict[str, Any] = {}
    by_key = {i["key"]: i for i in spec.get("inputs", [])}
    for k, v in data.items():
        idef = by_key.get(k, {})
        t = idef.get("type")
        store_as = idef.get("storeAs")
        if t == "secret-text":
            out[k] = {"set": bool(v), "length": len(str(v))}
        elif t == "file" and store_as == "env-inline":
            out[k] = {"set": bool(v), "length": len(str(v))}
        elif t == "file":
            p = Path(str(v)) if v else None
            out[k] = {"set": bool(v), "filename": p.name if p else None, "path": str(v)}
        else:
            out[k] = v
    return out


def missing_required(slug: str, spec: dict) -> List[str]:
    data = load(slug)
    out: List[str] = []
    for i in spec.get("inputs", []):
        if i.get("required") and not data.get(i["key"]):
            out.append(i["key"])
    return out
