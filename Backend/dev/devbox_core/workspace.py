"""Workspace registry on a devbox: /tmp/devbox-registry.json.

{"users": {<workspace>: {dir, ports, caddyPort, owner?}}}. The stack's
pre-flight (Backend/nix/scripts.nix) writes ports/caddyPort under the same
lock this module takes, and keeps any other field (such as owner).

Moved from test-tool/local-api/server.py so the devbox workflow can register
its workspaces too.
"""
from __future__ import annotations

import json
import re
import shlex

from . import ssh

REGISTRY_FILE = "/tmp/devbox-registry.json"
# Same lock as scripts.nix's pre-flight, so a register never races a stack start.
LOCK_FILE = "/tmp/devbox-ports.lock"

SAFE_NAME_RE = re.compile(r"^[a-zA-Z0-9_-]+$")

# jq: is the entry's owner the given one? Owners match on who made it ("by")
# and which release it belongs to; a missing owner only matches a missing one.
_SAME_OWNER = ('def same_owner($e; $o): (($e.owner // null) as $c | '
               'if $o == null then $c == null '
               'else ($c != null and $c.by == $o.by and $c.release == $o.release) end);')


class OwnerMismatch(ValueError):
    """The workspace exists but belongs to someone else."""


def workspace_dir(name: str) -> str:
    return f"/tmp/{name}/nammayatri"


def _check_name(name: str) -> None:
    if not SAFE_NAME_RE.match(name):
        raise ValueError(f"devName contains unsafe characters: {name!r}")


def _locked(update: str) -> str:
    """Bash that runs `update` (must write "$reg.tmp.$$") under the registry
    lock and then swaps the file in. Exit 3 = jq refused (owner mismatch)."""
    return f"""set -e
exec 9>{LOCK_FILE}
flock -w 30 9
reg={REGISTRY_FILE}
[ -s "$reg" ] || echo '{{"users":{{}}}}' > "$reg"
if ! {{ {update}; }}; then rm -f "$reg.tmp.$$"; exit 3; fi
mv -f "$reg.tmp.$$" "$reg"
"""


def _raise_for(r, host: str) -> None:
    err = (r.stderr or "").strip()
    if r.returncode == 3 and "owner mismatch" in err:
        raise OwnerMismatch(err.split("owner mismatch", 1)[1].strip(" :\"") or "owner mismatch")
    raise RuntimeError(f"registry update failed on {host}: {err or f'exit {r.returncode}'}")


def read_registry(t: ssh.Target, timeout: int = 10) -> dict:
    """The registry JSON from the devbox; {"users": {}} when missing or unreadable."""
    script = f"cat {REGISTRY_FILE} 2>/dev/null || echo '{{\"users\": {{}}}}'"
    try:
        r = ssh.run(t, script, timeout=timeout)
        if r.returncode == 0 and r.stdout.strip():
            return json.loads(r.stdout)
    except Exception:
        pass
    return {"users": {}}


def register(t: ssh.Target, name: str, owner: dict | None = None, timeout: int = 40) -> dict:
    """Ensure `name` is in the registry and return its entry.

    Runs as one locked read-modify-write on the devbox: an existing entry keeps
    every field (dir, ports, caddyPort, …); a new one gets dir. Other
    workspaces' entries are never touched.

    With an owner: a new entry gets it; an existing entry must already have the
    same owner, otherwise OwnerMismatch (an entry without an owner was made by
    hand and is never taken over). Without an owner nothing is checked or set,
    which is how the 7070 dashboard registers.
    Raises ValueError for an unsafe name, RuntimeError when the write fails."""
    _check_name(name)
    q = shlex.quote
    owner_json = json.dumps(owner) if owner else "null"
    prog = (_SAME_OWNER +
            ' if $owner != null and .users[$n] != null and (same_owner(.users[$n]; $owner) | not)'
            ' then error("owner mismatch: \\($n) belongs to \\(.users[$n].owner // "no owner (made by hand)")")'
            ' else .users[$n] = ((.users[$n] // {dir: $dir})'
            ' + (if $owner == null then {} else {owner: $owner} end)) end')
    update = (f'jq --arg n {q(name)} --arg dir {q(workspace_dir(name))} '
              f'--argjson owner {q(owner_json)} {q(prog)} "$reg" > "$reg.tmp.$$"')
    script = _locked(update) + f"jq -c --arg n {q(name)} '.users[$n]' \"$reg\"\n"
    r = ssh.run(t, script, timeout=timeout)
    if r.returncode != 0 or not r.stdout.strip():
        _raise_for(r, t.host)
    return json.loads(r.stdout)


def release(t: ssh.Target, name: str, owner: dict | None = None, timeout: int = 40) -> bool:
    """Remove `name` from the registry; True if it was there, False if already gone.

    The owner must match the entry's (None matches an entry without an owner),
    otherwise OwnerMismatch. Only the registry entry goes: the folder and the
    processes are the caller's job."""
    _check_name(name)
    q = shlex.quote
    owner_json = json.dumps(owner) if owner else "null"
    prog = (_SAME_OWNER +
            ' if .users[$n] == null then .'
            ' elif same_owner(.users[$n]; $owner) then del(.users[$n])'
            ' else error("owner mismatch: \\($n) belongs to \\(.users[$n].owner // "no owner (made by hand)")") end')
    update = (f'jq --arg n {q(name)} --argjson owner {q(owner_json)} {q(prog)} '
              f'"$reg" > "$reg.tmp.$$"')
    # Print whether the entry existed, read under the same lock before the update.
    script = _locked(f'had=$(jq -r --arg n {q(name)} \'.users[$n] != null\' "$reg") && {update}') + 'echo "$had"\n'
    r = ssh.run(t, script, timeout=timeout)
    if r.returncode != 0 or not r.stdout.strip():
        _raise_for(r, t.host)
    return r.stdout.strip().splitlines()[-1] == "true"
