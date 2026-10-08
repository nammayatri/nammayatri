#!/usr/bin/env python3
"""
Turn beckn-gateway and mock-registry into *project* packages of the Namma
Yatri stack project, so that `stack build` produces their executables.

    ./prepare-stack-yaml.py <source-root>

Prints the beckn-gateway commit it found, as `commit=<sha>`, so the caller can
clone exactly that revision.

WHY THIS IS NEEDED
------------------
Backend/stack.yaml lists beckn-gateway under `extra-deps:`:

    - git: https://github.com/nammayatri/beckn-gateway.git
      commit: 7094d2af...
      subdirs: [app/gateway, app/mock-registry]

stack builds *dependency* packages library-only. It builds executables only
for project packages — the ones under `packages:`. So as an extra-dep we would
get libbeckn-gateway and no `beckn-gateway-exe`, which is the one thing we
actually need: without the gateway running, the rider side (BAP) cannot reach
the driver side (BPP), a ride search comes back with a route and no price, and
the driver never sees the request.

So we move the two subdirs from `extra-deps:` to `packages:`, pointing at a
sibling clone at the same pinned commit. Upstream evidently did the same thing
at some point — the lines

    # - ../beckn-gateway/app/gateway
    # - ../beckn-gateway/app/mock-registry

are still sitting there commented out.

Note that beckn-gateway's own stack.yaml pins shared-kernel at 296681fc while
nammayatri pins 28bae0f3. Building it inside the nammayatri project means it
compiles against 28bae0f3. That is the combination nammayatri's own stack.yaml
already declares, and it is the cheap one: one snapshot, one dependency build
for all four executables. Building beckn-gateway as its own stack project
instead would pull a second mobility-core and, because stack keys its snapshot
database on the whole extra-deps set, risk re-compiling the entire dependency
tree a second time.
"""

import re
import sys
from pathlib import Path

GATEWAY_SUBDIRS = ["app/gateway", "app/mock-registry"]
CLONE_DIR = "../beckn-gateway"          # relative to Backend/stack.yaml

# The extra-deps entry we are removing, and the line that follows it. Both are
# unique in the file, which is what makes this a safe textual edit.
BLOCK_START = "  - git: https://github.com/nammayatri/beckn-gateway.git"
BLOCK_END = "  # -  ../../shared-kernel/lib/mobility-core"


def main() -> int:
    if len(sys.argv) != 2:
        print(__doc__.strip(), file=sys.stderr)
        return 2

    path = Path(sys.argv[1]).resolve() / "Backend" / "stack.yaml"
    if not path.is_file():
        fail(f"{path} not found — is the source ref the pinned 2023 baseline?")

    lines = path.read_text(encoding="utf-8").splitlines()

    new_entries = [f"  - {CLONE_DIR}/{s}" for s in GATEWAY_SUBDIRS]
    if all(e in lines for e in new_entries) and BLOCK_START not in lines:
        commit = read_commit_from_clone_or_die(Path(sys.argv[1]))
        print(f"commit={commit}")
        print("stack.yaml already prepared", file=sys.stderr)
        return 0

    # ---- 1. pull the pinned commit out of the extra-deps block ------------
    try:
        start = lines.index(BLOCK_START)
        end = lines.index(BLOCK_END)
    except ValueError:
        fail("could not locate the beckn-gateway extra-deps block in "
             "Backend/stack.yaml. Upstream changed it; re-check by hand.")
    if not start < end:
        fail("beckn-gateway extra-deps block looks malformed "
             f"(start line {start + 1}, end line {end + 1}).")

    block = lines[start:end]
    m = re.search(r"^\s*commit:\s*([0-9a-f]{40})\s*$", "\n".join(block), re.M)
    if not m:
        fail("no 40-character commit sha in the beckn-gateway extra-deps "
             f"block:\n" + "\n".join(block))
    commit = m.group(1)

    for sub in GATEWAY_SUBDIRS:
        if f"      - {sub}" not in block:
            fail(f"expected subdir '{sub}' in the beckn-gateway extra-deps "
                 f"block, which lists:\n" + "\n".join(block))

    # ---- 2. remove it from extra-deps ------------------------------------
    lines = lines[:start] + lines[end:]

    # ---- 3. add the two subdirs as project packages ----------------------
    # `packages:` is a flat list; append after its last entry rather than
    # anywhere inside, so we cannot land in the middle of extra-deps.
    try:
        pkg_start = lines.index("packages:")
    except ValueError:
        fail("no `packages:` key in Backend/stack.yaml.")

    pkg_end = pkg_start + 1
    while pkg_end < len(lines) and lines[pkg_end].startswith("  - "):
        pkg_end += 1
    if pkg_end == pkg_start + 1:
        fail("`packages:` in Backend/stack.yaml has no entries — refusing to "
             "guess where to insert.")

    lines = lines[:pkg_end] + new_entries + lines[pkg_end:]

    path.write_text("\n".join(lines) + "\n", encoding="utf-8")

    # ---- 4. verify -------------------------------------------------------
    text = path.read_text(encoding="utf-8")
    if "github.com/nammayatri/beckn-gateway.git" in text:
        fail("beckn-gateway is still an extra-dep after the edit.")
    for e in new_entries:
        if e not in text.splitlines():
            fail(f"project package entry missing after the edit: {e}")

    print(f"commit={commit}")
    print(f"beckn-gateway: extra-dep -> project packages "
          f"({', '.join(GATEWAY_SUBDIRS)}) at {commit[:7]}", file=sys.stderr)
    return 0


def read_commit_from_clone_or_die(root: Path) -> str:
    """On a second run the commit is no longer in stack.yaml; take it from the
    clone we made the first time."""
    head = root / "beckn-gateway" / ".git" / "HEAD"
    if head.is_file():
        v = head.read_text(encoding="utf-8").strip()
        if re.fullmatch(r"[0-9a-f]{40}", v):
            return v
    fail("stack.yaml is already prepared but ./beckn-gateway is not a clone "
         "at a known commit — clean the source tree and start over.")


def fail(msg: str):
    print(f"\nFAILED: {msg}", file=sys.stderr)
    sys.exit(1)


if __name__ == "__main__":
    sys.exit(main())
