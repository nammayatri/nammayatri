#!/usr/bin/env python3
"""Check the build workflow before pushing it.

    ./validate-workflow.py [path/to/algeria-backend-build.yml]

Reading a GitHub `if:` condition is not enough. Run 30993922925 skipped the
+213 patches, the gateway preparation and the cache restore, reported all three
as an ordinary green "skipped", and would have spent five hours producing
unpatched binaries with no gateway if the next step had not happened to fail.

The cause: the build is triggered by a push, so there is no `inputs` context and
every `inputs.x` is null. GitHub casts operands to numbers when their types
differ, and `null` and `false` both become 0 — so `inputs.x != false` is FALSE.

So this does not grep for the bad spelling. It evaluates every condition.
GitHub's `&&` and `||` return an operand rather than a boolean and treat null as
falsy, which is exactly what Python's `and` / `or` do with `None`, so the
translation below is faithful for the shapes used in this workflow.
"""

import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

import yaml

# Steps that must run on a push. If any of these skips, the artifact is either
# wrong (unpatched, no gateway) or the run throws away its progress.
MUST_RUN_ON_PUSH = {
    "Apply the Algeria +213 patches",
    "Make beckn-gateway and mock-registry project packages",
    "Restore stack cache",
    "Merge the cache into the image's stack root",
    "Build beckn-gateway and mock-registry",
    "Copy the built dependency tree back out",
    "Save stack cache",
}

# Steps that must still run when the app build fails, so a timed-out or broken
# run leaves the next one less to do instead of nothing.
MUST_RUN_ON_FAILURE = {
    "Upload build logs",
    "Copy the built dependency tree back out",
    "Save stack cache",
}


def evaluate(expr, *, event="push", apps_ok=True):
    """Translate a GitHub expression to Python and evaluate it."""
    e = expr.strip()
    if e.startswith("${{"):
        e = e[3:].rstrip("}").strip()
    e = e.replace("github.event_name != 'workflow_dispatch'",
                  repr(event != "workflow_dispatch"))
    e = e.replace("steps.build_apps.outcome == 'success'", repr(apps_ok))
    e = e.replace("always()", "True")
    # On a push there is no inputs context; on a dispatch, defaults are true.
    e = re.sub(r"inputs\.\w+", "None" if event == "push" else "True", e)
    e = e.replace("&&", " and ").replace("||", " or ")
    return bool(eval(e, {"__builtins__": {}}, {}))


def main() -> int:
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else \
        Path(__file__).resolve().parents[2] / "workflows" / "algeria-backend-build.yml"
    if not path.is_file():
        return fail(f"{path} not found")

    raw = path.read_text(encoding="utf-8")
    doc = yaml.safe_load(raw)
    steps = doc["jobs"]["build"]["steps"]
    names = {s.get("name") for s in steps}
    problems = []

    # ---- 1. every run: block in EVERY job must be valid shell ------------
    for job_name, job in doc["jobs"].items():
        for s in job.get("steps", []):
            run = s.get("run")
            if not run:
                continue
            script = re.sub(r"\$\{\{[^}]*\}\}", "EXPR", run)
            with tempfile.NamedTemporaryFile("w", suffix=".sh", delete=False) as f:
                f.write(script)
                tmp = f.name
            r = subprocess.run(["bash", "-n", tmp], capture_output=True, text=True)
            os.unlink(tmp)
            if r.returncode:
                problems.append(
                    f"shell syntax error in {job_name}/{s.get('name')!r}:\n{r.stderr}")
    print(f"jobs checked: {', '.join(doc['jobs'])}")

    # ---- 2. what actually runs on a push ---------------------------------
    print("as a push event:")
    for s in steps:
        name, cond = s.get("name", "?"), s.get("if")
        runs = True if cond is None else evaluate(cond)
        if cond is not None:
            print(f"  {'RUN ' if runs else 'SKIP'}  {name}")
        if name in MUST_RUN_ON_PUSH and not runs:
            problems.append(f"must run on a push but would skip: {name}")
    for n in MUST_RUN_ON_PUSH - names:
        problems.append(f"step disappeared: {n}")

    # ---- 3. a failed build must still bank its progress ------------------
    print("\nwith the app build failed:")
    for s in steps:
        name, cond = s.get("name", "?"), s.get("if")
        if name not in MUST_RUN_ON_FAILURE:
            continue
        runs = True if cond is None else evaluate(cond, apps_ok=False)
        print(f"  {'RUN ' if runs else 'SKIP'}  {name}")
        if not runs:
            problems.append(f"must run after a failure but would skip: {name}")

    # ---- 4. the specific traps -------------------------------------------
    uncommented = re.sub(r"^\s*#.*$", "", raw, flags=re.M)
    if "!= false" in uncommented:
        problems.append("a `!= false` guard is back — it is FALSE on a push")
    if "old-releases" in uncommented:
        problems.append("bionic repointed at old-releases; it is not there")

    print()
    if problems:
        for p in problems:
            print(f"PROBLEM: {p}")
        return 1
    print("ok — conditions, shell syntax and known traps all check out")
    return 0


def fail(msg):
    print(f"FAILED: {msg}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
