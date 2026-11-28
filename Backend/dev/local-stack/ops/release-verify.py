#!/usr/bin/env python3
"""Is the server running the commit it says it is? Compares, by sha256.

    release-verify.py <hashes file>                    expected = git, the claimed commit
    release-verify.py <hashes file> --manifest <file>  expected = that manifest (the test)

Run by `ops/deploy.sh verify`, and at the end of every release. The hashes file
is what `release-remote.py hashes` printed on the server: the commit named in
.shipped, then `sha256  path` for every file the server says was shipped, and
`sha256  @units/<name>` for every systemd unit it installed.

── Why the expected side comes from git, here ─────────────────────────────────
The release already checks its own writes against its own MANIFEST. That proves
the copy, not the claim: a .shipped naming the wrong commit, a .shipped.files
rewritten by hand, or a file edited and its record "fixed" to match would all
pass it. Here the server only measures, and what the files SHOULD be is
recomputed from the commit itself, on a machine the server cannot write to.
Phase 4 of the backend restructuring plan (2026-10-06).
"""
import hashlib
import io
import os
import subprocess
import sys
import tarfile

LS = 'Backend/dev/local-stack/stack/'
GREEN, RED, BOLD, END = '\033[1;32m', '\033[1;31m', '\033[1m', '\033[0m'


def expected_from_git(commit):
    """{path under stack/: sha256} for the commit, straight from git."""
    if subprocess.run(['git', 'cat-file', '-e', f'{commit}^{{commit}}'],
                      capture_output=True).returncode != 0:
        subprocess.run(['git', 'fetch', '-q', 'origin'], capture_output=True)
        if subprocess.run(['git', 'cat-file', '-e', f'{commit}^{{commit}}'],
                          capture_output=True).returncode != 0:
            sys.exit(f'{RED}the server claims commit {commit}, which this repository does not have{END}')
    raw = subprocess.run(['git', 'archive', commit, LS], capture_output=True, check=True).stdout
    out = {}
    with tarfile.open(fileobj=io.BytesIO(raw)) as tar:
        for m in tar.getmembers():
            if m.isfile():
                out[m.name[len(LS):]] = hashlib.sha256(tar.extractfile(m).read()).hexdigest()
    return out


def expected_from_manifest(path):
    out = {}
    for line in open(path, encoding='utf-8'):
        line = line.rstrip('\n')
        if line:
            digest, _mode, rel = line.split('  ', 2)
            out[rel] = digest
    return out


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    lines = [l.rstrip('\n') for l in open(sys.argv[1], encoding='utf-8') if l.strip()]
    if not lines or not lines[0].startswith('commit '):
        sys.exit(f'{RED}no "commit" line: the server did not answer as release-remote.py hashes does{END}')
    commit = lines[0].split()[1]
    measured, units = {}, {}
    for l in lines[1:]:
        digest, rel = l.split('  ', 1)
        if rel.startswith('@units/'):
            units[rel[len('@units/'):]] = digest
        else:
            measured[rel] = digest

    if '--manifest' in sys.argv:
        expected = expected_from_manifest(sys.argv[sys.argv.index('--manifest') + 1])
    else:
        expected = expected_from_git(commit)

    bad = []
    for rel, digest in sorted(expected.items()):
        if rel not in measured:
            bad.append(f'{rel}: in the commit, but not in the server\'s record of what it was shipped')
        elif measured[rel] == 'MISSING':
            bad.append(f'{rel}: missing on the server')
        elif measured[rel] != digest:
            bad.append(f'{rel}: different on the server')
    for rel in sorted(set(measured) - set(expected)):
        bad.append(f'{rel}: the server\'s record lists it, the commit does not have it')
    want_units = {os.path.basename(r): d for r, d in expected.items()
                  if r.startswith('systemd/') and r.count('/') == 1}
    for name, digest in sorted(want_units.items()):
        got = units.get(name, 'MISSING')
        if got != digest:
            bad.append(f'installed unit {name}: '
                       f'{"not installed" if got == "MISSING" else "differs from the commit"}')

    print(f'\n{BOLD}== does the server hold commit {commit[:10]}?{END}')
    if bad:
        for b in bad:
            print(f'   {RED}BAD {END}{b}')
        print(f'\n   {RED}{len(bad)} difference(s) from the commit the server claims{END}')
        sys.exit(1)
    print(f'   {GREEN}ok  {END}all {len(expected)} files, and {len(want_units)} installed unit(s), '
          f'are byte for byte the commit -- hashed on the server, expected from git')


if __name__ == '__main__':
    main()
