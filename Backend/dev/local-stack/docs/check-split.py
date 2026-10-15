#!/usr/bin/env python3
"""Phase 7's proof that the README split lost nothing (2026-10-08).

    python3 docs/check-split.py

Every non-blank line of the README as it was (git, commit a1582735cd) must
appear in the README or a docs/ page AS THE SPLIT LEFT THEM (commit
9610cab8a2), at least as often as before. Both sides come from git, so the
proof stays reproducible however the pages are edited afterwards. Link
targets are ignored -- moving a section changes where its links point, not what
it says. Prints every line that is missing, and exits 1 if any is.
"""
import collections, os, re, subprocess, sys

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OLD_COMMIT = 'a1582735cd'
SPLIT_COMMIT = '9610cab8a2'
# Lines deliberately reworded in the README's opening -- each named here, so
# nothing else can hide behind them.
REWORDED = {
    'Commands in this README written as `./x.sh` are run from `stack/`.',
    '├── docs/                  snapshots of the server, file by file',
}

def norm(line):
    line = re.sub(r'\*\[([^\]]+)\]\([^)]*\)\*', r'*\1*', line)   # *[x](y)* -> *x*
    line = re.sub(r'\[([^\]]+)\]\([^)]*\)', r'\1', line)         # [x](y)   -> x
    return line.rstrip()

old = subprocess.run(['git', 'show', f'{OLD_COMMIT}:Backend/dev/local-stack/README.md'],
                     cwd=HERE, capture_output=True, text=True, check=True).stdout
keep = lambda ls: [norm(l) for l in ls if l.strip() and l.strip() != '---']
want = collections.Counter(keep(old.split('\n')))
have = collections.Counter()
listing = subprocess.run(['git', 'ls-tree', '--full-tree', '-r', '--name-only', SPLIT_COMMIT,
                          'Backend/dev/local-stack/docs/', 'Backend/dev/local-stack/README.md'],
                         cwd=HERE, capture_output=True, text=True, check=True).stdout.split()
for f in [x for x in listing if x.endswith('.md')]:
    text = subprocess.run(['git', 'show', f'{SPLIT_COMMIT}:{f}'], cwd=HERE,
                          capture_output=True, text=True, check=True).stdout
    have.update(keep(text.split('\n')))

missing = [(l, n - have[l]) for l, n in want.items() if have[l] < n and l not in REWORDED]
for l, n in missing:
    print(f'MISSING x{n}: {l}')
print(f'{sum(want.values())} lines checked, {len(want)} distinct; '
      f'{len(missing)} missing; {len(REWORDED)} reworded on purpose')
sys.exit(1 if missing else 0)
