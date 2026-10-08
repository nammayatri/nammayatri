#!/usr/bin/env python3
"""Phase 7's proof that the README split lost nothing (2026-10-08).

    python3 docs/check-split.py

Every non-blank line of the README as it was (git, commit a1582735cd) must
appear in the new README or a docs/ page, at least as often as before. Link
targets are ignored -- moving a section changes where its links point, not what
it says. Prints every line that is missing, and exits 1 if any is.
"""
import collections, glob, os, re, subprocess, sys

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OLD_COMMIT = 'a1582735cd'
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
for f in [os.path.join(HERE, 'README.md')] + glob.glob(os.path.join(HERE, 'docs', '*.md')):
    have.update(keep(open(f).read().split('\n')))

missing = [(l, n - have[l]) for l, n in want.items() if have[l] < n and l not in REWORDED]
for l, n in missing:
    print(f'MISSING x{n}: {l}')
print(f'{sum(want.values())} lines checked, {len(want)} distinct; '
      f'{len(missing)} missing; {len(REWORDED)} reworded on purpose')
sys.exit(1 if missing else 0)
