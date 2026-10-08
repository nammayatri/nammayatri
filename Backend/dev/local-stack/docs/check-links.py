#!/usr/bin/env python3
"""Every relative link in the README and docs/ points at a file that exists,
and every #anchor at a heading on that page (GitHub's slugs). Phase 7.

    python3 docs/check-links.py
"""
import glob, os, re, sys

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FILES = [os.path.join(HERE, 'README.md')] + sorted(
    glob.glob(os.path.join(HERE, 'docs', '**', '*.md'), recursive=True))

def slug(h):
    return re.sub(r'[^\w\- ]', '', h.strip().lower()).replace(' ', '-')

def anchors(path):
    seen, out, inside = {}, set(), False
    for l in open(path).read().split('\n'):
        if l.startswith('```'):
            inside = not inside
        m = None if inside else re.match(r'^#{1,6} (.*)$', l)
        if m:
            s = slug(m.group(1)); n = seen.get(s, 0); seen[s] = n + 1
            out.add(s if n == 0 else f'{s}-{n}')
    return out

bad = n = 0
for f in FILES:
    text = re.sub(r'```.*?```', '', open(f).read(), flags=re.S)
    for target in re.findall(r'\]\(([^)\s]+)\)', text):
        if re.match(r'[a-z]+:', target):
            continue
        n += 1
        path, _, anchor = target.partition('#')
        dest = os.path.normpath(os.path.join(os.path.dirname(f), path)) if path else f
        if not os.path.exists(dest):
            bad += 1; print(f'{os.path.relpath(f, HERE)}: no such file: {target}'); continue
        if anchor and dest.endswith('.md') and anchor not in anchors(dest):
            bad += 1; print(f'{os.path.relpath(f, HERE)}: no such heading: {target}')
print(f'{n} links checked in {len(FILES)} files; {bad} broken')
sys.exit(1 if bad else 0)
