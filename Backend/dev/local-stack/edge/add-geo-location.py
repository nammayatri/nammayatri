#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Add the sign-in country route (/geo/) to the DEPLOYED edge config, in place.

Run ON the server. The deployed nginx.conf is ahead of the repo's copy -- the
website's blocks exist only on the box -- so this inserts one location and
never rewrites the file. Same shape as add-place-labels-location.py.

Idempotent, because a deploy script gets re-run after an unrelated failure.
Written with 'w' on the same path, which truncates in place: the file is
bind-mounted into ny-edge, and replacing the inode would leave nginx reading
the old one.
"""
import io
import sys

F = '/opt/ny/local-stack/edge/nginx.conf'

ANCHOR = """    location /wallet/ {
        limit_req zone=api burst=20 nodelay;
        proxy_pass http://127.0.0.1:8030;
        include /etc/nginx/conf.d/proxy-common.inc;
    }
"""

BLOCK = """
    # The sign-in screen's country, from the caller's IP (2026-09-14). The
    # shim reads X-Real-IP, which proxy-common.inc sets from $remote_addr.
    location /geo/ {
        limit_req zone=api burst=20 nodelay;
        proxy_pass http://127.0.0.1:8030;
        include /etc/nginx/conf.d/proxy-common.inc;
    }
"""

s = io.open(F, encoding='utf-8').read()

if 'location /geo/' in s:
    print('  already there -- nothing to do')
    sys.exit(0)

n = s.count(ANCHOR)
if n != 1:
    sys.exit('  the /wallet/ location matched %d times, expected 1 -- not touching it' % n)

io.open(F, 'w', encoding='utf-8', newline='\n').write(s.replace(ANCHOR, ANCHOR + BLOCK))
print('  inserted after /wallet/  (%d -> %d bytes)' % (len(s), len(s) + len(BLOCK)))
