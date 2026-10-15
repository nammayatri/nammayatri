#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Add the Arabic label route to the DEPLOYED edge config, in place.

Run ON the server. The deployed nginx.conf is ~11 KB ahead of the repo's copy --
the website's admin-console and movinapp.net blocks exist only on the box -- so
this inserts one location and never rewrites the file. Copying the repo's
version over it would delete the website.

Idempotent, because a deploy script gets re-run after an unrelated failure.
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
    # Arabic place names, read by the app straight from our own index.
    #
    # They cannot travel the normal way. The deployed rider-app validates
    # `language` against a five-value enum with no Arabic in it, and its Google
    # client has no `language` query parameter at all -- it accepts the field
    # and then drops it. Both measured against the binary on 2026-09-10.
    # Adding it is a backend rebuild; this is a location.
    #
    # `=` and the whole path, NOT the /place/ prefix. /place/autocomplete/json
    # and /place/details/json answer the backend and have no business being
    # reachable from a phone -- widening this would publish the whole geocoder.
    #
    # No auth, deliberately: OpenStreetMap names keyed by ids the caller already
    # holds, carrying no rider and no driver. Same rate limit as the rest.
    location = /place/labels/json {
        limit_req zone=api burst=20 nodelay;
        proxy_pass http://127.0.0.1:8030;
        include /etc/nginx/conf.d/proxy-common.inc;
    }
"""

s = io.open(F, encoding='utf-8').read()

if 'location = /place/labels/json' in s:
    print('  already there -- nothing to do')
    sys.exit(0)

n = s.count(ANCHOR)
if n != 1:
    sys.exit('  the /wallet/ location matched %d times, expected 1 -- not touching it' % n)

io.open(F, 'w', encoding='utf-8', newline='\n').write(s.replace(ANCHOR, ANCHOR + BLOCK))
print('  inserted after /wallet/  (%d -> %d bytes)' % (len(s), len(s) + len(BLOCK)))
