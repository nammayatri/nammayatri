'use strict';
// The edge's /places/ block (stack/edge/nginx.conf), day 2 of « Lieux
// proposés », 2026-11-09 -- and the /place/ paths it must NOT publish.
//
//   node tests/edge-places.test.js
//
// No nginx on the laptop and no CI, so this reads the file and chooses a
// location for each address the way nginx does (an exact `=` match first;
// else the longest prefix, final if it is `^~`; else the first regex that
// matches, in file order; else that longest prefix). It is not `nginx -t`:
// the release runs that on the box before it reloads, and refuses a file
// nginx refuses. This checks what `nginx -t` cannot: WHERE an address goes.
//
// Why it matters: a shim route without an edge block answers correctly on
// 8030 and 404 from every phone (docs/gotchas.md -- the wallet shipped that
// way); and a /place/ prefix published by mistake would hand the backend's
// whole geocoder to the internet.
const fs = require('fs');
const path = require('path');
const fakes = require('./lib/fakes');

const CONF = path.join(__dirname, '..', 'stack', 'edge', 'nginx.conf');
const { BODY_LIMIT } = require(path.join(__dirname, '..', 'stack', 'maps-shim', 'suggested-places.js'));

/** The text of each top-level `server { … }`, comments removed. */
function servers(text) {
  const clean = text.split('\n').map((l) => l.replace(/(^|\s)#.*$/, '')).join('\n');
  const out = [];
  const re = /^server\s*\{/gm;
  let m;
  while ((m = re.exec(clean))) {
    let depth = 0;
    for (let i = m.index; i < clean.length; i += 1) {
      if (clean[i] === '{') depth += 1;
      if (clean[i] === '}') { depth -= 1; if (depth === 0) { out.push(clean.slice(m.index, i + 1)); break; } }
    }
  }
  return out;
}

/** The locations directly inside one server: modifier, path, body. */
function locations(server) {
  const out = [];
  const re = /\n\s*location\s+(?:(=|~\*|~|\^~)\s+)?(\S+)\s*\{/g;
  let m;
  while ((m = re.exec(server))) {
    let depth = 0;
    for (let i = m.index + m[0].length - 1; i < server.length; i += 1) {
      if (server[i] === '{') depth += 1;
      if (server[i] === '}') {
        depth -= 1;
        if (depth === 0) { out.push({ mod: m[1] || '', path: m[2], body: server.slice(m.index + m[0].length, i) }); break; }
      }
    }
  }
  return out;
}

/** Which location nginx picks for a URI. */
function choose(locs, uri) {
  const exact = locs.find((l) => l.mod === '=' && l.path === uri);
  if (exact) return exact;
  const prefixes = locs.filter((l) => (l.mod === '' || l.mod === '^~') && uri.startsWith(l.path))
    .sort((a, b) => b.path.length - a.path.length);
  if (prefixes[0] && prefixes[0].mod === '^~') return prefixes[0];
  const regex = locs.find((l) => (l.mod === '~' || l.mod === '~*')
    && new RegExp(l.path, l.mod === '~*' ? 'i' : '').test(uri));
  return regex || prefixes[0] || null;
}

const t = fakes.checker();
const text = fs.readFileSync(CONF, 'utf8');
const api = servers(text).find((s) => /server_name\s+api\./.test(s));
t.ok(Boolean(api), 'the API server block is found (server_name api.…)');
const locs = locations(api);
const label = (l) => (l ? `location ${l.mod ? `${l.mod} ` : ''}${l.path}` : 'none');

t.section('1. the /places/ block');
const places = locs.filter((l) => l.path === '/places/');
t.ok(places.length === 1 && places[0].mod === '', 'exactly one `location /places/`, a plain prefix');
const body = (places[0] && places[0].body) || '';
t.ok(/limit_req\s+zone=api\s+burst=20\s+nodelay;/.test(body), 'rate-limited in zone `api` (burst 20)');
t.ok(/proxy_pass\s+http:\/\/127\.0\.0\.1:8030;/.test(body), 'sent to maps-shim on 127.0.0.1:8030');
t.ok(/include\s+\/etc\/nginx\/conf\.d\/proxy-common\.inc;/.test(body), 'with the common proxy headers');
const size = (body.match(/client_max_body_size\s+(\d+)([km]?);/) || []);
const bytes = Number(size[1]) * ({ k: 1024, m: 1048576 }[size[2]] || 1);
t.ok(bytes === BODY_LIMIT, `its body limit (${size[1]}${size[2]}) is the shim's BODY_LIMIT (${BODY_LIMIT} bytes)`);
t.ok(/limit_req_zone\s+\$binary_remote_addr\s+zone=api:/.test(text), 'the `api` zone is defined');
t.ok(locs.filter((l) => l.path.startsWith('/places')).length === 1, 'and nothing else under /places');

t.section('2. where each address goes, as nginx chooses');
for (const uri of ['/places/suggest', '/places/mine', '/places/nearby', '/places/anything-else']) {
  const l = choose(locs, uri);
  t.ok(l && l.path === '/places/', `${uri} -> location /places/`, label(l));
}
const labels = choose(locs, '/place/labels/json');
t.ok(labels && labels.mod === '=' && labels.path === '/place/labels/json',
  '/place/labels/json -> still its own exact location', label(labels));
for (const uri of ['/place/autocomplete/json', '/place/details/json', '/place/', '/place/labels/json/x',
  '/geocode/json', '/directions/json', '/places']) {
  const l = choose(locs, uri);
  t.ok(l && l.mod === '' && l.path === '/' && /return\s+404;/.test(l.body),
    `${uri} -> the catch-all 404 (not published)`, label(l));
}

t.section('3. the backend\'s /place/ stays unpublished');
const placeLocs = locs.filter((l) => l.path.startsWith('/place') && l.path !== '/places/');
t.ok(placeLocs.length === 1 && placeLocs[0].mod === '=' && placeLocs[0].path === '/place/labels/json',
  'the only /place… location is `= /place/labels/json`', placeLocs.map(label).join(', '));
t.ok(!locs.some((l) => (l.mod === '~' || l.mod === '~*') && /place|geocode/.test(l.path)),
  'no regex location mentions place or geocode');

t.finish();
