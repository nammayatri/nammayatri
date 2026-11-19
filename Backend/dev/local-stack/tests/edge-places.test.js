'use strict';
// The edge's /places/ block (stack/edge/nginx.conf), day 2 of « Lieux
// proposés », 2026-11-09 -- and the /place/ paths it must NOT publish.
//
//   node tests/edge-places.test.js
//
// No nginx on the laptop and no CI, so this reads the file and chooses a
// location for each address the way nginx does (tests/lib/nginx.js). It is
// not `nginx -t`: the release runs that on the box before it reloads. This
// checks what `nginx -t` cannot: WHERE an address goes.
//
// Why it matters: a shim route without an edge block answers correctly on
// 8030 and 404 from every phone (docs/gotchas.md -- the wallet shipped that
// way); and a /place/ prefix published by mistake would hand the backend's
// whole geocoder to the internet.
const path = require('path');
const fakes = require('./lib/fakes');
const { read, servers, locations, choose, label: labelOf } = require('./lib/nginx');

const { BODY_LIMIT, PHOTO_MAX_BYTES } = require(path.join(__dirname, '..', 'stack', 'maps-shim', 'suggested-places.js'));

const t = fakes.checker();
const text = read();
const api = servers(text).find((s) => /server_name\s+api\./.test(s));
t.ok(Boolean(api), 'the API server block is found (server_name api.…)');
const locs = locations(api);
const label = labelOf;

t.section('1. the /places/ block');
const places = locs.filter((l) => l.path === '/places/');
t.ok(places.length === 1 && places[0].mod === '', 'exactly one `location /places/`, a plain prefix');
const body = (places[0] && places[0].body) || '';
t.ok(/limit_req\s+zone=api\s+burst=20\s+nodelay;/.test(body), 'rate-limited in zone `api` (burst 20)');
t.ok(/proxy_pass\s+http:\/\/127\.0\.0\.1:8030;/.test(body), 'sent to maps-shim on 127.0.0.1:8030');
t.ok(/include\s+\/etc\/nginx\/conf\.d\/proxy-common\.inc;/.test(body), 'with the common proxy headers');
const sizeOf = (b) => {
  const m = (b.match(/client_max_body_size\s+(\d+)([km]?);/) || []);
  return { text: `${m[1]}${m[2]}`, bytes: Number(m[1]) * ({ k: 1024, m: 1048576 }[m[2]] || 1) };
};
const size = sizeOf(body);
t.ok(size.bytes === BODY_LIMIT, `its body limit (${size.text}) is the shim's BODY_LIMIT (${BODY_LIMIT} bytes)`);
t.ok(/limit_req_zone\s+\$binary_remote_addr\s+zone=api:/.test(text), 'the `api` zone is defined');

t.section('1b. the photos block (day 4): the same, with room for one image');
const photosLoc = locs.filter((l) => l.path === '/places/photos/');
t.ok(photosLoc.length === 1 && photosLoc[0].mod === '', 'exactly one `location /places/photos/`, a plain prefix');
const pbody = (photosLoc[0] && photosLoc[0].body) || '';
t.ok(/limit_req\s+zone=api\s+burst=20\s+nodelay;/.test(pbody), 'rate-limited in zone `api` (burst 20)');
t.ok(/proxy_pass\s+http:\/\/127\.0\.0\.1:8030;/.test(pbody), 'sent to maps-shim on 127.0.0.1:8030');
t.ok(/include\s+\/etc\/nginx\/conf\.d\/proxy-common\.inc;/.test(pbody), 'with the common proxy headers');
const psize = sizeOf(pbody);
t.ok(psize.bytes === PHOTO_MAX_BYTES, `its body limit (${psize.text}) is the shim's PHOTO_MAX_BYTES (${PHOTO_MAX_BYTES} bytes)`);
t.ok(!/add_header|return\s/.test(pbody), 'no header or answer of its own: the shim answers');
const underPlaces = locs.filter((l) => l.path.startsWith('/places')).map((l) => `${l.mod}${l.path}`).sort();
t.ok(JSON.stringify(underPlaces) === JSON.stringify(['/places/', '/places/photos/']),
  'and nothing else under /places: those two', underPlaces.join(', '));

t.section('2. where each address goes, as nginx chooses');
for (const uri of ['/places/suggest', '/places/mine', '/places/nearby', '/places/anything-else',
  '/places/photos', '/places/photosu12/1', '/places/u12/photo/1']) {
  const l = choose(locs, uri);
  t.ok(l && l.path === '/places/', `${uri} -> location /places/ (4k)`, label(l));
}
for (const uri of ['/places/photos/u12/1', '/places/photos/u12/2', '/places/photos/anything']) {
  const l = choose(locs, uri);
  t.ok(l && l.path === '/places/photos/', `${uri} -> location /places/photos/ (512k)`, label(l));
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
const placeLocs = locs.filter((l) => l.path.startsWith('/place') && !l.path.startsWith('/places/'));
t.ok(placeLocs.length === 1 && placeLocs[0].mod === '=' && placeLocs[0].path === '/place/labels/json',
  'the only /place… location is `= /place/labels/json`', placeLocs.map(label).join(', '));
t.ok(!locs.some((l) => (l.mod === '~' || l.mod === '~*') && /place|geocode/.test(l.path)),
  'no regex location mentions place or geocode');

t.finish();
