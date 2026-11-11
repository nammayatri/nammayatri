'use strict';
// The map pictures on the console's own address (stack/edge/nginx.conf, the
// admin.movinapp.net server), day 3 of « Lieux proposés », 2026-11-11.
//
//   node tests/edge-admin-tiles.test.js
//
// The console shows a proposed place on a 3×3 grid of map pictures. Its
// security policy allows images from its own address only, so the pictures are
// served under admin.movinapp.net/tiles/styles/basic-preview/… by the same
// tile server the API host publishes.
//
// What this holds, because nothing else would notice:
//   1. the block: the tiles zone, the tile server, the common proxy headers,
//      caching by `expires` with the server's own Cache-Control hidden;
//   2. NO add_header in it, nor in the file it includes. One add_header in a
//      location silently drops every add_header of the server block (HSTS,
//      nosniff, the referrer policy, X-Frame-Options) from its answers --
//      nginx -t passes and nothing looks wrong;
//   3. where each address goes, as nginx chooses (tests/lib/nginx.js): only
//      that style's pictures reach the tile server; the console, its API and
//      its files are where they were;
//   4. the policy still allows the pictures (img-src 'self'), and the API
//      host's own /tiles/ is untouched.
const fakes = require('./lib/fakes');
const ngx = require('./lib/nginx');

const PREFIX = '/tiles/styles/basic-preview/';
const UPSTREAM = 'http://127.0.0.1:8035/styles/basic-preview/';

const t = fakes.checker();
const text = ngx.read();
const admin = ngx.server(text, 'admin.');
t.ok(Boolean(admin), 'the console\'s server block is found (server_name admin.…)');
const locs = ngx.locations(admin);

t.section('1. the block');
const tiles = locs.filter((l) => l.path.startsWith('/tiles'));
t.ok(tiles.length === 1 && tiles[0].mod === '^~' && tiles[0].path === PREFIX,
  `exactly one /tiles… location on the console: \`^~ ${PREFIX}\``, tiles.map(ngx.label).join(', '));
const body = (tiles[0] && tiles[0].body) || '';
t.ok(/limit_req\s+zone=tiles\s+burst=\d+\s+nodelay;/.test(body), 'rate-limited in zone `tiles`');
t.ok(/limit_req_zone\s+\$binary_remote_addr\s+zone=tiles:/.test(text), 'the `tiles` zone is defined');
t.ok(new RegExp(`proxy_pass\\s+${UPSTREAM.replace(/[./]/g, '\\$&')};`).test(body),
  `sent to the tile server, that style only (${UPSTREAM})`);
t.ok(/include\s+\/etc\/nginx\/conf\.d\/proxy-common\.inc;/.test(body), 'with the common proxy headers');
t.ok(/proxy_hide_header\s+Cache-Control;/.test(body) && /expires\s+\d+d;/.test(body),
  'cached by `expires`, the tile server\'s own Cache-Control hidden (one header, not two)');
t.ok(/proxy_hide_header\s+Access-Control-Allow-Origin;/.test(body),
  'the tile server\'s CORS header hidden: same address, nobody else needs to read these');

t.section('2. no add_header, which would drop the server\'s security headers');
t.ok(!/\badd_header\b/.test(body), 'no add_header in the block');
t.ok(!/\badd_header\b/.test(ngx.uncomment(ngx.readCommon())), 'nor in proxy-common.inc, which it includes');
const level = ngx.serverLevel(admin);
for (const [name, re] of [
  ['HSTS', /add_header\s+Strict-Transport-Security\s/],
  ['nosniff', /add_header\s+X-Content-Type-Options\s+"nosniff"/],
  ['the referrer policy', /add_header\s+Referrer-Policy\s/],
  ['X-Frame-Options', /add_header\s+X-Frame-Options\s+"DENY"/],
  ['the console\'s policy', /add_header\s+Content-Security-Policy\s+\$console_csp/],
]) {
  t.ok(re.test(level), `the server block still sets ${name} -- inherited by the pictures`);
}

t.section('3. where each address goes, as nginx chooses');
const picture = `${PREFIX}16/33324/25564.png`; // central Algiers
let l = ngx.choose(locs, picture);
t.ok(l && l.path === PREFIX, `a picture (${picture}) -> the tile block`, ngx.label(l));
const rest = picture.slice(PREFIX.length);
t.ok(`${UPSTREAM}${rest}`.endsWith('/styles/basic-preview/16/33324/25564.png'),
  'and reaches the tile server as /styles/basic-preview/16/33324/25564.png (the prefix replaced)');
for (const uri of ['/tiles/', '/tiles/data/v3/14/8000/7000.pbf', '/tiles/styles/movin-ar/16/1/1.png',
  '/tiles/index.json', '/tiles/styles/basic-preview.json']) {
  l = ngx.choose(locs, uri);
  t.ok(l && l.mod === '' && l.path === '/', `${uri} -> not the tile server (the console's own page)`, ngx.label(l));
}
for (const [uri, want] of [
  ['/admin/places', '/admin/'],
  ['/admin/auth/login', '/admin/auth/login'],
  ['/assets/index-abc123.js', '/assets/'],
  ['/lieux-proposes', '/'],
]) {
  l = ngx.choose(locs, uri);
  t.ok(l && l.path === want, `${uri} -> still ${want}`, ngx.label(l));
}
t.ok(!locs.some((x) => x.mod === '~' || x.mod === '~*'), 'no regex location on the console that could take a path over');

t.section('4. the policy allows them; the API host is untouched');
const csp = (text.match(/map\s+\$sent_http_content_type\s+\$console_csp\s*\{[^}]*~\*\^text\/html\s+"([^"]+)"/) || [])[1] || '';
t.ok(/img-src 'self'/.test(csp), 'the console\'s policy allows images from its own address (img-src \'self\')', csp);
t.ok(/connect-src 'self';/.test(csp), 'and is otherwise unchanged: calls to its own address only');
const api = ngx.server(text, 'api.');
const apiTiles = ngx.locations(api).filter((x) => x.path.startsWith('/tiles'));
t.ok(apiTiles.length === 1 && apiTiles[0].mod === '' && apiTiles[0].path === '/tiles/'
  && /proxy_pass\s+http:\/\/127\.0\.0\.1:8035\/;/.test(apiTiles[0].body),
  'the API host still publishes /tiles/ as before (the phones\' map)');

t.finish();
