'use strict';
// Reading stack/edge/nginx.conf the way nginx reads it, for the edge tests
// (edge-places, edge-admin-tiles). No nginx on the laptop and no CI: this is
// not `nginx -t` -- the release runs that on the box and refuses a file nginx
// refuses. It answers what `nginx -t` cannot: WHERE an address goes, and what
// the location it lands in says.
const fs = require('fs');
const path = require('path');

const CONF = path.join(__dirname, '..', '..', 'stack', 'edge', 'nginx.conf');
const COMMON = path.join(__dirname, '..', '..', 'stack', 'edge', 'proxy-common.inc');

/** The text with every comment removed. */
const uncomment = (text) => text.split('\n').map((l) => l.replace(/(^|\s)#.*$/, '')).join('\n');

/** The text of each top-level `server { … }`, comments removed. */
function servers(text) {
  const clean = uncomment(text);
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

/** The server whose server_name starts with `name` (e.g. 'api.', 'admin.'). */
function server(text, name) {
  const re = new RegExp(`server_name\\s+${name.replace(/\./g, '\\.')}`);
  return servers(text).find((s) => re.test(s)) || null;
}

/** The locations directly inside one server: modifier, path, body. */
function locations(srv) {
  const out = [];
  const re = /\n\s*location\s+(?:(=|~\*|~|\^~)\s+)?(\S+)\s*\{/g;
  let m;
  while ((m = re.exec(srv))) {
    let depth = 0;
    for (let i = m.index + m[0].length - 1; i < srv.length; i += 1) {
      if (srv[i] === '{') depth += 1;
      if (srv[i] === '}') {
        depth -= 1;
        if (depth === 0) { out.push({ mod: m[1] || '', path: m[2], body: srv.slice(m.index + m[0].length, i) }); break; }
      }
    }
  }
  return out;
}

/** The server's own directives: its text with every location removed. */
function serverLevel(srv) {
  let out = srv;
  for (const l of locations(srv)) out = out.replace(l.body, '');
  return out;
}

/**
 * Which location nginx picks for a URI: an exact `=` match first; else the
 * longest prefix, final if it is `^~`; else the first regex that matches, in
 * file order; else that longest prefix.
 */
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

const label = (l) => (l ? `location ${l.mod ? `${l.mod} ` : ''}${l.path}` : 'none');

module.exports = {
  CONF, COMMON, read: () => fs.readFileSync(CONF, 'utf8'), readCommon: () => fs.readFileSync(COMMON, 'utf8'),
  uncomment, servers, server, locations, serverLevel, choose, label,
};
