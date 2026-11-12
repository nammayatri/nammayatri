'use strict';
// Preloaded into a maps-shim process (`node -r .../fake-pg-preload.js server.js`)
// by tests/maps-shim-routes.test.js: `require('pg')` returns a scripted Pool
// that answers the place index's queries and RECORDS each one.
//
// Why not PGlite here, as the money-path tests use: the place index is
// geo.search / geo.reverse (pg_trgm, unaccent) and atlas_app.geometry
// (PostGIS), none of which PGlite carries. The routes' own logic -- shaping
// Google's answers from those rows -- is what the split must not change, and
// recording the exact SQL proves the queries did not change either.
//
// The recording goes to the file named by FAKE_PG_LOG, one JSON line a query.
const fs = require('fs');
const Module = require('module');

const LOG = process.env.FAKE_PG_LOG;

const PLACES = {
  'osm:n1': { place_id: 'osm:n1', display_name: 'Marché Capitale', locality: 'Tevragh Zeina', kind: 'poi', lat: 18.0866, lon: -15.9750, name_ar: 'سوق العاصمة', locality_ar: 'تفرغ زينة' },
  'osm:w2': { place_id: 'osm:w2', display_name: 'Rue Didouche Mourad', locality: 'Alger Centre', kind: 'street', lat: 36.7650, lon: 3.0500, name_ar: null, locality_ar: null },
  'osm:n3': { place_id: 'osm:n3', display_name: 'Nouakchott', locality: null, kind: 'place', lat: 18.0790, lon: -15.9650, name_ar: 'نواكشوط', locality_ar: null },
};

// Places proposed by passengers and drivers (movin.suggested_place, day 2,
// 2026-11-09): u5 is approved, with an Arabic name; u6 exists but is not
// approved (pending or refused), so it must answer like an unknown id.
const PROPOSED = {
  u5: { place_id: 'u5', display_name: 'Café Tfeila', locality: 'Tevragh Zeina', kind: 'poi',
        lat: 18.0901, lon: -15.9702, country: 'MR', name_ar: 'مقهى تفيلة', locality_ar: 'تفرغ زينة' },
};

function answer(text, params) {
  const sql = text.replace(/\s+/g, ' ');
  if (/from movin\.suggested_place where status = 'approved'/.test(sql)) {
    if (params[0] === 'pboom') throw new Error('proposals unavailable (test)');
    return params[0] === 'tfeila' ? [{ ...PROPOSED.u5, distance_m: 420.4 }] : [];
  }
  if (/from movin\.suggested_place where place_id = \$1/.test(sql)) {
    return PROPOSED[params[0]] ? [PROPOSED[params[0]]] : [];
  }
  // The labels route since 2026-11-09: rows by id, then the localities' Arabic
  // names in one query (places.js arabicLocalities) -- no join per row.
  if (/from movin\.suggested_place where place_id = any\(\$1\) and status = 'approved'/.test(sql)) {
    return params[0].filter((id) => PROPOSED[id]).map((id) => ({
      place_id: id, name_ar: PROPOSED[id].name_ar, locality: PROPOSED[id].locality,
    }));
  }
  if (/select distinct on \(display_name\) display_name, name_ar from geo\.place/.test(sql)) {
    const known = {};
    for (const p of [...Object.values(PLACES), ...Object.values(PROPOSED)]) {
      if (p.locality && p.locality_ar) known[p.locality] = p.locality_ar;
    }
    return params[0].filter((n) => known[n]).map((n) => ({ display_name: n, name_ar: known[n] }));
  }
  if (/from geo\.search\(/.test(sql)) {
    if (params[0] === 'zzz') return [];
    if (params[0] === 'boom') throw new Error('index unavailable (test)');
    // A full screen from the index, so the cap shows: one proposal + seven.
    if (params[0] === 'tfeila') {
      return Array.from({ length: 8 }, (_, i) => ({ ...PLACES['osm:n1'], place_id: `osm:t${i}`, distance_m: 900 + i }));
    }
    return [
      { ...PLACES['osm:n1'], distance_m: 812.4 },
      { ...PLACES['osm:n3'], distance_m: 1500.6 },
    ];
  }
  if (/from geo\.reverse\(/.test(sql)) {
    return params[0] > 30 ? [PLACES['osm:w2']] : [PLACES['osm:n1']];
  }
  if (/from atlas_app\.geometry/.test(sql)) {
    // The point's country: west of 0 is Mauritania here, east is Algeria;
    // the middle of the Atlantic is neither.
    const lon = params[1];
    if (lon < -20) return [];
    return [{ region: lon < 0 ? 'Mauritania' : 'Algeria' }];
  }
  if (/from geo\.place where place_id = any\(\$1\) and name_ar is not null/.test(sql)) {
    return params[0].filter((id) => PLACES[id] && PLACES[id].name_ar).map((id) => ({
      place_id: id, name_ar: PLACES[id].name_ar, locality: PLACES[id].locality,
    }));
  }
  if (/from geo\.place where place_id = \$1/.test(sql)) {
    return PLACES[params[0]] ? [PLACES[params[0]]] : [];
  }
  return [];
}

class Pool {
  constructor() { this.handlers = {}; }
  on() { return this; }
  async query(text, params = []) {
    const sql = String(text).replace(/\s+/g, ' ').trim();
    // Only the place index is recorded: the wallet sweep and the dispatch
    // list also query on timers, at moments no test can pin down.
    if (LOG && /geo\.|atlas_app\.geometry|movin\.suggested_place/.test(sql)) {
      fs.appendFileSync(LOG, JSON.stringify({ sql, params }) + '\n');
    }
    return { rows: answer(sql, params), rowCount: 0 };
  }
  async connect() {
    return { query: (t, p) => this.query(t, p), release() {} };
  }
}

const load = Module._load;
Module._load = function (request, parent, isMain) {
  if (request === 'pg') return { Pool };
  return load.call(this, request, parent, isMain);
};
