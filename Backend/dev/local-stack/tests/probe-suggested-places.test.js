'use strict';
// investigations/probe-suggested-places.sql -- the live probe of day 2 -- is
// what investigations/probe-suggested-places.js generates from the shim's
// own code, writes nothing outside pg_temp, and its parts that need no
// PostGIS really run.
//
//   node tests/probe-suggested-places.test.js
//
//   1. no drift      -- regenerated from the shim, it is the committed file
//   2. read-only     -- every write targets pg_temp (this session's schema)
//   3. what it asks  -- place_type_of WITH the name; the 15 km fallback
//   4. it runs       -- sections 5-8 (Arabic localities; search, details,
//                       labels on the copy),
//                       in PGlite with a stand-in for geo.normalise and
//                       geo.place: proves their PREPARE, their parameters'
//                       types, and the answers the probe's titles promise.
//                       Sections 0-4 need PostGIS: only the live run can
//                       show those. (Section 5 was not run here at first,
//                       and the first live run stopped on it: EXECUTE
//                       refuses a subquery as a parameter.)
const fs = require('fs');
const path = require('path');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

const { build, OUT } = require(path.join(__dirname, '..', 'investigations', 'probe-suggested-places.js'));

(async () => {
  const t = fakes.checker();
  const quiet = console.log;
  console.log = () => {}; // places.js logs each captured call
  const fresh = await build();
  console.log = quiet;
  const committed = fs.readFileSync(OUT, 'utf8');

  t.section('1. no drift');
  t.ok(fresh === committed, 'the committed probe is exactly what the shim\'s code generates today',
    'run: node investigations/probe-suggested-places.js');

  t.section('2. read-only for every real table');
  const statements = committed.split('\n').filter((l) => !/^\s*(--|\\)/.test(l)).join('\n')
    .split(/;\s*\n/).map((s) => s.replace(/\s+/g, ' ').trim()).filter(Boolean);
  const writes = statements.filter((s) => /^(INSERT|UPDATE|DELETE|ALTER|CREATE|DROP|TRUNCATE|GRANT|COMMENT)\b/i.test(s));
  const outside = writes.filter((s) => !/^CREATE TEMP TABLE suggested_place \(LIKE movin\.suggested_place INCLUDING ALL\)$/.test(s)
    && !/^(INSERT INTO|ALTER TABLE) pg_temp\./.test(s));
  t.ok(writes.length === 3 && outside.length === 0,
    `${writes.length} writing statements, all to pg_temp (the copy, its column, its four rows)`, outside.join(' | '));
  const realRefs = (committed.match(/movin\.suggested_place\b/g) || []).length;
  t.ok(realRefs === 1, 'movin.suggested_place is named once: as the model of the copy (LIKE)');
  t.ok(!/\bBEGIN\b|\bCOMMIT\b/.test(statements.join(' ')), 'no transaction to leave open, nothing to commit');

  t.section('3. what it asks');
  t.ok(committed.includes('movin.place_type_of(p.class, p.subclass, p.display_name) = $1'),
    'search by type calls place_type_of with the name (three arguments)');
  t.ok(/osm_of_type\('market', 18\.0858, -15\.9785, 5000, 20\)/.test(committed)
    && /osm_of_type\('market', 18\.0858, -15\.9785, 15000, 20\)/.test(committed),
    'the 5 km search and the 15 km one are both measured');
  t.ok(/churches_as_mosques/.test(committed), 'and it counts churches left under « mosque » near Algiers');

  t.section('4. sections 5-8 run (PGlite, stand-ins for the index)');
  const db = await open();
  // The two things from `geo` these statements read: the names table, cut
  // down, and the normaliser AS THE LIVE ONE IS (geocoder/arabic-search.sql),
  // minus unaccent, which PGlite lacks -- above all it does NOT trim. The first
  // stand-in trimmed, and so passed a guard the live index then failed: « %% »
  // matched every proposal on the live run of 2026-11-09.
  await db.exec(`
    CREATE SCHEMA IF NOT EXISTS geo;
    CREATE FUNCTION geo.normalise(t text) RETURNS text LANGUAGE sql IMMUTABLE STRICT AS
      $$ SELECT regexp_replace(
                  translate(regexp_replace(lower(t), '[ًٌٍَُِّْـ]', '', 'g'), 'أإآٱىةؤئ', 'اااايهوي'),
                  '[^a-z0-9ء-ي]+', ' ', 'g') $$;
    CREATE TABLE geo.place (display_name text, name_ar text, importance real);
    INSERT INTO geo.place VALUES ('Tevragh Zeina', 'تفرغ زينة', 0.8), ('Hydra', 'حيدرة', 0.8);
    CREATE TEMP TABLE suggested_place (LIKE movin.suggested_place INCLUDING ALL);
    INSERT INTO pg_temp.suggested_place
      (proposed_name, proposed_type, name, type, name_ar, lat, lon, country, locality,
       author_side, author_id, status, decided_at, decided_by_email) VALUES
      ('Café Sonde Tfeila', 'cafe', 'Café Sonde Tfeila', 'cafe', 'مقهى المسبار', 18.0901, -15.9702, 'MR', 'Tevragh Zeina', 'rider', 'probe', 'approved', now(), 'probe@mova'),
      ('Marché Sonde Ksar', 'market', 'Marché Sonde Ksar', 'market', NULL, 18.1105, -15.9512, 'MR', 'Ksar', 'rider', 'probe', 'approved', now(), 'probe@mova'),
      ('Pharmacie Sonde Hydra', 'pharmacy', 'Pharmacie Sonde Hydra', 'pharmacy', 'صيدلية المسبار', 36.7425, 3.0434, 'DZ', 'Hydra', 'rider', 'probe', 'approved', now(), 'probe@mova'),
      ('Café Sonde En Attente', 'cafe', 'Café Sonde En Attente', 'cafe', NULL, 18.0870, -15.9760, 'MR', 'Tevragh Zeina', 'rider', 'probe', 'pending', NULL, NULL);
  `);
  const block = (name) => {
    const m = committed.match(new RegExp(`PREPARE ${name} AS\\n([\\s\\S]*?);\\n`));
    return m ? `PREPARE ${name} AS\n${m[1]}` : null;
  };
  const executes = (name) => [...committed.matchAll(new RegExp(`EXECUTE ${name}\\((.*)\\);`, 'g'))].map((m) => `EXECUTE ${name}(${m[1]})`);
  const rowsOf = async (sql) => (await db.exec(sql)).at(-1).rows;

  for (const name of ['localities_ar', 'approved_matching', 'approved_place', 'labels']) {
    let err = null;
    try { await db.exec(block(name)); } catch (e) { err = e.message; }
    t.ok(block(name) && err === null, `PREPARE ${name} is accepted by Postgres`, err || 'not found in the probe');
  }
  const ar = (await rowsOf(executes('localities_ar')[0])).map((r) => `${r.display_name}:${r.name_ar}`).sort();
  t.ok(executes('localities_ar').length === 1 && JSON.stringify(ar) === JSON.stringify(['Hydra:حيدرة']),
    'the localities in Arabic: one EXECUTE, a literal list, the names the index holds', ar.join(' '));
  const [tfeila, arabic, attente, percent, punct] = executes('approved_matching');
  const names = async (sql) => (await rowsOf(sql)).map((r) => r.display_name);
  t.ok(JSON.stringify(await names(tfeila)) === JSON.stringify(['Café Sonde Tfeila']), '« tfeila » finds the approved café');
  t.ok(JSON.stringify(await names(arabic)) === JSON.stringify(['Café Sonde Tfeila', 'Pharmacie Sonde Hydra']),
    '« المسبار » finds both by their Arabic names, the near one first');
  t.ok((await names(attente)).length === 0, '« attente »: the pending café is never shown');
  t.ok((await names(percent)).length === 0, '« %% »: nothing (normalised to a lone space, trimmed to empty)',
    JSON.stringify(await names(percent)));
  t.ok(JSON.stringify(await names(punct)) === JSON.stringify(['Café Sonde Tfeila']),
    '« Tfeila! »: the café -- trailing punctuation does not leave a space to match');
  const [u1, u4] = executes('approved_place');
  t.ok((await rowsOf(u1)).length === 1 && (await rowsOf(u4)).length === 0, 'details: u1 (approved) answers, u4 (pending) does not');
  const labels = (await rowsOf(executes('labels')[0])).map((r) => `${r.place_id}:${r.locality_ar || ''}`).sort();
  t.ok(JSON.stringify(labels) === JSON.stringify(['u1:تفرغ زينة', 'u3:حيدرة']),
    'labels: u1 and u3 with their localities in Arabic; not u2 (no Arabic name), not u4 (pending)', labels.join(' '));

  await db.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(1); });
