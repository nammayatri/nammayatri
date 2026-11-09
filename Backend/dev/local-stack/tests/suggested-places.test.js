'use strict';
// The proposed places' table and the type mapping (stack/db/suggested-places.sql),
// against a real Postgres (PGlite), applied the way a release applies it:
// once by lib/db.js, then again here.
//
//   cd Backend/dev/local-stack/tests && npm ci && node suggested-places.test.js
//
//   1. the file     -- applies twice; no PostGIS needed
//   2. the types    -- the CHECK, the mapping and the website's list agree
//   3. the mapping  -- OpenStreetMap tags to our types, one line per decision
//   4. the rules    -- every CHECK refuses what it is there to refuse
//
// PGlite has no PostGIS, so the GiST index is NOT built here (section 1 says
// so). The CI's ride regression builds it on postgis/postgis:15-3.4 through
// setup.sh's seed_suggested_places.
const fs = require('fs');
const os = require('os');
const path = require('path');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

const FILE = path.join(__dirname, '..', 'stack', 'db', 'suggested-places.sql');

// The website's packages/domain/src/place-types.ts, PLACE_TYPES, in its order.
// A copy, because CI has no website checkout; section 2 compares it with the
// real file whenever one is on this machine.
const PLACE_TYPES = [
  'cafe', 'restaurant', 'pharmacy', 'hospital', 'mosque', 'school', 'shop',
  'market', 'bank', 'hotel', 'fuel', 'administration', 'other',
];

const same = (a, b) => JSON.stringify([...a].sort()) === JSON.stringify([...b].sort());

(async () => {
  const t = fakes.checker();
  const db = await open();
  const sqlText = fs.readFileSync(FILE, 'utf8');

  /* ── 1. the file ────────────────────────────────────────────────────── */
  t.section('1. the file applies twice, with no PostGIS');
  let again = null;
  try { await db.exec(sqlText); } catch (e) { again = e.message; }
  t.ok(again === null, 'a second application changes nothing and raises nothing', again || '');
  const idx = await db.sql(
    `SELECT indexname FROM pg_indexes WHERE schemaname = 'movin' AND tablename = 'suggested_place' ORDER BY 1`);
  const names = idx.map((r) => r.indexname);
  t.ok(names.includes('suggested_place_queue') && names.includes('suggested_place_by_author'),
    'the console queue and per-author indexes exist', names.join(', '));
  t.ok(!names.includes('suggested_place_position'),
    'no PostGIS here, so no position index -- and no error for it');

  /* ── 2. the types ───────────────────────────────────────────────────── */
  t.section('2. one list of types: the CHECK, the mapping, the website');
  const checkWords = async (name) => {
    const r = await db.sql(
      `SELECT pg_get_constraintdef(oid) AS def FROM pg_constraint WHERE conname = $1`, [name]);
    return r.length ? [...r[0].def.matchAll(/'([a-z_]+)'/g)].map((m) => m[1]) : [];
  };
  const typeCheck = await checkWords('suggested_place_type_known');
  const proposedCheck = await checkWords('suggested_place_proposed_type_known');
  t.ok(same(typeCheck, PLACE_TYPES), 'the CHECK on `type` is exactly the thirteen keys', typeCheck.join(','));
  t.ok(same(proposedCheck, PLACE_TYPES), 'the CHECK on `proposed_type` is the same list', proposedCheck.join(','));

  const block = sqlText.split('-- >>> place_type_of')[1]?.split('-- <<< place_type_of')[0] || '';
  const returned = new Set([...block.matchAll(/(?:THEN|ELSE)\s+'([a-z_]+)'/g)].map((m) => m[1]));
  t.ok(block.includes('CREATE OR REPLACE FUNCTION movin.place_type_of'),
    'the function sits between its two markers (the probe copies that block)');
  t.ok([...returned].every((k) => PLACE_TYPES.includes(k)),
    'every value place_type_of can return is accepted by the CHECK', [...returned].join(','));
  t.ok(same(returned, PLACE_TYPES), 'and every type is reachable from some OpenStreetMap tag');

  // The source itself, when the website is checked out beside us. CI has no
  // website, so it skips: the copy above is then what is compared.
  const site = [process.env.MOVA_WEBSITE, path.join(os.homedir(), 'movin-website-release')]
    .filter(Boolean)
    .map((d) => path.join(d, 'packages', 'domain', 'src', 'place-types.ts'))
    .find((f) => fs.existsSync(f));
  if (site) {
    const src = fs.readFileSync(site, 'utf8');
    const list = (src.match(/PLACE_TYPES = \[([\s\S]*?)\] as const/) || [])[1] || '';
    const keys = [...list.matchAll(/'([a-z_]+)'/g)].map((m) => m[1]);
    t.ok(JSON.stringify(keys) === JSON.stringify(PLACE_TYPES),
      `the website's place-types.ts holds the same keys in the same order (${site})`, keys.join(','));
  } else {
    console.log('  --    the website checkout is not on this machine: compared with the copy only');
  }

  /* ── 3. the mapping ─────────────────────────────────────────────────── */
  t.section('3. OpenStreetMap tag -> our type');
  const cases = [
    ['amenity', 'cafe', 'cafe'],
    ['amenity', 'restaurant', 'restaurant'],
    ['amenity', 'fast_food', 'restaurant'],
    ['amenity', 'pharmacy', 'pharmacy'],
    ['healthcare', 'pharmacy', 'pharmacy'],
    ['shop', 'chemist', 'shop'], // a drugstore, not a pharmacy
    ['amenity', 'hospital', 'hospital'],
    ['amenity', 'clinic', 'hospital'],
    ['healthcare', 'doctor', 'hospital'],
    ['building', 'hospital', 'hospital'],
    ['amenity', 'place_of_worship', 'mosque'],
    ['building', 'mosque', 'mosque'],
    ['amenity', 'university', 'school'],
    ['building', 'school', 'school'],
    ['shop', 'supermarket', 'shop'],
    ['shop', 'mall', 'shop'],
    ['amenity', 'marketplace', 'market'],
    ['amenity', 'bank', 'bank'],
    ['amenity', 'atm', 'other'], // nobody asks a driver for an ATM
    ['tourism', 'guest_house', 'hotel'],
    ['amenity', 'fuel', 'fuel'],
    ['amenity', 'townhall', 'administration'],
    ['amenity', 'police', 'administration'],
    ['office', 'government', 'administration'],
    ['office', 'company', 'other'],
    ['building', 'yes', 'other'],
    ['highway', 'primary', 'other'], // a street
    ['place', 'suburb', 'other'], // a district
    ['public_transport', 'station', 'other'],
    [null, null, 'other'],
  ];
  for (const [cls, sub, want] of cases) {
    const got = (await db.sql('SELECT movin.place_type_of($1, $2) AS t', [cls, sub]))[0].t;
    t.ok(got === want, `${cls}=${sub} -> ${want}`, `got ${got}`);
  }

  /* ── 4. the rules ───────────────────────────────────────────────────── */
  t.section('4. what the table refuses');
  // A valid Nouakchott proposal; each case below breaks one thing.
  const base = {
    proposed_name: 'Pharmacie Tfeila', proposed_type: 'pharmacy',
    name: 'Pharmacie Tfeila', type: 'pharmacy',
    lat: 18.0866, lon: -15.975, country: 'MR', author_side: 'rider', author_id: 'rider-1',
  };
  const insert = async (over = {}) => {
    const row = { ...base, ...over };
    const cols = Object.keys(row);
    return (await db.sql(
      `INSERT INTO movin.suggested_place (${cols.join(', ')})
       VALUES (${cols.map((_, i) => `$${i + 1}`).join(', ')}) RETURNING id, place_id, status`,
      Object.values(row)))[0];
  };
  const refused = async (over, pattern, label) => {
    let err = null;
    try { await insert(over); } catch (e) { err = e.message; }
    t.ok(err !== null && pattern.test(err), label, err || 'it was accepted');
  };

  const first = await insert();
  t.ok(first.status === 'pending', 'a new proposal is pending');
  t.ok(first.place_id === `u${first.id}`, `its place_id is u<id> (${first.place_id}), never an OSM n…/w…`);
  const dz = await insert({ lat: 36.7538, lon: 3.0588, country: 'DZ', author_side: 'driver', author_id: 'drv-1',
    proposed_name: 'Café du Port', proposed_type: 'cafe', name: 'Café du Port', type: 'cafe' });
  t.ok(dz.place_id === `u${dz.id}` && dz.id !== first.id, 'an Algiers proposal by a driver, with its own id');
  let dup = null;
  try { await db.sql(`UPDATE movin.suggested_place SET place_id = 'u1'`); } catch (e) { dup = e.message; }
  t.ok(dup !== null, 'place_id cannot be written by hand: it is derived from the id', dup || 'it was accepted');

  await refused({ type: 'pharmacie' }, /suggested_place_type_known/, 'a type the list does not have (the French word)');
  await refused({ proposed_type: 'atm' }, /suggested_place_proposed_type_known/, 'a proposed type the list does not have');
  await refused({ proposed_name: 'X', name: 'X' }, /name_shape/, 'a one-character name');
  await refused({ proposed_name: 'x'.repeat(81), name: 'x'.repeat(81) }, /name_shape/, 'an 81-character name');
  await refused({ proposed_name: ' Café ', name: ' Café ' }, /name_shape/, 'an untrimmed name');
  await refused({ name: 'Café\nTfeila' }, /suggested_place_name_shape/, 'a newline in the shown name');
  await refused({ name_ar: 'ص' }, /name_ar_shape/, 'a one-letter Arabic name');
  await refused({ lat: -15.975, lon: 18.0866 }, /in_our_box/, 'Nouakchott with latitude and longitude swapped');
  await refused({ lat: 3.0588, lon: 36.7538, country: 'DZ' }, /in_our_box/, 'Algiers swapped');
  await refused({ lat: 48.85, lon: 2.35 }, /in_our_box/, 'Paris');
  await refused({ country: 'FR' }, /country_check/, 'a country we do not serve');
  await refused({ author_side: 'admin' }, /author_side_check/, 'an author who is neither a rider nor a driver');
  await refused({ author_id: null }, /null value/, 'no author');
  await refused({ status: 'refused', decided_at: new Date(), decided_by_email: 'a@mova.test' },
    /refusal_has_reason/, 'a refusal with no reason');
  await refused({ status: 'approved', refusal_reason: 'duplicate', decided_at: new Date(), decided_by_email: 'a@mova.test' },
    /refusal_has_reason/, 'an approval that still carries a refusal reason');
  await refused({ status: 'approved' }, /decision_named/, 'an approval that names nobody');
  await refused({ status: 'deleted', decided_at: new Date(), decided_by_email: 'a@mova.test' },
    /status_check/, 'a status the life cycle does not have');
  await refused({ refusal_reason: 'x'.repeat(41), status: 'refused', decided_at: new Date(), decided_by_email: 'a@mova.test' },
    /refusal_reason_check/, 'a refusal reason that is a sentence, not a code');

  const ok = await insert({ status: 'approved', decided_at: new Date(), decided_by_email: 'agent@mova.test',
    name_ar: 'صيدلية تفيلة' });
  t.ok(ok.status === 'approved', 'an approval with a name, a date and an Arabic name is accepted');
  const back = await db.sql(
    `UPDATE movin.suggested_place SET status = 'refused', refusal_reason = 'duplicate',
            decided_at = now(), decided_by_email = 'agent@mova.test'
      WHERE id = $1 RETURNING status, refusal_reason`, [ok.id]);
  t.ok(back[0].status === 'refused', 'an approved place found wrong later is refused, not deleted');

  await db.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(1); });
